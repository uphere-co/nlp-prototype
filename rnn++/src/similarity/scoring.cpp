#include "similarity/scoring.h"

#include <fmt/printf.h>

#include "wordrep/indexes.h"

#include "data_source/db.h"

#include "utils/profiling.h"

using wordrep::Sentence;
using wordrep::CharOffset;
using wordrep::ChunkIndex;
using data::PerSentQueryResult;
using data::DBIndexer;
using data::ScoreWithOffset;

namespace {

auto get_clip_offset = [](Sentence sent, engine::DepSearchScore const &score, auto const &tokens,
                          auto max_clip_len)->std::pair<CharOffset,CharOffset> {
    auto scores = score.scores_with_idx();
    if(!scores.size()) return {{},{}};
    std::sort(scores.begin(), scores.end(), [](auto x, auto y){return x.second>y.second;});
    auto pair = scores.front();
    auto idx  = pair.first;
    auto i_word_beg = idx.front();
    auto i_word_end = idx.back();
    CharOffset clip_beg = tokens.word_beg(i_word_beg);
    CharOffset clip_end = tokens.word_end(i_word_end);
    auto len_sent = sent.chrlen();
    max_clip_len = max_clip_len>len_sent? len_sent:max_clip_len;
    auto max_len = typename decltype(clip_beg)::val_t{max_clip_len};

    for(auto pair : scores){
        auto idx = pair.first;
        //auto score = pair.second;
        auto beg = tokens.word_beg(idx.front());
        auto end = tokens.word_end(idx.back());
        if(beg<clip_beg && clip_end < beg+max_len ) {
            clip_beg = beg;
            i_word_beg = idx.front();
        } else if(end>clip_end && end < clip_beg+max_len ) {
            clip_end = end;
            i_word_end = idx.back();
        }
//        fmt::print("{} {} {} {}\n", idx.val, score, tokens.word_beg(idx).val, tokens.word_end(idx).val);
    }
    auto len = clip_end.val-clip_beg.val;
    while(max_len-len>0) {
        if(i_word_beg>sent.front()) {
            --i_word_beg;
            clip_beg = tokens.word_beg(i_word_beg);
        }
        if(i_word_end<sent.back()){
            ++i_word_end;
            clip_end = tokens.word_end(i_word_end);
        }
        len = clip_end.val-clip_beg.val;
        if(i_word_beg==sent.front() && i_word_end==sent.back()) break;
    }

//    fmt::print("{} {}\n", clip_beg.val, clip_end.val);
    return {clip_beg, clip_end};
};

}//nameless namespace

namespace engine{

ScoredSentence output(wordrep::Scoring::ScoredSentence const& sent){
    DepSearchScore score{0};
    for(auto e : sent.entities) {
        if(!e.second) continue;
        auto& query = e.first;
        auto& matched = e.second.value();
        score.insert(query.idxs, matched);
    }
    for(auto e : sent.words){
        if(!e.second) continue;
        auto& query = e.first;
        auto& matched = e.second.value();
        score.insert({query.idx}, matched);
    }
    return {sent.orig, score};
}

//Select top N results by sent_uid
std::vector<ScoredSentence> plain_rank_cut(std::vector<ScoredSentence> relevant_sents,
                                           size_t n_max_result){
    auto n_found = relevant_sents.size();
    if(!n_found) return relevant_sents;
    auto n_cut = std::min(n_max_result, n_found);
    auto beg = relevant_sents.begin();
    auto rank_cut = beg+n_cut;
    std::partial_sort(beg,rank_cut,relevant_sents.end(),
                      [](auto const &x, auto const &y){return x.score > y.score;});
    auto score_cutoff = 0.5*relevant_sents.front().score;
    rank_cut = std::find_if_not(beg, rank_cut,
                                [score_cutoff](auto const &x){return x.score>score_cutoff;});
    std::vector<ScoredSentence> top_n_results;
    std::copy(beg, rank_cut, std::back_inserter(top_n_results));
    return top_n_results;
}
//Select top N results by chunk_idx
//TODO : optimize this function.
std::vector<ScoredSentence> rank_cut_by_unique_chunk(std::vector<ScoredSentence> relevant_sents,
                                           size_t n_unique_chunk_idx){
    if(relevant_sents.empty()) return relevant_sents;
    n_unique_chunk_idx = std::min(relevant_sents.size(),n_unique_chunk_idx);

    auto beg = relevant_sents.begin();
    auto end = relevant_sents.end();
    util::Timer timer;
    timer.here_then_reset(fmt::format("Begin sorting with {} sentences", n_unique_chunk_idx));
    std::partial_sort(beg,end,end, [](auto const &x, auto const &y){
        if(x.score==y.score)
            return x.sent.dict->chunk_idx(x.sent.front())<y.sent.dict->chunk_idx(y.sent.front());
        return x.score > y.score;});
    timer.here_then_reset("Sorting finished");
    auto score_cutoff = 0.5*relevant_sents.front().score;
    auto rank_cut = beg;
    std::set<ChunkIndex> chunk_idxs;
    while(rank_cut!=end && chunk_idxs.size()<n_unique_chunk_idx){
        if(rank_cut->score<score_cutoff) break;
        chunk_idxs.insert(rank_cut->sent.dict->chunk_idx(rank_cut->sent.front()));
        ++rank_cut;
    }
    std::vector<ScoredSentence> top_n_results;
    std::copy(beg, rank_cut, std::back_inserter(top_n_results));
    timer.here_then_reset("Pick top N result:  finished");
    return top_n_results;
}


PerSentQueryResult build_query_result_POD(
        Sentence const &query_sent, ScoredSentence const &matched_sentence,
        DBIndexer const &db_indexer, int64_t max_clip_len){
    auto const &scores = matched_sentence.scores;
    auto sent = matched_sentence.sent;
    auto scores_with_idxs = scores.serialize();
    std::sort(scores_with_idxs.begin(), scores_with_idxs.end(),
              [](auto const &x, auto const &y){return std::get<2>(x)>std::get<2>(y);});

    auto const &tokens = *(sent.dict);
    auto const &query_tokens = *(query_sent.dict);

    auto chunk_idx = tokens.chunk_idx(sent.front());
    auto row_uid = db_indexer.row_uid(chunk_idx);//if a chunk is a row, chunk_idx is row_uid
    auto col_uid = db_indexer.column_uid(chunk_idx);
    auto row_idx = db_indexer.row_idx(chunk_idx);

    PerSentQueryResult result;
    for(auto elm : scores_with_idxs){
        auto lhs_idx = std::get<0>(elm);
        auto rhs_idx = std::get<1>(elm);
        auto score   = std::get<2>(elm);
        if(score==0.0) continue;
        ScoreWithOffset tmp;
        tmp.score = score;
        tmp.query_token.beg = query_tokens.word_beg(lhs_idx.front()).val;
        tmp.query_token.end = query_tokens.word_end(lhs_idx.back()).val;
        tmp.matched_token.beg = tokens.word_beg(rhs_idx.front()).val;
        tmp.matched_token.end = tokens.word_end(rhs_idx.back()).val;
        result.scores_with_offset.push_back(tmp);
    }
    result.score = scores.score_sum();
    data::set_db_info(result, col_uid, row_uid, row_idx, sent);
    result.highlight_offset = {0,0};
    auto clip_offset = get_clip_offset(sent, scores, tokens, max_clip_len);
    result.clip_offset = {clip_offset.first.val, clip_offset.second.val};

    return result;
}

}//namespace engine

