#include <algorithm>
#include <map>
#include <utils/profiling.h>

#include "similarity/dep_similarity.h"

#include "similarity/similarity_measure.h"

#include "fmt/printf.h"

#include "utils/parallel.h"
#include "utils/profiling.h"
#include "utils/span.h"
#include "utils/string.h"
#include "utils/hdf5.h"
#include "utils/math.h"

using namespace wordrep;
using namespace util::io;

namespace engine {

void WordSimCache::cache(std::vector<VocaIndex> const &words) {
    auto n= voca.wvecs.size();
    std::vector<VocaIndex> words_to_cache;
    std::vector<dist_cache_t*> dists;
    for(auto vidx : words) {
        if(distance_caches.find(vidx) == distance_caches.end()) {
            distance_caches[vidx] = dist_cache_t{n};
            words_to_cache.push_back(vidx);
            dists.push_back(&distance_caches[vidx]);
        }
    }
    auto n_words = dists.size();

    auto dist_measure = similarity::Similarity<similarity::measure::angle>{};
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000),
                      [&](tbb::blocked_range<decltype(n)> const &r){
                          for(decltype(n) i=r.begin(); i!=r.end(); ++i){
                              for(decltype(n_words)j=0; j!=n_words; ++j ){
                                  auto qidx = words_to_cache[j];
                                  auto q = voca.wvecs[qidx];
                                  auto widx = VocaIndex{i};
                                  (*dists[j])[widx] = dist_measure(voca.wvecs[widx], q);
                              }
                          }
                      });
}


//TODO: remove code duplication for parsing CoreNLP outputs
class DepParsedQuery{
public:
    using val_t = WordSimCache::val_t;
    DepParsedQuery(std::vector<val_t> const &cutoffs, Sentence query_sent, WordSimCache const &similarity)
    : len{diff(query_sent.end,query_sent.beg)}, query_sent{query_sent}, cutoffs{cutoffs}, dists{} {
        for(auto idx=query_sent.beg; idx!=query_sent.end; ++idx)
            sorted_idxs.push_back({cutoffs[diff(idx,query_sent.beg)],idx});
        std::sort(sorted_idxs.begin(),sorted_idxs.end(),[](auto x, auto y){return x.first>y.first;});
        val_t sum=0.0;
        std::vector<val_t> cutoff_cumsum;
        for(auto pair : sorted_idxs) {
            sum += pair.first;
            cutoff_cumsum.push_back(sum);
        }
        auto it = std::find_if_not(cutoff_cumsum.cbegin(),cutoff_cumsum.cend(),
                                   [&cutoff_cumsum](auto x){return x/cutoff_cumsum.back()<0.3;});
        auto it2 = std::find_if_not(cutoff_cumsum.cbegin(),cutoff_cumsum.cend(),
                                   [&cutoff_cumsum](auto x){return x/cutoff_cumsum.back()<0.5;});
        auto it3 = std::find_if_not(cutoff_cumsum.cbegin(),cutoff_cumsum.cend(),
                                   [&cutoff_cumsum](auto x){return x/cutoff_cumsum.back()<0.8;});
        n_cut = it - cutoff_cumsum.cbegin();
        n_cut2 = it2 - cutoff_cumsum.cbegin();
        n_cut3 = it3 - cutoff_cumsum.cbegin();
        cut = *it * 0.21;
        cut2 = *it * 0.35;
        cut3 = *it * 0.5;

        for(auto idx=query_sent.beg; idx!=query_sent.end; ++idx)
            dists.push_back(&similarity.distances(query_sent.tokens->word(idx)));
    }

    auto get_scores(Sentence const &sent) const {
        auto beg=sent.beg;
        auto end=sent.end;
        val_t total_score{0.0};
        std::vector<std::pair<DPTokenIndex, val_t>>  scores(len);
        auto i_trial{0};

        for(auto pair: sorted_idxs){
            ++i_trial;
            DPTokenIndex tidx = pair.second;
            auto j = diff(tidx, query_sent.beg);
            val_t score{0.0};
            if(cutoffs[j]<0.4) continue;
            assert(query_sent.tokens->words_pidx[tidx.val].val==j);
            for(auto i=beg; i!=end; ++i) {
                auto word = sent.tokens->word(i);
                auto dependent_score = (*dists[j])[word];
                auto head_word = sent.tokens->head_word(i);
                auto qhead_pidx = query_sent.tokens->heads_pidx[tidx.val].val;
                if(cutoffs[qhead_pidx]<0.4) continue;
                if(qhead_pidx<0) {
                    auto tmp = cutoffs[j] * dependent_score;
                    if(tmp>score){
                        score = tmp;
                        scores[j] = {i, score};
                    }
                } else {
                    auto governor_score = (*dists[qhead_pidx])[head_word];
                    auto tmp = cutoffs[j] * dependent_score * (1 + governor_score*cutoffs[qhead_pidx]);
                    if(tmp>score){
                        score = tmp;
                        scores[j] = {i, score};
                    }
                }
            }

            total_score += score;
            if(i_trial==n_cut){
                if(total_score <cut) return scores;
            }
            else if(i_trial==n_cut2){
                if(total_score < cut2) return scores;
            }
            else if(i_trial==n_cut3){
                if(total_score < cut3) return scores;
            }
        }
        return scores;
    }
    std::size_t n_words() const {return len;}

private:
    int64_t len;
    Sentence query_sent;
    std::vector<val_t> cutoffs;
    std::vector<std::pair<val_t,DPTokenIndex>> sorted_idxs; //Descending order of cutoff.
    std::ptrdiff_t n_cut;
    std::ptrdiff_t n_cut2;
    std::ptrdiff_t n_cut3;
    val_t cut;
    val_t cut2;
    val_t cut3;
    std::vector<WordSimCache::dist_cache_t const*> dists;
};


DepSimilaritySearch::DepSimilaritySearch(json_t const &config)
: voca{config["wordvec_store"], config["voca_name"],
       config["w2vmodel_name"], config["w2v_float_t"]},
  tokens{H5file{H5name{config["dep_parsed_store"].get<std::string>()},
                hdf5::FileMode::read_exist}, config["dep_parsed_prefix"]},
  wordUIDs{config["word_uids_dump"].get<std::string>()},
  posUIDs{config["pos_uids_dump"].get<std::string>()},
  arclabelUIDs{config["arclabel_uids_dump"].get<std::string>()},
  word_cutoff{H5file{H5name{config["word_prob_dump"].get<std::string>()}, hdf5::FileMode::read_exist}},
  sents{tokens.IndexSentences()},
  ygpdb{config["column_uids_dump"].get<std::string>()},
  ygp_indexer{H5file{H5name{config["dep_parsed_store"].get<std::string>()},
                     hdf5::FileMode::read_exist}, config["dep_parsed_prefix"].get<std::string>()}
{}

//TODO: fix it to be thread-safe
DepSimilaritySearch::json_t DepSimilaritySearch::register_documents(json_t const &ask) {
    if (ask.find("sentences") == ask.end()) return json_t{};
    query_tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, ask);
    query_tokens.build_voca_index(voca.indexmap);
    auto uids = query_tokens.build_sent_uid(SentUID{SentUID::val_t{0x80000000}});
    std::cerr<<fmt::format("# of sents : {}\n", uids.size()) << std::endl;
    json_t answer{};
    for(auto uid :uids ) answer["sent_uids"].push_back(uid.val);
    return answer;
}

struct Query{
    using json_t = DepSimilaritySearch::json_t;
    Query(json_t const &ask){
        for(SentUID::val_t uid : ask["sent_uids"] ) uids.push_back(SentUID{uid});
    }
    static bool is_valid(json_t const &query){
        return query.find("sent_uids")!=query.end() && query.find("max_clip_len")!=query.end();
    }
    std::vector<SentUID> uids;
};
DepSimilaritySearch::json_t DepSimilaritySearch::process_query(json_t const &ask) const {
    if (!Query::is_valid(ask)) return json_t{};
    Query query{ask};
    std::vector<Sentence> query_sents{};
    std::vector<std::string> query_strs{};
    auto sent_to_str=[&](auto &sent){
        std::stringstream ss;
        for(auto i=sent.beg; i!=sent.end; ++i) {ss <<  wordUIDs[voca.indexmap[sent.tokens->word(i)]]<< " ";}
        return ss.str();
    };
    //TODO: fix it to be incremental
    auto qsents = query_tokens.IndexSentences();
    for(auto sent : qsents) fmt::print("{} user documents\n", qsents.size());
    for(auto uid : query.uids){
        auto it = std::find_if(sents.cbegin(), sents.cend(), [uid](auto sent){return sent.uid==uid;});
        if(it==sents.cend()) it=std::find_if(qsents.cbegin(), qsents.cend(), [uid](auto sent){return sent.uid==uid;});
        if(it==qsents.cend()) continue;
        auto sent = *it;
        query_sents.push_back(sent);
        query_strs.push_back(sent_to_str(sent));
    }
    fmt::print("Will process {} user documents\n", query_sents.size());
    return process_query_sents(query_sents, query_strs);
    auto max_clip_len = ask["max_clip_len"].get<int64_t>();
}

DepSimilaritySearch::json_t DepSimilaritySearch::process_query_sents(
        std::vector<wordrep::Sentence> query_sents, std::vector<std::string> query_strs) const {
    auto max_clip_len = 200;
    auto n_queries = query_sents.size();
    tbb::concurrent_vector<json_t> answers;
    tbb::task_group g;
    assert(query_sents.size()==n_queries);
    for(decltype(n_queries)i=0; i!=n_queries; ++i){
        std::string query_str = query_strs[i];
        auto query_sent = query_sents[i];
        if(query_sent.beg==query_sent.end) continue;
        auto query_sent_beg = query_sent.tokens->word_beg(query_sent.beg).val;
        auto query_sent_end = query_sent.tokens->word_end(query_sent.end-1).val;
        g.run([&answers,max_clip_len, query_sent,query_sent_beg,query_sent_end, query_str,this](){
            util::Timer timer{};
            std::vector<val_t> cutoffs;
            std::vector<VocaIndex> vidxs;
            std::vector<std::string> words;
            for(auto idx = query_sent.beg; idx!=query_sent.end; ++idx) {
                auto wuid = query_sent.tokens->word_uid(idx);
                auto word = wordUIDs[wuid];
                words.push_back(word);
                cutoffs.push_back(word_cutoff.cutoff(wuid));
                auto vuid = voca.indexmap[wuid];
                if(vuid == VocaIndex{}) vuid = voca.indexmap[WordUID{}];
                vidxs.push_back(vuid);
            }
            timer.here_then_reset("Get cutoffs");
            dists_cache.cache(vidxs);
            timer.here_then_reset("Built Similarity caches.");
            auto relevant_sents = this->process_query_sent(query_sent, cutoffs);
            timer.here_then_reset("Query answered.");
            auto answer = write_output(relevant_sents, max_clip_len);
            answer["input"]=query_str;
            answer["input_offset"]={query_sent_beg,query_sent_end};
            answer["input_uid"] = query_sent.uid.val;
            answer["cutoffs"] = cutoffs;
            answer["words"] = words;
            answers.push_back(answer);
        });
    }
    g.wait();
    json_t output{};
    for(auto &answer : answers) output.push_back(answer);
    return output;
}

std::vector<ScoredSentence> deduplicate_results(tbb::concurrent_vector<ScoredSentence> const &relevant_sents){
    using val_t = ScoredSentence::val_t;
    std::map<val_t, bool> is_seen{};
    std::vector<ScoredSentence> dedup_sents;
    for(auto const &sent : relevant_sents){
        auto score = sent.score;
        if(is_seen.find(score)!=is_seen.cend()) continue;
        is_seen[score] = true;
        dedup_sents.push_back(sent);
    }
    return dedup_sents;
}


std::vector<ScoredSentence> DepSimilaritySearch::process_query_sent(Sentence query_sent,
                                                                    std::vector<val_t> const &cutoffs) const {
    DepParsedQuery query{cutoffs, query_sent, dists_cache};

    tbb::concurrent_vector<ScoredSentence> relevant_sents{};
    auto n = sents.size();
    tbb::parallel_for(decltype(n){0}, n, [&](auto i) {
        auto sent = sents[i];
        auto scores = query.get_scores(sent);
        ScoredSentence scored_sent{sent, scores};
        if (scored_sent.score > query.n_words() * 0.2) {
            relevant_sents.push_back(scored_sent);
        }
    });
    return deduplicate_results(relevant_sents);
}

auto get_clip_offset = [](Sentence sent,
                          auto &scores, auto const &tokens, auto max_clip_len){
    std::sort(scores.begin(), scores.end(), [](auto x, auto y){return x.second>y.second;});
    auto pair = scores.front();
    auto i_word_beg = pair.first;
    auto i_word_end = pair.first;
    CharOffset clip_beg = tokens.word_beg(i_word_beg);
    CharOffset clip_end = tokens.word_end(i_word_end);
    auto len_sent = tokens.word_end(sent.end).val - tokens.word_beg(sent.beg).val;
    max_clip_len = max_clip_len>len_sent? len_sent:max_clip_len;
    auto max_len = typename decltype(clip_beg)::val_t{max_clip_len};

    for(auto pair : scores){
        auto idx = pair.first;
        //auto score = pair.second;
        auto beg = tokens.word_beg(idx);
        auto end = tokens.word_end(idx);
        if(beg<clip_beg && clip_end < beg+max_len ) {
            clip_beg = beg;
            i_word_beg = idx;
        } else if(end>clip_end && end < clip_beg+max_len ) {
            clip_end = end;
            i_word_end = idx;
        }
//        fmt::print("{} {} {} {}\n", idx.val, score, tokens.word_beg(idx).val, tokens.word_end(idx).val);
    }
    auto len = clip_end.val-clip_beg.val;
    while(max_len-len>0) {
        if(i_word_beg>sent.beg) {
            --i_word_beg;
            clip_beg = tokens.word_beg(i_word_beg);
        }
        if(i_word_end<sent.end){
            ++i_word_end;
            clip_end = tokens.word_end(i_word_end);
        }
        len = clip_end.val-clip_beg.val;
        if(i_word_beg==sent.beg && i_word_end==sent.end) break;
    }

//    fmt::print("{} {}\n", clip_beg.val, clip_end.val);
    return std::make_pair(clip_beg, clip_end);
};

DepSimilaritySearch::json_t DepSimilaritySearch::write_output(std::vector<ScoredSentence> relevant_sents,
                                                              int64_t max_clip_len) const{
    auto n_found = relevant_sents.size();
//    for(size_t i=0; i<words.size(); ++i) {
//        fmt::print("{:<10} {:6}.uid {:6}.vocaindex : {}\n", words[i], wordUIDs[words[i]].val,
//                   voca.indexmap[wordUIDs[words[i]]].val, cutoffs[i]);
//    }
    json_t answer{};
    if(!n_found) return answer;
    auto n_max_result=n_found>5? 5 : n_found;
    auto rank_cut = relevant_sents.begin()+n_max_result;
    std::partial_sort(relevant_sents.begin(),rank_cut,relevant_sents.end(),
                      [](auto const &x, auto const &y){return x.score > y.score;});
    auto score_cutoff = 0.5*relevant_sents[0].score;
    rank_cut = std::find_if_not(relevant_sents.begin(), rank_cut,
                                [score_cutoff](auto const &x){return x.score>score_cutoff;});
    for(auto it=relevant_sents.cbegin(); it!=rank_cut; ++it){
        auto const &scored_sent = *it;
        auto score = scored_sent.score;
        auto scores = scored_sent.scores;
        auto sent = scored_sent.sent;
        auto chunk_idx = tokens.chunk_idx(sent.beg);
        auto row_uid = ygp_indexer.row_uid(chunk_idx);//if a chunk is a row, chunk_idx is row_uid
        auto col_uid = ygp_indexer.column_uid(chunk_idx);
        auto row_idx = ygp_indexer.row_idx(chunk_idx);
        answer["score"].push_back(score);
        auto sent_to_str=[&](auto &sent){
            std::stringstream ss;
            for(auto i=sent.beg; i!=sent.end; ++i) {ss <<  wordUIDs[voca.indexmap[sent.tokens->word(i)]]<< " ";}
            return ss.str();
        };
        answer["result_DEBUG"].push_back(sent_to_str(sent));
        answer["result_sent_uid"].push_back(sent.uid.val);
        answer["result_row_uid"].push_back(row_uid.val);
        answer["result_row_idx"].push_back(row_idx.val);
        answer["result_table_name"].push_back(ygpdb.table(col_uid));
        answer["result_column_name"].push_back(ygpdb.column(col_uid));
        answer["result_index_col_name"].push_back(ygpdb.index_col(col_uid));
        answer["result_column_uid"].push_back(col_uid.val);
        auto beg = tokens.word_beg(sent.beg);
        auto end = tokens.word_end(--sent.end);
        auto clip_offset = get_clip_offset(sent, scores, tokens, max_clip_len);
        answer["result_offset"].push_back({beg.val,end.val});
        answer["clip_offset"].push_back({clip_offset.first.val, clip_offset.second.val});
        answer["highlight_offset"].push_back({beg.val+10, beg.val+60<end.val?beg.val+60:end.val});
    }
    return answer;

}

}//namespace engine
