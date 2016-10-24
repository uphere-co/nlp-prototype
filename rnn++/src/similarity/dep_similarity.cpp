#include <algorithm>

#include "similarity/dep_similarity.h"

#include "similarity/similarity_measure.h"

#include "fmt/printf.h"

#include "utils/parallel.h"
#include "utils/span.h"
#include "utils/string.h"
#include "utils/hdf5.h"
#include "utils/math.h"

using namespace wordrep;
using namespace util::io;

namespace {
//TODO: merge this with BoWQuery.
struct BoWVQuery2{
    using voca_info_t  = wordrep::VocaInfo;
    using word_block_t = voca_info_t::voca_vecs_t;
    using val_t        = word_block_t::val_t;
    using idx_t        = VocaIndex;

    BoWVQuery2(std::vector<idx_t> const &words, std::vector<val_t> cutoffs,
               voca_info_t const &voca)
    : idxs{words}, cutoffs{cutoffs}, distances(cutoffs.size())
    {
        auto n= voca.wvecs.size();
        auto n_queries=idxs.size();
        for(auto& v: distances) v.resize(n);
        tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000),
                          [&](tbb::blocked_range<decltype(n)> const &r){
                              for(decltype(n) i=r.begin(); i!=r.end(); ++i){
                                  for(int64_t qi=0; qi!=n_queries; ++qi){
                                      auto q = voca.wvecs[idxs[qi]];
                                      auto widx = idx_t{i};
                                      get_distance(WordPosition{qi},widx) = similarity::Similarity<similarity::measure::angle>{}(voca.wvecs[widx], q);
                                  }
                              }
                          });
    }

    val_t& get_distance(WordPosition i, idx_t widx) { return distances[i.val][widx.val];}
    val_t get_distance(WordPosition i, idx_t widx) const { return distances[i.val][widx.val];}

    bool is_similar(util::span_dyn<idx_t> widxs){
        auto n = cutoffs.size();
        auto end=std::cend(widxs);
        for(int64_t i=0; i!=n; ++i){
            auto cut= cutoffs[i];
            auto result = std::find_if(std::cbegin(widxs), end, [&](auto widx){
                return get_distance(WordPosition{i}, widx) >=cut;
            });
            if(result==end) return false;
        }
        return true;
    }

    std::vector<idx_t> idxs;
    std::vector<val_t> cutoffs;
    std::vector<std::vector<val_t>> distances;
};

//TODO: remove code duplication for parsing CoreNLP outputs
struct DepParsedQuery{
    using val_t = BoWVQuery2::val_t;
    DepParsedQuery(std::vector<val_t> const &cutoff, nlohmann::json const &sent,
                   wordrep::VocaIndexMap const &voca,
                   WordUIDindex const &wordUIDs)
    : len{cutoff.size()}, cutoff{cutoff}, words(len), words_pidx(len), head_words(len), heads_pidx(len), arc_labels(len){ //
//        fmt::print("len : {}\n", len);
        for(auto const&x : sent["basic-dependencies"]) {
            auto i = x["dependent"].get<int64_t>() - 1;
            words[i]  = voca[wordUIDs[x["dependentGloss"].get<std::string>()]];
            words_pidx[i] = WordPosition{x["dependent"].get<WordPosition::val_t>()-1};
            head_words[i] = voca[wordUIDs[x["governorGloss"].get<std::string>()]];
            heads_pidx[i] = WordPosition{x["governor"].get<WordPosition::val_t>()-1};
            //TODO: fix it.
            arc_labels[i]= ArcLabelUID{int64_t{0}};//x["dep"];
//            fmt::print("{} {} {} {} {}, {}\n", word[i], word_pidx[i], head_word[i], head_pidx[i], arc_label[i], cutoff[i]);
        }
//        fmt::print("\n");
//        for(auto &x :sent["tokens"]) fmt::print("{} {}\n", x["pos"].get<std::string>(), x["word"].get<std::string>());
    }

    val_t get_score(Sentence const &sent, DepParsedTokens const &data_tokens,
                    BoWVQuery2 const &similarity) const {
        std::vector<val_t> scores(len, 0.0);
        auto beg=sent.beg;
        auto end=sent.end;
        for(auto i=beg; i!=end; ++i) {
            auto word = data_tokens.word(i);
            auto head_word = data_tokens.head_word(i);
            for(decltype(len)j=0; j<len; ++j){
                auto dependent_score = similarity.get_distance(WordPosition{j},word);
                if(heads_pidx[j].val<0) {
                    auto score = cutoff[j] * dependent_score;
                    scores[j] = std::max(scores[j], score);
                } else {
                    auto governor_score = similarity.get_distance(heads_pidx[j], head_word);
                    auto score = cutoff[j] * dependent_score * (1 + governor_score)*get_cutoff(heads_pidx[j]);
                    scores[j] = std::max(scores[j], score);
                }
            }
        }
        auto total_score = util::math::sum(util::span_dyn<val_t>{scores});
        return total_score;
    }

    val_t get_cutoff (WordPosition idx) const {return cutoff[idx.val];}

    std::size_t len;
    std::vector<val_t> cutoff;
    std::vector<VocaIndex> words;
    std::vector<WordPosition> words_pidx;
    std::vector<VocaIndex> head_words;
    std::vector<WordPosition> heads_pidx;
    std::vector<ArcLabelUID> arc_labels;
};


struct SentenceProb{
    int get_rank_cutoff(Sentence const &sent) const {
        return 5;
    }
};

}//nameless namespace

namespace engine {

DepSimilaritySearch::DepSimilaritySearch(json_t const &config)
: voca{config["wordvec_store"], config["voca_name"],
       config["w2vmodel_name"], config["w2v_float_t"]},
  tokens{H5file{H5name{config["dep_parsed_store"].get<std::string>()},
                hdf5::FileMode::read_exist}, config["dep_parsed_prefix"]},
  wordUIDs{config["word_uids_dump"].get<std::string>()},
  word_cutoff{H5file{H5name{config["word_prob_dump"].get<std::string>()}, hdf5::FileMode::read_exist}},
  sents{tokens.IndexSentences()},
  texts{config["plain_text"].get<std::string>()},
  ygp_indexer{H5file{H5name{config["dep_parsed_store"].get<std::string>()},
                     hdf5::FileMode::read_exist}, config["dep_parsed_prefix"].get<std::string>()}
{}

DepSimilaritySearch::json_t DepSimilaritySearch::process_queries(json_t ask) const {
    json_t answers{};
    auto n_queries = ask["sentences"].size();
    for(decltype(n_queries)i=0; i!=n_queries; ++i){
        nlohmann::json& sent_json = ask["sentences"][i];
        std::string query_str = ask["queries"][i];
        json_t answer = process_query(sent_json);
        answer["input"]=query_str;
        answers.push_back(answer);
    }
    return answers;
}

DepSimilaritySearch::json_t DepSimilaritySearch::process_query(json_t sent_json) const {
    std::vector<std::string> words;
    for(auto const &x : sent_json["basic-dependencies"])
        words.push_back(x["dependentGloss"].get<std::string>());
    std::vector<DepParsedQuery::val_t> cutoff;
    for(auto const &word : words)
        cutoff.push_back(word_cutoff.cutoff(wordUIDs[word]));

    for(size_t i=0; i<words.size(); ++i)
        fmt::print("{:<10} {:6}.uid {:6}.vocaindex : {}\n", words[i], wordUIDs[words[i]].val,
                   voca.indexmap[wordUIDs[words[i]]].val,cutoff[i]);

    DepParsedQuery query{cutoff, sent_json, voca.indexmap, wordUIDs};
    BoWVQuery2 similarity{query.words, cutoff, voca};

    Sentence query_sent{SentUID{int64_t{0}}, DPTokenIndex{int64_t{0}}, DPTokenIndex{cutoff.size()}};
    SentenceProb sent_model{};
    auto rank_cutoff = sent_model.get_rank_cutoff(query_sent);

    json_t answer{};
    auto sent_to_str=[&](auto &sent){
        auto beg=sent.beg;
        auto end=sent.end;
        std::stringstream ss;
        for(auto i=beg; i!=end; ++i) {
            ss <<  wordUIDs[voca.indexmap[tokens.word(i)]]<< " ";
        }
        return ss.str();
    };

    //std::vector<std::pair<DepParsedQuery::val_t, SentUID>> relevant_sents{};
    std::vector<std::pair<DepParsedQuery::val_t, Sentence>> relevant_sents{};
    for(auto sent: sents) {
        auto score = query.get_score(sent, tokens, similarity);
        if ( score > query.len*0.2) {
            //relevant_sents.push_back(std::make_pair(score,sent.uid));
            relevant_sents.push_back(std::make_pair(score,sent));
        }
    }
    auto n_found = relevant_sents.size();
    auto n_max_result=n_found>5? 5 : n_found;
    auto rank_cut = relevant_sents.begin()+n_max_result;
    std::partial_sort(relevant_sents.begin(),rank_cut,relevant_sents.end(),
                      [](auto const &x, auto const &y){return x.first>y.first;});
    auto score_cutoff = 0.5*relevant_sents[0].first;
    rank_cut = std::find_if_not(relevant_sents.begin(), rank_cut,
                                [score_cutoff](auto const &x){return x.first>score_cutoff;});
    for(auto it=relevant_sents.cbegin(); it!=rank_cut; ++it){
        auto const &pair = *it;
        //auto sent = sents[pair.second.val];
        auto sent = pair.second;
        //answer[query_str].push_back(sent.uid.val);
        auto chunk_idx = tokens.chunk_idx(sent.beg);
        auto row_uid = ygp_indexer.row_uid(chunk_idx);//if a chunk is a row, chunk_idx is row_uid
        auto row_id = ygp_indexer.row_idx(chunk_idx);
        answer["score"].push_back(pair.first);
        answer["result"].push_back(sent_to_str(sent));
        answer["result_row_uid"].push_back(row_uid.val);
        answer["result_row_idx"].push_back(row_id.val);
        answer["result_column_uid"].push_back(-1);
        auto beg = tokens.word_beg(sent.beg).val;
        auto end = tokens.word_end(--sent.end).val;
        answer["result_offset"].push_back({beg,end});
        answer["result_raw"].push_back(texts.getline(row_uid));
        answer["highlight_offset"].push_back({beg+10, beg+60<end?beg+60:end});
        answer["cutoffs"] = cutoff;
        answer["words"] = words;
    }
    return answer;
}

}//namespace engine
