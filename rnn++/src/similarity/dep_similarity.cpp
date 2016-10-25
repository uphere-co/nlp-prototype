#include <algorithm>
#include <map>

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
class BoWVQuery2{
public:
    using voca_info_t  = wordrep::VocaInfo;
    using word_block_t = voca_info_t::voca_vecs_t;
    using val_t        = word_block_t::val_t;

    BoWVQuery2(std::vector<VocaIndex> const &words,
               voca_info_t const &voca)
    : idxs{words}, distances(idxs.size())
    {
        auto n= voca.wvecs.size();
        auto n_queries=idxs.size();
        for(auto& v: distances) v.resize(n);
        tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000),
                          [&](tbb::blocked_range<decltype(n)> const &r){
                              for(decltype(n) i=r.begin(); i!=r.end(); ++i){
                                  for(int64_t qi=0; qi!=n_queries; ++qi){
                                      auto q = voca.wvecs[idxs[qi]];
                                      auto widx = VocaIndex{i};
                                      get_distance(WordPosition{qi},widx) = similarity::Similarity<similarity::measure::angle>{}(voca.wvecs[widx], q);
                                  }
                              }
                          });
    }

    val_t& get_distance(WordPosition i, VocaIndex widx) { return distances[i.val][widx.val];}
    val_t get_distance(WordPosition i, VocaIndex widx) const { return distances[i.val][widx.val];}
private:
    std::vector<VocaIndex> idxs;
    std::vector<std::vector<val_t>> distances;
};

//TODO: remove code duplication for parsing CoreNLP outputs
class DepParsedQuery{
public:
    using val_t = BoWVQuery2::val_t;
    DepParsedQuery(std::vector<val_t> const &cutoff, nlohmann::json const &sent)
    : len{cutoff.size()}, cutoff{cutoff}, words_pidx(len), heads_pidx(len), arc_labels(len){ //
        for(auto const&x : sent["basic-dependencies"]) {
            auto i = x["dependent"].get<int64_t>() - 1;
            words_pidx[i] = WordPosition{x["dependent"].get<WordPosition::val_t>()-1};
            heads_pidx[i] = WordPosition{x["governor"].get<WordPosition::val_t>()-1};
            //TODO: fix it.
            arc_labels[i]= ArcLabelUID{int64_t{0}};//x["dep"];
        }
    }

    val_t get_score(Sentence const &sent, DepParsedTokens const &data_tokens,
                    BoWVQuery2 const &similarity) const {
        auto beg=sent.beg;
        auto end=sent.end;
        val_t total_score{0.0};
        for(decltype(len)j=0; j<len; ++j){
            val_t score{0.0};
            for(auto i=beg; i!=end; ++i) {
                auto word = data_tokens.word(i);
                auto head_word = data_tokens.head_word(i);
                auto dependent_score = similarity.get_distance(WordPosition{j},word);
                if(heads_pidx[j].val<0) {
                    auto tmp = cutoff[j] * dependent_score;
                    score = std::max(tmp, score);
                } else {
                    auto governor_score = similarity.get_distance(heads_pidx[j], head_word);
                    auto tmp = cutoff[j] * dependent_score * (1 + governor_score)*get_cutoff(heads_pidx[j]);
                    score = std::max(tmp, score);
                }
            }
            total_score += score;
        }
        //auto total_score = util::math::sum(util::span_dyn<val_t>{scores});
        return total_score;
    }
    val_t get_cutoff (WordPosition idx) const {return cutoff[idx.val];}
    std::size_t n_words() const {return len;}

private:
    std::size_t len;
    std::vector<val_t> cutoff;
    std::vector<WordPosition> words_pidx;
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

struct QueryResultBuilder{
    using json_t = DepSimilaritySearch::json_t;
    QueryResultBuilder(){}

    json_t answer;
};

DepSimilaritySearch::json_t DepSimilaritySearch::process_query(json_t sent_json) const {
    std::vector<std::string> words;
    for(auto const &x : sent_json["basic-dependencies"])
        words.push_back(x["dependentGloss"].get<std::string>());
    std::vector<DepParsedQuery::val_t> cutoff;
    std::vector<VocaIndex> vidxs;
    for(auto const &word : words) {
        auto wuid = wordUIDs[word];
        cutoff.push_back(word_cutoff.cutoff(wuid));
        auto vuid = voca.indexmap[wuid];
        if(vuid == VocaIndex{}) vuid = voca.indexmap[WordUID{}];
        vidxs.push_back(vuid);
    }
    for(size_t i=0; i<words.size(); ++i) {
        fmt::print("{:<10} {:6}.uid {:6}.vocaindex : {}\n", words[i], wordUIDs[words[i]].val,
                   voca.indexmap[wordUIDs[words[i]]].val, cutoff[i]);
    }

    DepParsedQuery query{cutoff, sent_json};
    BoWVQuery2 similarity{vidxs, voca};

    std::vector<std::pair<DepParsedQuery::val_t, Sentence>> relevant_sents{};
    std::map<DepParsedQuery::val_t, bool> is_seen{};
    for(auto sent: sents) {
        auto score = query.get_score(sent, tokens, similarity);
        if ( score > query.n_words()*0.2 && !is_seen[score]) {
            relevant_sents.push_back(std::make_pair(score,sent));
            is_seen[score] = true;
        }
    }
    auto n_found = relevant_sents.size();

    json_t answer{};
    if(!n_found) return answer;
    auto n_max_result=n_found>5? 5 : n_found;
    auto rank_cut = relevant_sents.begin()+n_max_result;
    std::partial_sort(relevant_sents.begin(),rank_cut,relevant_sents.end(),
                      [](auto const &x, auto const &y){return x.first>y.first;});
    auto score_cutoff = 0.5*relevant_sents[0].first;
    rank_cut = std::find_if_not(relevant_sents.begin(), rank_cut,
                                [score_cutoff](auto const &x){return x.first>score_cutoff;});
    for(auto it=relevant_sents.cbegin(); it!=rank_cut; ++it){
        auto const &pair = *it;
        auto sent = pair.second;
        auto chunk_idx = tokens.chunk_idx(sent.beg);
        auto row_uid = ygp_indexer.row_uid(chunk_idx);//if a chunk is a row, chunk_idx is row_uid
        auto col_uid = ygp_indexer.column_uid(chunk_idx);
        auto row_id = ygp_indexer.row_idx(chunk_idx);
        answer["score"].push_back(pair.first);
        auto sent_to_str=[&](auto &sent){
            std::stringstream ss;
            for(auto i=sent.beg; i!=sent.end; ++i) {ss <<  wordUIDs[voca.indexmap[tokens.word(i)]]<< " ";}
            return ss.str();
        };
        answer["result_DEBUG"].push_back(sent_to_str(sent));
        answer["result_row_uid"].push_back(row_uid.val);
        answer["result_row_idx"].push_back(row_id.val);
        answer["result_column_uid"].push_back(col_uid.val);
        auto beg = tokens.word_beg(sent.beg).val;
        auto end = tokens.word_end(--sent.end).val;
        answer["result_offset"].push_back({beg,end});
        answer["result_raw"].push_back(texts.getline(row_uid));
        answer["highlight_offset"].push_back({beg+10, beg+60<end?beg+60:end});
        answer["cutoffs"] = cutoff; //TODO : meaningless unless user can adjust these
        answer["words"] = words; //TODO: removable?
    }
    return answer;
}

}//namespace engine
