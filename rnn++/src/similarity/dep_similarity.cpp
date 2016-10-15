#include <algorithm>

#include "similarity/dep_similarity.h"

#include "similarity/similarity_measure.h"

#include "fmt/printf.h"

#include "utils/parallel.h"
//#include "utils/span.h"
#include "utils/string.h"
#include "utils/hdf5.h"
#include "utils/math.h"

using namespace wordrep;
using namespace util::io;

//TODO: merge this with BoWQuery.
struct BoWVQuery2{
    using voca_info_t  = wordrep::VocaInfo;
    using word_block_t = voca_info_t::voca_vecs_t;
    using val_t        = word_block_t::val_t;
    using idx_t        = word_block_t::idx_t;

    BoWVQuery2(std::vector<VocaIndex> const &words, std::vector<val_t> cutoffs,
               voca_info_t const &voca)
    : idxs{words}, cutoffs{cutoffs}, distances(cutoffs.size())
    {
        auto n= voca.wvecs.size();
        auto n_queries=idxs.size();
        for(auto& v: distances) v.resize(n);
        tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000),
                          [&](tbb::blocked_range<decltype(n)> const &r){
                              for(decltype(n) i=r.begin(); i!=r.end(); ++i){
                                  for(decltype(n_queries)qi=0; qi!=n_queries; ++qi){
                                      auto q = voca.wvecs[idxs[qi]];
                                      auto widx = idx_t{i};
                                      get_distance(qi,widx) = similarity::Similarity<similarity::measure::angle>{}(voca.wvecs[widx], q);
                                  }
                              }
                          });
    }

    val_t get_distance(size_t i, idx_t widx) const { return distances[i][widx.val];}
    val_t& get_distance(size_t i, idx_t widx) { return distances[i][widx.val];}
    val_t get_distance(WordPosIndex i, idx_t widx) const { return distances[i.val-1][widx.val];}

    bool is_similar(util::span_dyn<idx_t> widxs){
        auto n = cutoffs.size();
        auto end=std::cend(widxs);
        for(decltype(n)i=0; i!=n; ++i){
            auto cut= cutoffs[i];
            auto result = std::find_if(std::cbegin(widxs), end, [&](auto widx){
                return get_distance(i, widx) >=cut;
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
    DepParsedQuery(std::vector<val_t> const &cutoff, nlohmann::json const &sent, wordrep::VocaIndexMap const &voca)
    : len{cutoff.size()}, cutoff{cutoff}, word(len), word_pidx(len), head_word(len), head_pidx(len), arc_label(len){ //
//        fmt::print("len : {}\n", len);
        for(auto const&x : sent["basic-dependencies"]) {
            auto i = x["dependent"].get<int64_t>() - 1;
            word[i]  = voca[wordUIDs[x["dependentGloss"].get<std::string>()]];
            word_pidx[i] = WordPosIndex{x["dependent"].get<WordPosIndex::val_t>()};
            head_word[i] = voca[wordUIDs[x["governorGloss"].get<std::string>()]];
            head_pidx[i] = WordPosIndex{x["governor"].get<WordPosIndex::val_t>()};
            //TODO: fix it.
            arc_label[i]= ArcLabel{int64_t{0}};//x["dep"];
//            fmt::print("{} {} {} {} {}, {}\n", word[i], word_pidx[i], head_word[i], head_pidx[i], arc_label[i], cutoff[i]);
        }
//        fmt::print("\n");
//        for(auto &x :sent["tokens"]) fmt::print("{} {}\n", x["pos"].get<std::string>(), x["word"].get<std::string>());
    }

    bool is_similar(Sentence const &sent, DepParsedTokens const &words,
                    BoWVQuery2 const &similarity) const {
        std::vector<bool> is_found(len, false);
        auto beg=sent.beg.val;
        auto end=sent.end.val;
        for(auto i=beg; i<end; ++i) {
            auto word = words.word[i];
            auto head_word = words.head_word[i];
            for(decltype(len)j=0; j<len; ++j){
                if(cutoff[j]<1.0){
                    if(similarity.get_distance(j,word) >= cutoff[j]) {
                        is_found[j] = true;
//                        fmt::print("{} ", j);
                    }
                } else {
                    if((similarity.get_distance(j,word) >= cutoff[j]) &&
                       (similarity.get_distance(head_pidx[j], head_word) >=get_cutoff(head_pidx[j]))) is_found[j] = true;
                }
            }
        }
//        fmt::print(": {} \n", sent.uid.val);
        for(decltype(len)j=0; j<len; ++j){
            if(cutoff[j]==0.0) is_found[j] = true;
        }
        auto n_found = std::count_if(is_found.cbegin(), is_found.cend(), [](bool x){return x;});
        if(1.0*n_found/is_found.size()>0.8) {
            fmt::print("{} {}\n", sent.uid.val, n_found);
            return true;
        }
        return false;
        return std::all_of(is_found.cbegin(), is_found.cend(), [](bool i){ return i;});
    }

    val_t get_cutoff (WordPosIndex idx) const {return cutoff[idx.val -1];}

    std::size_t len;
    std::vector<val_t> cutoff;
    std::vector<VocaIndex> word;
    std::vector<WordPosIndex> word_pidx;
    std::vector<VocaIndex> head_word;
    std::vector<WordPosIndex> head_pidx;
    std::vector<ArcLabel> arc_label;
    WordUIDindex wordUIDs{"/home/jihuni/word2vec/ygp/words.uid"};
};


namespace engine {

DepSimilaritySearch::DepSimilaritySearch(json_t const &config)
: voca{config["wordvec_store"], config["voca_name"],
       config["w2vmodel_name"], config["w2v_float_t"]},
  tokens{H5file{H5name{config["dep_parsed_store"].get<std::string>()},
                hdf5::FileMode::read_exist}, config["dep_parsed_text"]},
  wordUIDs{"/home/jihuni/word2vec/ygp/words.uid"},
  word_cutoff{H5file{H5name{"/home/jihuni/word2vec/ygp/prob.test.h5"}, hdf5::FileMode::read_exist}},
  sents{tokens.SegmentSentences()},
  sents_plain{util::string::readlines(config["plain_text"])}
{}



DepSimilaritySearch::json_t DepSimilaritySearch::process_queries(json_t ask) const {
    nlohmann::json& sent_json = ask["sentences"][0];
    std::string query_str = ask["queries"][0];
    auto words = util::string::split(query_str);
    std::vector<DepParsedQuery::val_t> cutoff;
    for(auto word :words)  cutoff.push_back((word_cutoff.cutoff(wordUIDs[word])));
    for(auto word :words)  fmt::print("{} ", voca.indexmap[wordUIDs[word]].val);
    fmt::print(" : VocaIndex\n");
    for(int i=0; i<words.size(); ++i) fmt::print("{} : {}\n", words[i], cutoff[i]);
    DepParsedQuery query{cutoff, sent_json, voca.indexmap};
    BoWVQuery2 similarity{query.word, cutoff, voca};

    json_t answer{};
    for(auto sent: sents){
        if( query.is_similar(sent, tokens, similarity)) {
            answer[query_str].push_back(sent.uid.val);
        }
    }
    return answer;
}

}//namespace engine