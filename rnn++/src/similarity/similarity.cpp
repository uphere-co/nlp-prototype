#include <fstream>
#include <utils/profiling.h>

#include "tbb/task_group.h"
#include "fmt/printf.h"

#include "similarity.h"
#include "similarity/similarity_measure.h"

#include "utils/parallel.h"
#include "utils/linear_algebra.h"
#include "utils/print.h"
#include "utils/json.h"
#include "utils/loop_gen.h"



//using namespace sent2vec;
using namespace rnn::wordrep;
using namespace rnn::simple_model;
using namespace util;
using namespace util::math;
using namespace util::io;
using namespace similarity;

namespace{
using json = nlohmann::json;

using val_t = SimilaritySearch::param_t::value_type ;
using idx_t = SimilaritySearch::voca_info_t::voca_idx_map_t::idx_t;
constexpr int word_dim=SimilaritySearch::param_t::dim;

struct Query{
    using word_block_t = SimilaritySearch::voca_info_t::voca_vecs_t;
    using vec_view_t = word_block_t::span_t;
    using vec_t = Vector<word_block_t::float_t, word_dim>;
    Query(std::string word, vec_view_t vec, Voca const &voca)
            :query_word{word}, query_vec{vec}, distances(voca.size())
    {}
    std::string query_word;
    vec_t query_vec;
    std::vector<val_t> distances;
};


template<measure T>
void process_query(Query &query, SimilaritySearch::voca_info_t::voca_vecs_t const& voca_vecs){
    auto n=voca_vecs.size();
    assert(n==query.distances.size());
    auto q=query.query_vec.span;
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000), 
                      [&](tbb::blocked_range<decltype(n)> const &r){
        for(decltype(n) i=r.begin(); i!=r.end(); ++i){
            auto const& v=voca_vecs[i];
            query.distances[i] =Similarity<T>(v,q);
        }
    });
};

template<measure T>
void process_queries(std::vector<Query> &queries, SimilaritySearch::voca_info_t::voca_vecs_t const& voca_vecs){
    auto n=voca_vecs.size();
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000), 
                      [&](tbb::blocked_range<decltype(n)> const &r){
        for(decltype(n) i=r.begin(); i!=r.end(); ++i){
            for(auto &query:queries){
                auto q=query.query_vec.span;
                auto const& v=voca_vecs[i];
                query.distances[i] =Similarity<T>{}(v,q);
            }            
        }
    });
};

void collect_query_result(Query const &query, Voca const &voca, json &output){
    auto n_top = 20;
    std::vector<idx_t> idxs(query.distances.size());
    idx_t i{0};
    for(auto &x:idxs) x=i++;
    std::partial_sort(idxs.begin(),idxs.begin()+n_top,idxs.end(),
                      [&](auto i, auto j){return query.distances[i]>query.distances[j];});
    print("-----------------------------------------\n");
    print(query.query_word);
    print("\n------------------\n");
    json answer;    
    answer["query"]=query.query_word;
    json similar_ones;
    auto beg=idxs.begin();
    for(auto it=beg; it!=beg+n_top; ++it){
        print(voca.getWord(*it).val);
        print(query.distances[*it]);
        print("\n");
        similar_ones[it-beg] = voca.getWord(*it).val;
    }
    answer["result"]=similar_ones;
    output[output.size()]=answer;
}

json collect_queries_results(std::vector<Query> const &queries, Voca const &voca){
    json output;
    for(auto &query:queries) collect_query_result(query, voca, output);
    return output;
}

}//nameless namespace


SimilaritySearch::json_t SimilaritySearch::process_queries(json_t ask) const {
    std::vector<Query> queries;
    for(auto const &line : ask["queries"]){        
        auto init_nodes = rnn.initialize_tree(line);
        if(init_nodes.val.size()==1){
            auto& node=init_nodes.val[0];
            queries.emplace_back(node.name.val, node.vec.span, phrase_voca);
        }
        DPtable table=dp_merging(param, init_nodes);
        auto phrases = table.get_phrases();
        for(auto const &phrase:phrases){
            auto parsed_tree_str = phrase->name.val;
            queries.emplace_back(parsed_tree_str, phrase->vec.span, phrase_voca);
        }
    }
    ::process_queries<measure::angle>(queries, sent_vecs);
    json_t answer=collect_queries_results(queries, phrase_voca);
    return answer;
}

struct BoWVQuery{
    using word_block_t = BoWVSimilaritySearch::voca_info_t::voca_vecs_t;
    using val_t        = word_block_t::float_t;
    using idx_t        = word_block_t::idx_t;

    BoWVQuery(std::string query, std::vector<val_t> cutoffs,
              BoWVSimilaritySearch::voca_info_t const &rnn)
    : idxs{rnn.word2idx.getIndex(query)}, cutoffs{cutoffs}, distances(idxs.size()), str{query}
    {
        auto n=rnn.voca_vecs.size();
        auto n_queries=idxs.size();
        for(auto& v: distances) v.resize(n);
        tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000),
                          [&](tbb::blocked_range<decltype(n)> const &r){
                              for(decltype(n) i=r.begin(); i!=r.end(); ++i){
                                  for(decltype(n_queries)qi=0; qi!=n_queries; ++qi){
                                      auto q = rnn.voca_vecs[idxs[qi]];
                                      distances[qi][i]=Similarity<measure::angle>{}(rnn.voca_vecs[i], q);
                                  }
                              }
                          });
    }

    bool is_similar(util::span_dyn<const int32_t> widxs){
        auto n = idxs.size();
        for(decltype(n)i=0; i!=n; ++i){
            auto cut= cutoffs[i];
            auto end=std::cend(widxs);
            auto result = std::find_if(std::cbegin(widxs), end, [&](auto widx){
//                    auto w=rnn.voca_vecs[widx];
//                    return Similarity<measure::angle>(w,q) >= cut;
                return distances[i][widx] >=cut;
            });
            if(result==end) return false;
        }
        return true;
    }

    std::vector<idx_t> idxs;
    std::vector<val_t> cutoffs;
    std::vector<std::vector<val_t>> distances;
    std::string str;
};


BoWVSimilaritySearch::json_t BoWVSimilaritySearch::process_queries(json_t ask) const{
    std::vector<BoWVQuery> queries;
    auto const& query_strs=ask["queries"];
    auto const& cutoffs=ask["cutoffs"];
    auto n_queries = std::cend(cutoffs) - std::cbegin(cutoffs);
    util::Timer timer{};
    for(decltype(n_queries)i=0; i!=n_queries; ++i){
        std::string words=query_strs[i];
        std::cerr<<"Words: "<<words << std::endl;
        auto cutoff = cutoffs[i];
        BoWVQuery query{words, cutoff, rnn};
        queries.push_back(query);
        timer.here_then_reset("Construct query.");
    }
    json_t answer{};
    auto n = text.sents.size();
    for(decltype(n)i=0; i!=n; ++i){
        auto sent_widxs = text.sents[i];
        for(auto &query : queries){
            bool is_similar = query.is_similar(sent_widxs);
            if(is_similar) answer[query.str].push_back(lines.val[i]);
        }
    }
    timer.here_then_reset("Queries are answered.");
    return answer;
}

std::vector<int32_t> load_data(std::string datset_name) {
    using namespace util::io;
    H5file h5store{H5name{"indexed_text.h5"}, hdf5::FileMode::read_exist};
    return h5store.getRawData<int32_t>(H5name{datset_name});
}

/*


struct WordIndex{int64_t val;};
struct VocaIndex{int64_t val;};
struct ParsedWord{
    ParsedWord(util::io::H5file const &file, std::string prefix)
            : sent_idx{file.getRawData<int64_t>(H5name{prefix+".sent_idx"})},
              word_raw{file.getRawData<char>(H5name{prefix+".word"})},
              word{util::string::unpack_word_views(word_raw)},
              idx_word{file.getRawData<int64_t>(H5name{prefix+".word_pidx"})},
              head_word_raw{file.getRawData<char>(H5name{prefix+".head_word"})},
              head_word{util::string::unpack_word_views(head_word_raw)},
              idx_head{file.getRawData<int64_t>(H5name{prefix+".head_pidx"})},
              arc_label_raw{file.getRawData<char>(H5name{prefix+".arc_label"})},
              arc_label{util::string::unpack_word_views(arc_label_raw)}
    {}
    std::vector<int64_t>     sent_idx;
    std::vector<char>        word_raw;
    std::vector<const char*> word;
    std::vector<int64_t>     idx_word;
    std::vector<char>        head_word_raw;
    std::vector<const char*> head_word;
    std::vector<int64_t>     idx_head;
    std::vector<char>        arc_label_raw;
    std::vector<const char*> arc_label;
};

void convert_h5py_to_native(){
//    auto voca = rnn::wordrep::load_voca("test.Google.h5", "news.en.words");
    auto voca = rnn::wordrep::load_voca("news.h5", "news.en.words");
    auto word2idx = voca.indexing();
    H5file infile{H5name{"news.Google.h5"}, hdf5::FileMode::read_exist};
    ParsedWord news{infile, "test"};
    ParsedWordIdx news_indexed{news, word2idx};
//    news_indexed.write_to_disk("test.Google.h5", "test");
    news_indexed.write_to_disk("news.dep.h5", "test");
}

std::vector<int64_t> get_voca_idxs(wordrep::VocaIndexMap const &word2idx,
                                   std::vector<const char*> words){
    wordrep::WordUIDindex wordUIDs{"/home/jihuni/word2vec/ygp/words.uid"};
    std::vector<int64_t> vidxs;
    for(auto x : words) {
        int64_t idx = word2idx.getIndex(wordUIDs[x]).val;
        vidxs.push_back(idx);
    }
    return vidxs;
}

//TODO : arc_label index, Sent end/beg
ParsedWordIdx::ParsedWordIdx(ParsedWord const &words, wordrep::VocaIndexMap const &word2idx)
    : sent_idx{words.sent_idx},
    word{get_voca_idxs(word2idx, words.word)},
    word_pidx{words.idx_word},
    head_word{get_voca_idxs(word2idx, words.head_word)},
    head_pidx{words.idx_head},
    arc_label_raw{util::string::pack_words(words.arc_label)},
    arc_label{util::string::unpack_word_views(arc_label_raw)}
{}


*/