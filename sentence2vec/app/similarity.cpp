#include <fstream>

#include "wordrep/sentence2vec.h"

#include "parser/parser.h"
#include "parser/wordvec.h"
#include "utils/profiling.h"
#include "utils/parallel.h"
#include "utils/linear_algebra.h"
#include "utils/print.h"
#include "utils/loop_gen.h"

#include "json/json.hpp"

using namespace sent2vec;
using namespace rnn::wordrep;
using namespace rnn::simple_model;
using namespace util;
using namespace util::math;
using namespace util::io;
using json = nlohmann::json;

namespace{
using val_t = double;
using idx_t = std::size_t;
//constexpr int word_dim=100; //already declared in sentence2vec.h
constexpr util::DataType w2vmodel_f_type = util::DataType::dp;
}//nameless namespace


struct Query{
    using vec_view_t = WordBlock::span_t;
    Query(std::string word, vec_view_t vec, Voca const &voca)
    :query_word{word}, query_vec{vec}, distances(voca.size())
    {}
    std::string query_word;
    vec_view_t query_vec;
    std::vector<val_t> distances;
};

auto process_query_angle=[](Query &query, auto const& voca_vecs){
    auto n=voca_vecs.size();
    assert(n==query.distances.size());
    auto q=query.query_vec;
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000), 
                      [&](tbb::blocked_range<decltype(n)> const &r){
        for(decltype(n) i=r.begin(); i!=r.end(); ++i){
            auto const& v=voca_vecs[i];
            query.distances[i] =dot(v,q)/std::sqrt(dot(v,v)*dot(q,q));//sigmoid(v,q);
            // distances2[i]=dot(v,q2)/std::sqrt(dot(v,v)*dot(q2,q2));//sigmoid(v,q2);
        }
    });
};

auto euclidean_distance_i=[](int64_t i, auto &out, auto const &x, auto const &y){
    auto tmp=x[i]-y[i];
    out += tmp*tmp;
};
auto euclidean_distance=[](auto const &x, auto const &y){
    // pa||pb == pa[i] log(pb[i]/pa[i]) 
    VecLoop_void<val_t,word_dim> vecloop_void{};
    val_t distance{};
    vecloop_void(euclidean_distance_i, distance, x, y);
    return distance;
};

auto process_query_euclidean=[](Query &query, auto const& voca_vecs){
    auto n=voca_vecs.size();
    assert(n==query.distances.size());
    auto q=query.query_vec;
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000), 
                      [&](tbb::blocked_range<decltype(n)> const &r){
        for(decltype(n) i=r.begin(); i!=r.end(); ++i){
            auto const& v=voca_vecs[i];
            query.distances[i] = -euclidean_distance(v,q);
            // distances2[i]=dot(v,q2)/std::sqrt(dot(v,v)*dot(q2,q2));//sigmoid(v,q2);
        }
    });
};

auto process_queries_simple=[](std::vector<Query> &queries, auto const& voca_vecs){
    for(auto &query:queries) process_query(query, voca_vecs);
};
auto process_queries_angle=[](std::vector<Query> &queries, auto const& voca_vecs){    
    auto n=voca_vecs.size();
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000), 
                      [&](tbb::blocked_range<decltype(n)> const &r){
        for(decltype(n) i=r.begin(); i!=r.end(); ++i){
            for(auto &query:queries){
                auto q=query.query_vec;
                auto const& v=voca_vecs[i];
                query.distances[i] =dot(v,q)/std::sqrt(dot(v,v)*dot(q,q));//sigmoid(v,q);
            }            
        }
    });
};

auto process_queries_innerdot=[](std::vector<Query> &queries, auto const& voca_vecs){
    auto n=voca_vecs.size();
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000),
                      [&](tbb::blocked_range<decltype(n)> const &r){
                          for(decltype(n) i=r.begin(); i!=r.end(); ++i){
                              for(auto &query:queries){
                                  auto q=query.query_vec;
                                  auto const& v=voca_vecs[i];
                                  query.distances[i] = dot(v,q);
                              }
                          }
                      });
};

auto process_queries_euclidean=[](std::vector<Query> &queries, auto const& voca_vecs){    
    auto n=voca_vecs.size();
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000), 
                      [&](tbb::blocked_range<decltype(n)> const &r){
        for(decltype(n) i=r.begin(); i!=r.end(); ++i){
            for(auto &query:queries){
                auto q=query.query_vec;
                auto const& v=voca_vecs[i];
                auto tmp{q};
                tmp-=v;
                query.distances[i] = -euclidean_distance(v,q);
            }            
        }
    });
};
void display_query(Query const &query, std::vector<std::string> const &sents){
    auto n_top = 20;
    std::vector<idx_t> idxs(query.distances.size());
    idx_t i{0};
    for(auto &x:idxs) x=i++;
    std::partial_sort(idxs.begin(),idxs.begin()+n_top,idxs.end(),
                      [&](auto i, auto j){return query.distances[i]>query.distances[j];});
    print("------------------\n");
    for(auto it=idxs.begin(); it!=idxs.begin()+n_top; ++it){
        print(sents[*it]);
        print("\n");
    }
}
void display_query(Query const &query, Voca const &voca, json &output){
    auto n_top = 20;
    std::vector<idx_t> idxs(query.distances.size());
    idx_t i{0};
    for(auto &x:idxs) x=i++;
    std::partial_sort(idxs.begin(),idxs.begin()+n_top,idxs.end(),
                      [&](auto i, auto j){return query.distances[i]>query.distances[j];});
    // print("------------------\n");
    json answer;    
    answer["query"]=query.query_word;
    json similar_ones;
    auto beg=idxs.begin();
    for(auto it=beg; it!=beg+n_top; ++it){
        // print(voca.getWord(*it).val);
        // print(query.distances[*it]);
        // print("\n");
        similar_ones[it-beg] = voca.getWord(*it).val;
    }
    answer["result"]=similar_ones;
    output[output.size()]=answer;
}
json display_queries(std::vector<Query> const &queries, Voca const &voca){
    json output;
    for(auto &query:queries) display_query(query, voca, output);
    return output;
}

void KLdistance(){
    // pa||pb == pa[i] log(pb[i]/pa[i]) 
    VecLoop_void<val_t,word_dim> vecloop_void{};
}

struct SimilaritySearch{
    SimilaritySearch(json const &config)
    :   sent_vecs{load_voca_vecs<word_dim>(config["phrase_store"], config["phrase_vec"], w2vmodel_f_type)},
        phrase_voca{load_voca(config["phrase_store"], config["phrase_word"])},
        // phrase2idx{phrase_voca.indexing()},
        param{load_param(config["rnn_param_store"], config["rnn_param_uid"], util::DataType::dp)},
        rnn{config["wordvec_store"], config["voca_name"], config["w2vmodel_name"], util::DataType::dp}
    {}

    json process_queries(json ask){
        std::vector<Query> queries;
        for(auto const &line : ask["queries"]){        
            auto init_nodes = rnn.initialize_tree(line);
            DPtable table=dp_merging(param, init_nodes);
            auto phrases = table.get_phrases();
            for(auto const &phrase:phrases){
                auto parsed_tree_str = phrase->name.val;
                queries.emplace_back(parsed_tree_str, phrase->vec.span, phrase_voca);
            }
        }
        process_queries_innerdot(queries, sent_vecs);
        json answer=display_queries(queries, phrase_voca);
        return answer;
    }

    WordBlock sent_vecs;
    Voca phrase_voca;
    // VocaIndexMap phrase2idx;
    Param param; 
    VocaInfo rnn;
};

json load_json(std::string filename){
    json j;    
    std::ifstream jsonData(filename, std::ifstream::in);
    if(jsonData.is_open()) {
        jsonData >> j;
    }
    return j;
}

int main(){
    Timer timer{};
    
    auto j = load_json("/data/groups/uphere/similarity_test/input.json");
    SimilaritySearch engine{j};
    timer.here_then_reset("Search engine loaded.");
    auto answer=engine.process_queries(j);
    timer.here_then_reset("Queries answered.");
    std::cout << answer.dump(4) << std::endl;
    return 0;
}
