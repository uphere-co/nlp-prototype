#include <fstream>

#include "tbb/task_group.h"

//#include "wordrep/sentence2vec.h"

#include "parser/parser.h"
#include "parser/wordvec.h"
#include "utils/profiling.h"
#include "utils/parallel.h"
#include "utils/linear_algebra.h"
#include "utils/print.h"
#include "utils/json.h"
#include "utils/loop_gen.h"

#include "similarity.h"

//using namespace sent2vec;
using namespace rnn::wordrep;
using namespace rnn::simple_model;
using namespace util;
using namespace util::math;
using namespace util::io;

using json = nlohmann::json;


auto process_query_angle=[](Query &query, auto const& voca_vecs){
    auto n=voca_vecs.size();
    assert(n==query.distances.size());
    auto q=query.query_vec.span;
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000), 
                      [&](tbb::blocked_range<decltype(n)> const &r){
        for(decltype(n) i=r.begin(); i!=r.end(); ++i){
            auto const& v=voca_vecs[i];
            query.distances[i] =dot(v,q)/std::sqrt(dot(v,v)*dot(q,q));//sigmoid(v,q);
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
        }
    });
};

auto process_queries_angle=[](std::vector<Query> &queries, auto const& voca_vecs){    
    auto n=voca_vecs.size();
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000), 
                      [&](tbb::blocked_range<decltype(n)> const &r){
        for(decltype(n) i=r.begin(); i!=r.end(); ++i){
            for(auto &query:queries){
                auto q=query.query_vec.span;
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

void KLdistance(){
    // pa||pb == pa[i] log(pb[i]/pa[i]) 
    VecLoop_void<val_t,word_dim> vecloop_void{};
}

json SimilaritySearch::process_queries(json ask) const {
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
    //process_queries_innerdot(queries, sent_vecs);
    process_queries_angle(queries, sent_vecs);
    json answer=collect_queries_results(queries, phrase_voca);
    return answer;
}

