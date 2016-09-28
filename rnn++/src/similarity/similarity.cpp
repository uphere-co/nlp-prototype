#include <fstream>

#include "tbb/task_group.h"

//#include "wordrep/sentence2vec.h"

#include "parser/parser.h"
#include "parser/wordvec.h"
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

enum class measure{
    angle,
    inner,
    euclidean,
};
template<measure T>
val_t similarity(Query::vec_view_t v, Query::vec_view_t q);

template<>
val_t similarity<measure::angle>(Query::vec_view_t v, Query::vec_view_t q){
    return dot(v,q)/std::sqrt(dot(v,v)*dot(q,q));
}
template<>
val_t similarity<measure::inner>(Query::vec_view_t v, Query::vec_view_t q){
    return dot(v,q);
}
auto euclidean_distance_i=[](int64_t i, auto &out, auto const &x, auto const &y){
    auto tmp=x[i]-y[i];
    out += tmp*tmp;
};
template<>
val_t similarity<measure::euclidean>(Query::vec_view_t v, Query::vec_view_t q){
    VecLoop_void<val_t,word_dim> vecloop_void{};
    val_t distance{};
    vecloop_void(euclidean_distance_i, distance, v, q);
    return distance;
}

template<measure T>
void process_query(Query &query, SimilaritySearch::voca_info_t::voca_vecs_t const& voca_vecs){
    auto n=voca_vecs.size();
    assert(n==query.distances.size());
    auto q=query.query_vec.span;
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000), 
                      [&](tbb::blocked_range<decltype(n)> const &r){
        for(decltype(n) i=r.begin(); i!=r.end(); ++i){
            auto const& v=voca_vecs[i];
            query.distances[i] =similarity<T>(v,q);
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
                query.distances[i] =similarity<T>(v,q);
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
json SimilaritySearch::process_queries(json ask) const {
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
    json answer=collect_queries_results(queries, phrase_voca);
    return answer;
}

