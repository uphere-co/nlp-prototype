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
void display_query(Query const &query, Voca const &voca){
    auto n_top = 20;
    std::vector<idx_t> idxs(query.distances.size());
    idx_t i{0};
    for(auto &x:idxs) x=i++;
    std::partial_sort(idxs.begin(),idxs.begin()+n_top,idxs.end(),
                      [&](auto i, auto j){return query.distances[i]>query.distances[j];});
    print("------------------\n");
    for(auto it=idxs.begin(); it!=idxs.begin()+n_top; ++it){
        print(voca.getWord(*it).val);
        print(query.distances[*it]);
        print("\n");
    }
}
void display_queries(std::vector<Query> const &queries, Voca const &voca){
    for(auto &query:queries) display_query(query, voca);
}

void KLdistance(){
    // pa||pb == pa[i] log(pb[i]/pa[i]) 
    VecLoop_void<val_t,word_dim> vecloop_void{};
}

int main(){
    Timer timer{};
    std::ifstream jsonData("/data/groups/uphere/test.json", std::ifstream::in);
    json j;

    if(jsonData.is_open()) {
        jsonData >> j;
    }

    std::string phrase_store = j["phrase_store"];
    std::string phrase_vec = j["phrase_vec"];
    std::string phrase_word = j["phrase_word"];
    std::string rnn_param_store = j["rnn_param_store"];
    std::string rnn_param_uid = j["rnn_param_uid"];
    std::string wordvec_store = j["wordvec_store"];
    std::string voca_name = j["voca_name"];
    std::string w2vmodel_name = j["w2vmodel_name"];        
    
    rnn::simple_model::TokenizedSentences dataset{"1b.trainset.1M"};
    auto& sents = dataset.val;
    auto n_sent=sents.size();    
    // auto sent_vecs = load_voca_vecs<word_dim>("data.1M.h5", "1b.training.1M.sentvec", w2vmodel_f_type);
    // auto voca_vecs = load_voca_vecs<word_dim>("data.1M.h5", "1b.training.1M", w2vmodel_f_type);
    // Voca voca = load_voca("data.1M.h5", "1b.training.1M.word");
    auto sent_vecs = load_voca_vecs<word_dim>(phrase_store, phrase_vec, w2vmodel_f_type);
    Voca voca = load_voca(phrase_store, phrase_word);
    // auto sent_vecs = load_voca_vecs<word_dim>("phrases.h5", "wsj.s2010.train.vecs", w2vmodel_f_type);
    // Voca voca = load_voca("phrases.h5", "wsj.s2010.train.words");
    // auto voca_vecs = load_voca_vecs<word_dim>("gensim.h5", "1b.training.1M.gensim", w2vmodel_f_type);
    // Voca voca = load_voca("gensim.h5", "1b.training.1M.gensim.word" );
    // auto voca_vecs = load_voca_vecs<word_dim>("data.w2v.h5", "foo.vec", w2vmodel_f_type);
    // Voca voca = load_voca("data.w2v.h5", "foo.word");
    print(voca.size());
    print(":voca size.\n");
    VocaIndexMap word2idx = voca.indexing();
    auto voca_size = voca.size();
    timer.here_then_reset("Data loaded.");
    auto param = load_param(rnn_param_store, rnn_param_uid, util::DataType::dp);
    timer.here_then_reset("Param loaded.");

    //auto line="spokesman declined to comment";
    auto line=j["queries"][0];
    
    VocaInfo rnn{wordvec_store, voca_name, w2vmodel_name, util::DataType::dp};
    auto init_nodes = rnn.initialize_tree(line);
    DPtable table=dp_merging(param, init_nodes);
    auto phrases = table.get_phrases();
//    auto root_node=table.root_node();

    std::vector<Query> queries;
    for(auto const &phrase:phrases){
        auto parsed_tree_str = phrase->name.val;
//        print(parsed_tree_str);
//        print("\n");
        queries.emplace_back(parsed_tree_str, phrase->vec.span, voca);
    }
    // queries.emplace_back("(Donaldson (Lufkin (would (n't (comment .)))))",voca, word2idx);
    // queries.emplace_back("(would (n't (comment .)))",voca, word2idx);
    // queries.emplace_back("((((Donaldson Lufkin) would) (n't comment)) .)",voca, word2idx);
    // queries.emplace_back("(n't comment)",voca, word2idx);
    // queries.emplace_back("((((It belonged) to) (her grandfather)) .)",voca, word2idx);
    // queries.emplace_back("(((((Opponents do) n't) buy) (such arguments)) .)",voca, word2idx);
    // queries.emplace_back("(such arguments)",voca, word2idx);
    // queries.emplace_back("(such arguments)",voca, word2idx);

//    queries.emplace_back("(spokesman (declined (to comment)))",voca, word2idx);
//    queries.emplace_back("(declined (to comment))",voca, word2idx);
//    queries.emplace_back("(no comment)",voca, word2idx);

    // queries.emplace_back("(no comment)",voca, word2idx);
    // queries.emplace_back("(had (no comment))",voca, word2idx);
    // queries.emplace_back("((had (no comment)) .)",voca, word2idx);
    // queries.emplace_back("((A (Shearson spokesman)) ((had (no comment)) .))",voca, word2idx);
    // queries.emplace_back("(Hess ((declined (*-1 (to comment))) .))",voca, word2idx);

    // queries.emplace_back("Physics",voca, word2idx);
    // queries.emplace_back("physics",voca, word2idx);
    // queries.emplace_back("Mathematics",voca, word2idx);
    // queries.emplace_back("mathematics",voca, word2idx);
    // queries.emplace_back("math",voca, word2idx);
    // queries.emplace_back("biology",voca, word2idx);
    // queries.emplace_back("science",voca, word2idx);
    // queries.emplace_back("academic",voca, word2idx);
    timer.here_then_reset("Got index.");    
    // process_queries_euclidean(queries, sent_vecs);
//    process_queries_angle(queries, sent_vecs);
    process_queries_innerdot(queries, sent_vecs);
    timer.here_then_reset("Calculate distances.");
    //std::sort(distances.begin(),distances.end());
    display_queries(queries, voca);
    timer.here_then_reset("Queries answered.");

    // Query sent_query{sents[0],0, n_sent};
    // process_query(sent_query, sent_vecs);
    // display_query(sent_query, sents);
    timer.here_then_reset("Sentence query answered.");
    
    return 0;
}
