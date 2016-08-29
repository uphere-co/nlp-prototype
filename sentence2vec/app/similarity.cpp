#include "wordrep/sentence2vec.h"

#include "parser/parser.h"
#include "parser/wordvec.h"
#include "utils/profiling.h"
#include "utils/parallel.h"
#include "utils/linear_algebra.h"
#include "utils/print.h"
#include "utils/loop_gen.h"

using namespace sent2vec;
using namespace rnn::wordrep;
using namespace util;
using namespace util::math;
using namespace util::io;

namespace{
using val_t = double;
using idx_t = std::size_t;
//constexpr int word_dim=100; //already declared in sentence2vec.h
constexpr util::DataType w2vmodel_f_type = util::DataType::dp;
}//nameless namespace

struct Query{
    Query(std::string word, Voca const &voca, VocaIndexMap const &word2idx)
    :word{word}, distances(voca.size()),
     idx{word2idx.getIndex(Word{word})} {}    
    std::string word;
    std::vector<val_t> distances;
    idx_t idx;
};

auto process_query=[](Query &query, auto const& voca_vecs){
    auto n=voca_vecs.size();
    assert(n==query.distances.size());
    auto q=voca_vecs[query.idx];
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000), 
                      [&](tbb::blocked_range<decltype(n)> const &r){
        for(decltype(n) i=r.begin(); i!=r.end(); ++i){
            auto const& v=voca_vecs[i];
            query.distances[i] =dot(v,q)/std::sqrt(dot(v,v)*dot(q,q));//sigmoid(v,q);
            // distances2[i]=dot(v,q2)/std::sqrt(dot(v,v)*dot(q2,q2));//sigmoid(v,q2);
        }
    });
};
auto process_queries_simple=[](std::vector<Query> &queries, auto const& voca_vecs){
    for(auto &query:queries) process_query(query, voca_vecs);
};
auto process_queries=[](std::vector<Query> &queries, auto const& voca_vecs){    
    auto n=voca_vecs.size();
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000), 
                      [&](tbb::blocked_range<decltype(n)> const &r){
        for(decltype(n) i=r.begin(); i!=r.end(); ++i){
            for(auto &query:queries){
                auto q=voca_vecs[query.idx];
                auto const& v=voca_vecs[i];
                query.distances[i] =dot(v,q)/std::sqrt(dot(v,v)*dot(q,q));//sigmoid(v,q);
            }            
        }
    });
};
void display_query(Query const &query, Voca const &voca){
    std::vector<idx_t> idxs(query.distances.size());
    idx_t i{0};
    for(auto &x:idxs) x=i++;
    std::partial_sort(idxs.begin(),idxs.begin()+10,idxs.end(),
                      [&](auto i, auto j){return query.distances[i]>query.distances[j];});
    print("------------------\n");
    for(auto it=idxs.begin(); it!=idxs.begin()+10; ++it){
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
    auto voca_vecs = load_voca_vecs<word_dim>("data.1M.h5", "1b.training.1M", w2vmodel_f_type);
    Voca voca = load_voca("data.1M.h5", "1b.training.1M.word");
    // auto voca_vecs = load_voca_vecs<word_dim>("gensim.h5", "1b.training.1M.gensim", w2vmodel_f_type);
    // Voca voca = load_voca("gensim.h5", "1b.training.1M.gensim.word" );
    // auto voca_vecs = load_voca_vecs<word_dim>("data.w2v.h5", "foo.vec", w2vmodel_f_type);
    // Voca voca = load_voca("data.w2v.h5", "foo.word");
    print(voca.size());
    print(":voca size.\n");
    VocaIndexMap word2idx = voca.indexing();
    auto voca_size = voca.size();
    timer.here_then_reset("Data loaded.");

    std::vector<Query> queries;
    queries.emplace_back("Physics",voca, word2idx);
    queries.emplace_back("physics",voca, word2idx);
    queries.emplace_back("Mathematics",voca, word2idx);
    queries.emplace_back("mathematics",voca, word2idx);
    queries.emplace_back("math",voca, word2idx);
    queries.emplace_back("biology",voca, word2idx);
    queries.emplace_back("science",voca, word2idx);
    queries.emplace_back("academic",voca, word2idx);
    timer.here_then_reset("Got index.");    
    process_queries(queries, voca_vecs);
    timer.here_then_reset("Calculate distances.");
    //std::sort(distances.begin(),distances.end());
    display_queries(queries, voca);    
    timer.here_then_reset("Query answered.");
    
    return 0;
}
