#include "parser/parser.h"
#include "parser/wordvec.h"
#include "utils/profiling.h"
#include "utils/parallel.h"
#include "utils/linear_algebra.h"
#include "utils/print.h"

using namespace rnn::wordrep;
using namespace util;
using namespace util::math;

int main(){
    using val_t = double;
    constexpr int word_dim=100;
    constexpr util::DataType w2vmodel_f_type = util::DataType::dp;

    Timer timer{};
    Voca voca = load_voca("wordvec.h5", "1b.training.word_key");
    VocaIndexMap word2idx = voca.indexing();
    auto voca_size = voca.size();
    auto voca_vecs = load_voca_vecs<word_dim>("trained.h5", "1b.training", w2vmodel_f_type);
    timer.here_then_reset("Data loaded.");

    std::string query="The";
    std::string query2="the";
    auto idx=word2idx.getIndex(Word{query});
    auto idx2=word2idx.getIndex(Word{query2});
    timer.here_then_reset("Got index.");
    print(voca_size);
    print(idx);
    print(idx2);
    auto q=voca_vecs[idx];
    auto q2=voca_vecs[idx2];

    std::vector<val_t> distances(voca_size);
    std::vector<val_t> distances2(voca_size);
    // for(decltype(voca_size) i=0; i<voca_size; ++i){
    auto n=voca_size;
    //tbb::parallel_for(decltype(n){0}, n, [&](decltype(n) i){
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000), 
                      [&](tbb::blocked_range<decltype(n)> const &r){
        for(decltype(n) i=r.begin(); i!=r.end(); ++i){
            auto const& v=voca_vecs[i]; 
            distances[i]=dot(q, v);
            distances2[i]=dot(q2, v);
        }
    });
    //}
    timer.here_then_reset("Calculate distances.");
    //std::sort(distances.begin(),distances.end());
    std::partial_sort(distances.begin(),distances.begin()+20,distances.end());
    auto idx_best_match=std::min_element(distances.cbegin(),distances.cend())-distances.cbegin();
    auto idx2_best_match=std::min_element(distances2.cbegin(),distances2.cend())-distances2.cbegin();
    timer.here_then_reset("Query answered.");
    print(voca.getWord(idx_best_match).val);
    print(voca.getWord(idx2_best_match).val);

    return 0;
}