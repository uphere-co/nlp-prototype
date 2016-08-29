#include "wordrep/sentence2vec.h"

#include "parser/parser.h"
#include "parser/wordvec.h"
#include "utils/profiling.h"
#include "utils/parallel.h"
#include "utils/linear_algebra.h"
#include "utils/print.h"

using namespace sent2vec;
using namespace rnn::wordrep;
using namespace util;
using namespace util::math;
using namespace util::io;


int main(){
    using val_t = double;
    using idx_t = std::size_t;
    constexpr int word_dim=100;
    constexpr util::DataType w2vmodel_f_type = util::DataType::dp;

    H5file file{H5name{"data.1M.h5"}, hdf5::FileMode::read_exist};
    UnigramDist unigram{file, "1b.training.1M.word", "1b.training.1M.count"};

    Timer timer{};
    Voca voca = load_voca("data.1M.h5", "1b.training.1M.word");
    VocaIndexMap word2idx = voca.indexing();
    auto voca_size = voca.size();
    auto voca_vecs = load_voca_vecs<word_dim>("trained.h5", "1b.training.1M", w2vmodel_f_type);
    timer.here_then_reset("Data loaded.");

    std::string query="Physics";
    std::string query2="physics";
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
            distances[i] =sigmoid(v,q);
            distances2[i]=sigmoid(v,q2);
            if(unigram.count[i]<5) {
                distances[i] -= 1000000.0;
                distances2[i]-= 1000000.0;
            }
        }
    });
    //}
    timer.here_then_reset("Calculate distances.");
    //std::sort(distances.begin(),distances.end());
    std::vector<idx_t> idxs(voca_size);
    idx_t i{0};
    for(auto &x:idxs) x=i++;
    std::partial_sort(idxs.begin(),idxs.begin()+20,idxs.end(),
                      [&](auto i, auto j){return distances[i]<distances[j];});
    for(auto it=idxs.begin(); it!=idxs.begin()+20; ++it)
        print(voca.getWord(*it).val);
    print("\n");
    std::partial_sort(idxs.begin(),idxs.begin()+20,idxs.end(),
                      [&](auto i, auto j){return distances2[i]<distances2[j];});
    for(auto it=idxs.begin(); it!=idxs.begin()+20; ++it)
        print(voca.getWord(*it).val);
    print("\n");
    timer.here_then_reset("Query answered.");
    
    return 0;
}