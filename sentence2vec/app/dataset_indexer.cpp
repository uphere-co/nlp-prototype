#include <limits>
#include <random>

#include "parser/parser.h"
#include "utils/print.h"
#include "utils/hdf5.h"

/*
-UNKNOWN-
-LPB- (
-RPB- )
*/
namespace sent2vec  {

using char_t = char;
using wcount_t = int32_t;

}

auto is_unknown_widx = [](auto x){return x==std::numeric_limits<decltype(x)>::max();};
auto occurence_cutoff = [](auto x){return x>5;};

struct UnigramDist{
    using char_t =  sent2vec::char_t;
    using count_t = sent2vec::wcount_t;
    using iter_t = std::vector<double>::const_iterator; 

    UnigramDist(util::io::H5file const &h5store, std::string voca_file, std::string count_file)
    : voca{h5store.getRawData<char_t>(util::io::H5name{voca_file})},
      count{h5store.getRawData<count_t>(util::io::H5name{count_file})},
      prob(count.size()) 
    {
        auto norm = 1.0 / std::accumulate(count.cbegin(), count.cend(), count_t{0});
        for(size_t i=0; i<count.size(); ++i) prob[i]=count[i]*norm;
    }
    rnn::wordrep::Voca voca;
    std::vector<count_t> count;
    std::vector<double> prob;
};

template<typename T>
struct Sampler{
    Sampler(T beg, T end)
    : rd{}, gen{rd()}, dist{beg, end} {}
    auto operator() () {return dist(gen);}

    std::random_device rd;
    std::mt19937 gen;
    std::discrete_distribution<> dist;
};

int main(){
    using namespace rnn::simple_model;
    using namespace rnn::wordrep;
    using namespace util::io;
    using namespace util;
    using namespace sent2vec;

    H5file file{H5name{"data.h5"}, hdf5::FileMode::read_exist};
    UnigramDist word_dist{file, "1b.short_sents.bar.word_key", "1b.short_sents.word_count"};
    
    VocaIndexMap word2idx = word_dist.voca.indexing();
    
    TokenizedSentences dataset{"testset"};
    auto& lines = dataset.val;
    for(size_t sidx=0; sidx<lines.size(); ++sidx){
        auto& sent = lines[sidx];
        auto idxs = word2idx.getIndex(sent);

        for(auto idx : idxs) {            
            if(is_unknown_widx(idx)) continue;
            if(!occurence_cutoff(word_dist.count[idx])) continue; 
            print(idx);
            print(" : ");
            print(word_dist.count[idx]);
        }
        print('\n');
    }

    Sampler<UnigramDist::iter_t> negative_sampler{word_dist.prob.cbegin(), word_dist.prob.cend()};

    

    std::map<int, int> m;
    for(int n=0; n<10000; ++n) {
        ++m[negative_sampler()];
    }
    for(auto p : m) {
        std::cout << p.first << " " <<word_dist.voca.getWord(p.first).val <<" generated " << p.second << " times\n";
    }

    return 0;
}
