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
using idx_t = std::size_t;
using float_t = double;

auto is_unknown_widx = [](auto x){return x==std::numeric_limits<decltype(x)>::max();};
auto occurence_cutoff = [](auto x){return x>5;};


struct UnigramDist{
    using char_t =  sent2vec::char_t;
    using count_t = sent2vec::wcount_t;
    using prob_t = std::vector<double>;
    using iter_t = prob_t::const_iterator; 

    UnigramDist(util::io::H5file const &h5store, std::string voca_file, std::string count_file)
    : voca{h5store.getRawData<char_t>(util::io::H5name{voca_file})},
      count{h5store.getRawData<count_t>(util::io::H5name{count_file})},
      prob(count.size()) 
    {
        auto norm = 1.0 / std::accumulate(count.cbegin(), count.cend(), count_t{0});
        for(size_t i=0; i<count.size(); ++i) {
            prob[i]=count[i]*norm;
        }
    }
    prob_t get_probs(std::vector<idx_t> idxs){
        prob_t ps;
        for(auto idx:idxs) {
            if(is_unknown_widx(idx)) ps.push_back(-1.0);
            else ps.push_back(prob[idx]);
        }
        return ps;
    } 
    rnn::wordrep::Voca voca;
    std::vector<count_t> count;
    prob_t prob;
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
/*
pw = cnt / pos_max < 1
x = pw/sample 
x>1 : typical 
x<1 : rare
ran = (sqrt(x) + 1) / x;
if(ran < rand_gen_double()) continue;
*/

struct SubSampler{
    SubSampler(float_t rate)
    : rd{}, gen{rd()}, uni01{0.0,1.0}, rate_inv{1.0/rate} {}
    template<typename TW, typename TP>
    auto operator() (TW const &widxs, TP const &probs) {
        TW widxs_subsampled;
        for(decltype(probs.size()) i=0; i<probs.size(); ++i){
            auto p_word = probs[i];
            if(p_word<0.0) continue;
            auto x = p_word*rate_inv;
            auto p = (std::sqrt(x)+1)/x;
            if(p < uni01(gen)) continue;
            widxs_subsampled.push_back(widxs[i]);
        }
        return widxs_subsampled;
    }
    std::random_device rd;
    std::mt19937 gen;
    std::uniform_real_distribution<> uni01;
    float_t rate_inv;
};



struct WordVecContext{
};
struct SentVecContext{
    //assert(idx_self<=widxs.size())
    SentVecContext(idx_t sidx, idx_t idx_self, std::vector<idx_t> widxs, idx_t left, idx_t right)
    : sidx{sidx}, widx{widxs[idx_self]} {
        auto beg=widxs.cbegin();
        auto end=widxs.cend();
        auto self=beg+idx_self;
        auto left_beg = self-left<beg? beg : self-left;
        auto right_end= self+1+right>end? end : self+1+right;
        std::copy(left_beg, self, std::back_inserter(left_widxs));
        std::copy(self+1, right_end, std::back_inserter(right_widxs));
    }
    idx_t sidx;
    idx_t widx;
    std::vector<idx_t> left_widxs;
    std::vector<idx_t> right_widxs;    
};


}//namespace sent2vec




using namespace rnn::simple_model;
using namespace rnn::wordrep;
using namespace util::io;
using namespace util;
using namespace sent2vec;
void test_unigram_sampling(){
    H5file file{H5name{"data.h5"}, hdf5::FileMode::read_exist};
    UnigramDist word_dist{file, "1b.short_sents.bar.word_key", "1b.short_sents.word_count"};    
    Sampler<UnigramDist::iter_t> negative_sampler{word_dist.prob.cbegin(), word_dist.prob.cend()};
    
    std::map<int, int> m;
    for(int n=0; n<10000; ++n) {
        ++m[negative_sampler()];
    }
    for(auto p : m) {
        std::cout << p.first << " " <<word_dist.voca.getWord(p.first).val <<" generated " << p.second << " times\n";
    }
}

auto print_context=[](auto const &context, auto const &word_dist){
    for(auto idx: context.left_widxs){
        if(is_unknown_widx(idx)) {print("-UNKNOWN-"); continue;}
        print(word_dist.voca.getWord(idx).val);
    }
    print("__");
    print(word_dist.voca.getWord(context.widx).val);
    print("__");
    for(auto idx: context.right_widxs){
        if(is_unknown_widx(idx))  {print("-UNKNOWN-"); continue;}
        print(word_dist.voca.getWord(idx).val);
    }
    print('\n');
};

void test_context_words(){
    H5file file{H5name{"data.h5"}, hdf5::FileMode::read_exist};
    UnigramDist word_dist{file, "1b.short_sents.bar.word_key", "1b.short_sents.word_count"};    
    VocaIndexMap word2idx = word_dist.voca.indexing();
    
    TokenizedSentences dataset{"testset"};
    SubSampler sub_sampler{0.0001};
    auto& lines = dataset.val;
    for(size_t sidx=0; sidx<lines.size(); ++sidx){
        auto& sent = lines[sidx];
        auto widxs_orig = word2idx.getIndex(sent);
        auto pws = word_dist.get_probs(widxs_orig);
        auto widxs = sub_sampler(widxs_orig, pws);
        // SentVecContext context{sidx, 2, widxs, 5,5};
        // print_context(context, word_dist);
        if(widxs.size()<5) continue;
        print_context(SentVecContext{sidx, 4, widxs, 5,5}, word_dist);
        // for(auto idx : idxs) {            
        //     if(is_unknown_widx(idx)) continue;
        //     if(!occurence_cutoff(word_dist.count[idx])) continue; 
        //     print(idx);
        //     print(" : ");
        //     print(word_dist.count[idx]);
        // }
        // print('\n');
    }

}

int main(){
    // test_unigram_sampling();
    test_context_words();

    return 0;
}
