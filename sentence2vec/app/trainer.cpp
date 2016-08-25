#include <limits>
#include <random>

#include "parser/parser.h"
#include "parser/wordvec.h"
#include "utils/print.h"
#include "utils/hdf5.h"
#include "utils/math.h"
#include "utils/profiling.h"

/*
-UNKNOWN-
-LPB- (
-RPB- )
*/
namespace sent2vec  {

using char_t = char;
using wcount_t = int32_t;
using idx_t = std::size_t;
using val_t = double;

constexpr int word_dim=100;

auto is_unknown_widx = [](auto x){return x==std::numeric_limits<decltype(x)>::max();};

struct UnigramDist{
    using char_t =  sent2vec::char_t;
    using count_t = sent2vec::wcount_t; 

    UnigramDist(util::io::H5file const &h5store, std::string voca_file, std::string count_file)
    : voca{h5store.getRawData<char_t>(util::io::H5name{voca_file})},
      count{h5store.getRawData<count_t>(util::io::H5name{count_file})},
      prob(count.size()) {
        auto norm = 1.0 / std::accumulate(count.cbegin(), count.cend(), count_t{0});
        for(size_t i=0; i<count.size(); ++i) {
            prob[i]=count[i]*norm;
        }
    }
    val_t get_prob(idx_t idx) const {
        if(is_unknown_widx(idx)) return -1.0;
        return prob[idx];
    } 
    wcount_t get_count(idx_t idx) const{
        if(is_unknown_widx(idx)) return 0;
        return count[idx];
    }
    rnn::wordrep::Voca voca;
    std::vector<count_t> count;
    std::vector<val_t> prob;
};

struct OccurrenceFilter{
    OccurrenceFilter(wcount_t cutoff, UnigramDist const &unigram)
    : unigram{unigram}, cutoff{cutoff} {}
    std::vector<idx_t> operator() (std::vector<idx_t> idxs){
        std::vector<idx_t> filtered;
        for(auto idx:idxs) 
            if(unigram.get_count(idx>cutoff))
                filtered.push_back(idx);
        return filtered;
    }
    UnigramDist const &unigram;
    wcount_t cutoff;
};

template<typename T>
class Sampler{
public:
    Sampler(T beg, T end)
    : rd{}, gen{rd()}, dist{beg, end} {}
    Sampler (Sampler const &sampler)
    : rd{}, gen{rd()}, dist{sampler.dist} {}
    auto operator() () {return dist(gen);}

private:
    std::random_device rd;
    std::mt19937 gen;
    std::discrete_distribution<idx_t> dist;
};

class SubSampler{
public:
    SubSampler(val_t rate, UnigramDist const &unigram)
    : unigram{unigram}, rd{}, gen{rd()}, uni01{0.0,1.0}, rate_inv{1.0/rate} {}
    template<typename TW>
    auto operator() (TW const &widxs) {
        TW widxs_subsampled;
        auto is_sampled=[this](auto p_word){
            if(p_word<0.0) return false;
            //x>1 : typical word
            //x<1 : rare word
            auto x = p_word*rate_inv;
            auto p = (std::sqrt(x)+1)/x;
            if(p < this->uni01(gen)) return false;
            return true;
        };
        for(auto widx : widxs)
            if(is_sampled(unigram.get_prob(widx)))
                widxs_subsampled.push_back(widx);
        return widxs_subsampled;
    }
private:
    UnigramDist const &unigram;
    std::random_device rd;
    std::mt19937 gen;
    std::uniform_real_distribution<> uni01;
    val_t rate_inv;
};


struct WordVecContext{
    using widxs_t = std::vector<idx_t> ;
    using iter_t = widxs_t::const_iterator; 
    WordVecContext(iter_t self, widxs_t const &widxs, 
                   idx_t left, idx_t right)
    : widx{*self} {
        auto beg=widxs.cbegin();
        auto end=widxs.cend();
        assert(self<end);
        auto left_beg = self-left<beg? beg : self-left;
        auto right_end= self+1+right>end? end : self+1+right;
        std::copy(left_beg, self, std::back_inserter(cidxs));
        std::copy(self+1, right_end, std::back_inserter(cidxs));
    }
    idx_t widx;
    widxs_t cidxs;    
};
struct SentVecContext{
    using widxs_t = std::vector<idx_t> ;
    using iter_t = widxs_t::const_iterator; 
    SentVecContext(idx_t sidx, iter_t self, widxs_t const &widxs, 
                   idx_t left, idx_t right)
    : sidx{sidx}, widx{*self} {
        auto beg=widxs.cbegin();
        auto end=widxs.cend();
        assert(self<end);
        auto left_beg = self-left<beg? beg : self-left;
        auto right_end= self+1+right>end? end : self+1+right;
        std::copy(left_beg, self, std::back_inserter(cidxs));
        std::copy(self+1, right_end, std::back_inserter(cidxs));
    }
    idx_t sidx;
    idx_t widx;
    widxs_t cidxs;    
};


class NegativeSampleDist{
public:
    using dist_t = std::vector<val_t>;
    using iter_t = dist_t::const_iterator; 
    NegativeSampleDist(dist_t const &wc_pdf, val_t power)
    :dist{wc_pdf} {
        for(auto &x : dist) x=std::pow(x, power);
    }
    Sampler<iter_t> get_sampler() const {        
        return Sampler<iter_t>{dist.cbegin(), dist.cend()};
    }
private:
    dist_t dist;
};

}//namespace sent2vec




using namespace rnn::simple_model;
using namespace rnn::wordrep;
using namespace util::math;
using namespace util::io;
using namespace util;
using namespace sent2vec;
void test_negative_sampling(){
    H5file file{H5name{"data.h5"}, hdf5::FileMode::read_exist};
    UnigramDist word_dist{file, "1b.short_sents.bar.word_key", "1b.short_sents.word_count"};
    // Sampler<UnigramDist::iter_t> sampler{word_dist.prob.cbegin(),word_dist.prob.cend()};
    // NegativeSampleDist neg_sample_dist{word_dist.prob, 0.0};    
    NegativeSampleDist neg_sample_dist{word_dist.prob, 0.75};
    auto sampler=neg_sample_dist.get_sampler();
    
    std::map<int, int> m;
    for(int n=0; n<100000; ++n) {
        ++m[sampler()];
    }
    std::map<int, int, std::greater<int>> m_inv;
    for(auto const x:m) m_inv[x.second]=x.first;
    for(auto p : m_inv) {
        std::cout << p.second << " " <<word_dist.voca.getWord(p.second).val <<" generated " << p.first << " times\n";
    }
    std::cerr<<"Voca size: "<<word_dist.prob.size()<<std::endl;
}

auto print_word=[](auto widx, auto const &word_dist){
    print(word_dist.voca.getWord(widx).val);
};
auto print_context=[](auto const &context, auto const &word_dist){
    print(word_dist.voca.getWord(context.widx).val);
    print(": ");
    for(auto idx: context.cidxs){
        if(is_unknown_widx(idx))  {print("-UNKNOWN-"); continue;}
        print_word(idx, word_dist);
    }
    print('\n');
};

void test_context_words(){
    H5file file{H5name{"data.h5"}, hdf5::FileMode::read_exist};
    UnigramDist unigram{file, "1b.short_sents.bar.word_key", "1b.short_sents.word_count"};    
    VocaIndexMap word2idx = unigram.voca.indexing();
    
    TokenizedSentences dataset{"testset"};
    auto& lines = dataset.val;
    SubSampler sub_sampler{0.0001, unigram};
    for(size_t sidx=0; sidx<lines.size(); ++sidx){
        auto& sent = lines[sidx];
        auto widxs_orig = word2idx.getIndex(sent);
        auto widxs = sub_sampler(widxs_orig);
        for(auto widx:widxs) assert(!is_unknown_widx(widx));
        for(auto self=widxs.cbegin(); self!=widxs.end(); ++self){
            print_context(SentVecContext{sidx, self, widxs, 5,5}, unigram);
        }
    }

}


template<int word_dim>
WordBlock_base<word_dim> random_WordBlock(idx_t voca_size){
    //constexpr int word_dim = 100;
    std::random_device rd{};
    std::mt19937 gen{rd()};
    std::uniform_real_distribution<val_t> uni01{0.0,1.0};
    std::vector<val_t> wvec_init(voca_size*word_dim);
    for(auto &x : wvec_init) x= uni01(gen)-0.5;
    return WordBlock_base<word_dim>{wvec_init};    
}
void test_voca_update(){
    Timer timer{};
    constexpr int word_dim=100;
    auto voca_size = 100;

    using WordBlock = WordBlock_base<word_dim>;
    WordBlock voca_vecs=random_WordBlock<word_dim>(voca_size);
    auto vec = voca_vecs[0];
    auto vec2 = voca_vecs[1];
    
    print(sum(voca_vecs[0]));
    for(auto &x:vec)x=1.0;
    // for(int i=0; i<100; ++i)voca_vecs[0][i]=2.5;
    print(sum(voca_vecs[0]));
    for(int i=0; i<100; ++i) vec[i]+=2.5; //vec==3.5
    print(sum(voca_vecs[0]));
    print(":sum\n");
    print(dot(vec, vec2));
    for(auto &x:vec2)x=2.0;
    print(dot(vec, vec2));
    voca_vecs.push_back(vec);
    span_1d<val_t,word_dim> vec3 = voca_vecs[100]; //vec3==1
    // vec+=vec2; //vec==3
    for(int i=0; i<100; ++i) vec[i]=2.5; //vec==2.5
    // assert(voca_vecs[0]==vec);
    // voca_vecs[0]+=voca_vecs[1];
    print(dot(vec, vec2));
    print(dot(voca_vecs[0], vec2));
    print(dot(vec3, vec2));
    print(":dot\n");
}

auto sigmoid=[](auto const &x, auto const &y){
    auto xy = util::math::dot(x,y);
    return  1/(1+std::exp(-xy));
};
auto sigmoid_plus=[](auto const &x, auto const &y){
    auto xy = util::math::dot(x,y);
    return  1/(1+std::exp(xy));
};

struct SparseGrad{
    SparseGrad(val_t f_wc, val_t f_wcn,
               idx_t idx_w, idx_t idx_c, idx_t idx_cn)
    : f_wc{f_wc}, f_wcn{f_wcn},
      idx_w{idx_w},idx_c{idx_c}, idx_cn{idx_cn} {}
    val_t f_wc, f_wcn;
    idx_t idx_w, idx_c, idx_cn;
};
//vocavecs_gradient_descent=[]
auto vocavecs_gradient=[](auto const &voca_vecs, 
                     auto idx_w, auto idx_c, auto idx_cn){
    auto w=voca_vecs[idx_w];
    auto c=voca_vecs[idx_c];
    auto cn=voca_vecs[idx_cn];
    // grad_w : (1-sigmoid(w,c)) *c + (sigmoid_plus(w,c)-1) * c_n
    //grad_c : (1-sigmoid(w,c)) * w 
    //grad_cn : (sigmoid_plus(w,c)-1) *w
    return SparseGrad{1-sigmoid(w,c), sigmoid_plus(w,cn)-1, idx_w, idx_c, idx_cn};
};

auto word2vec_grad_w = [](int64_t i, auto x_c, auto x_cn, auto &w, auto const &c, auto const &cn){
    w[i] += x_c * c[i] + x_cn*cn[i];
};
auto word2vec_grad_c = [](int64_t i, auto x, auto &c, auto const &w){
    c[i] += x * w[i];
};

struct VocavecsGradientDescent{
    VocavecsGradientDescent(val_t alpha)
    :alpha{alpha} {}
    template<typename T>
    void operator() (T &voca_vecs, SparseGrad const & grad){
        auto w=voca_vecs[grad.idx_w];
        auto c=voca_vecs[grad.idx_c];
        auto cn=voca_vecs[grad.idx_cn];
        auto x_wc = grad.f_wc *alpha;
        auto x_wcn = grad.f_wcn *alpha;
        vecloop_void(word2vec_grad_w, x_wc, x_wcn, w, c, cn);
        vecloop_void(word2vec_grad_c, x_wc, c, w);
        vecloop_void(word2vec_grad_c, x_wcn, cn, w);
    }
    val_t alpha;
    VecLoop_void<val_t,word_dim> vecloop_void{};
};

void test_word2vec_grad_update(){
    Timer timer{};
    // constexpr util::DataType w2vmodel_f_type = util::DataType::sp;
    constexpr int word_dim=100;

    H5file file{H5name{"data.h5"}, hdf5::FileMode::read_exist};
    UnigramDist unigram{file, "1b.short_sents.bar.word_key", "1b.short_sents.word_count"};
    NegativeSampleDist neg_sample_dist{unigram.prob, 0.75};    
    auto negative_sampler=neg_sample_dist.get_sampler();
    SubSampler sub_sampler{0.0001, unigram};
    OccurrenceFilter freq_filter{5, unigram};

    VocavecsGradientDescent optimizer{0.025};

    VocaIndexMap word2idx = unigram.voca.indexing();
    auto voca_size = unigram.voca.size();

    timer.here_then_reset("UnigramDist constructed");
    WordBlock voca_vecs=random_WordBlock<word_dim>(voca_size);
    std::cerr << "Sum: "<<sum(voca_vecs[10]) << std::endl;
    timer.here_then_reset("Initial WordBlock constructed");

    print(voca_size);
    print(": voca_size\n");
    timer.here_then_reset("Training begins");
    TokenizedSentences dataset{"testset"};
    auto filtered_words=[&word2idx,&freq_filter,&sub_sampler](auto const &sent){
        auto widxs_orig = word2idx.getIndex(sent);
        auto widxs_filtered = freq_filter(widxs_orig);
        auto widxs = sub_sampler(widxs_filtered);
        return widxs;
    };
    auto& lines = dataset.val;    
    // for(auto const &sent:lines){
    auto n=lines.size();
    tbb::parallel_for(decltype(n){0}, n, [&](decltype(n) i){
        auto &sent=lines[i];
        auto widxs = filtered_words(sent);
        for(auto self=widxs.cbegin(); self!=widxs.end(); ++self){
            auto widx = *self;
            WordVecContext c_words{self, widxs, 5,5};
            for(auto cidx: c_words.cidxs) {
                auto cnidx = negative_sampler();
                auto grad = vocavecs_gradient(voca_vecs, widx, cidx, cnidx);
                optimizer(voca_vecs, grad);
                print_word(widx, unigram);
                print_word(cidx, unigram);
                print_word(cnidx,unigram);
                print("\n");
            }
        }
    });
    //}
    timer.here_then_reset("test_word2vec_grad_update() is finished.");
    H5file h5store{H5name{"trained.h5"}, hdf5::FileMode::rw_exist};
    h5store.overwriteRawData(H5name{"testset"}, voca_vecs._val );
    timer.here_then_reset("Wrote word2vecs to disk.");
}

int main(){
    // test_negative_sampling();
    // test_context_words();
    // test_voca_update();
    // test_word2vec_grad_update();
    // return 0;


    Timer timer{};
    // constexpr util::DataType w2vmodel_f_type = util::DataType::sp;
    constexpr int word_dim=100;

    H5file file{H5name{"wordvec.h5"}, hdf5::FileMode::read_exist};
    UnigramDist unigram{file, "1b.training.word_key", "1b.training.word_count"};
    NegativeSampleDist neg_sample_dist{unigram.prob, 0.75};    
    auto negative_sampler=neg_sample_dist.get_sampler();
    SubSampler sub_sampler{0.0001, unigram};
    OccurrenceFilter freq_filter{5, unigram};
    VocavecsGradientDescent optimizer{0.025};
    VocaIndexMap word2idx = unigram.voca.indexing();
    auto voca_size = unigram.voca.size();
    WordBlock voca_vecs=random_WordBlock<word_dim>(voca_size);
    timer.here_then_reset("Initial WordBlock constructed");

    auto filtered_words=[&word2idx,&freq_filter,&sub_sampler](auto const &sent){
        auto widxs_orig = word2idx.getIndex(sent);
        auto widxs_filtered = freq_filter(widxs_orig);
        auto widxs = sub_sampler(widxs_filtered);
        return widxs;
    };
    timer.here_then_reset("Training begins");
    TokenizedSentences dataset{"1b.trainset"};
    auto& lines = dataset.val;
    auto n=lines.size();
    for(int i=0; i<1; ++i){  
    // for(auto const &sent:lines){
    tbb::parallel_for(decltype(n){0}, n, [&](decltype(n) i){
        auto &sent=lines[i];    
        auto widxs = filtered_words(sent);
        for(auto self=widxs.cbegin(); self!=widxs.end(); ++self){
            auto widx = *self;
            WordVecContext c_words{self, widxs, 5,5};
            for(auto cidx: c_words.cidxs) {
                for(int j=0; j<5; ++j){
                auto cnidx = negative_sampler();
                auto grad = vocavecs_gradient(voca_vecs, widx, cidx, cnidx);
                optimizer(voca_vecs, grad);
                }
            }
        }
    });
    }
    timer.here_then_reset("test_word2vec_grad_update() is finished.");
    H5file h5store{H5name{"trained.h5"}, hdf5::FileMode::rw_exist};
    h5store.overwriteRawData(H5name{"1b.training"}, voca_vecs._val );
    timer.here_then_reset("Wrote word2vecs to disk.");

    return 0;
}
