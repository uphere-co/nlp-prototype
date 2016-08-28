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
            if(unigram.get_count(idx)>cutoff)
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
    : dist{beg, end} {}
    Sampler (Sampler const &sampler)
    : dist{sampler.dist} {}
    template<typename GEN>
    auto operator() (GEN &gen) {return dist(gen);}

private:
    std::discrete_distribution<idx_t> dist;
};

class Sampler2{
public:
    using dist_t = std::vector<val_t>;
    using iter_t = std::vector<val_t>::const_iterator;
    Sampler2(dist_t const &dist0, idx_t n_cell)
    : dist{dist0}, cdf(dist.size()), idxs(dist.size()), cell(n_cell+1) {
        idx_t i=0;
        for(auto &idx:idxs) idx=i++;
        std::sort(idxs.begin(), idxs.end(), [this](auto x, auto y){return dist[x]>dist[y];});
        std::sort(dist.begin(), dist.end(), std::greater<val_t>{});
        std::partial_sum(dist.cbegin(), dist.cend(), cdf.begin());
        auto sum=cdf.back();
        resolution=sum/n_cell;        
        for(idx_t i=0; i<cell.size(); ++i){
            cell[i]=std::upper_bound(cdf.cbegin(),cdf.cend(), i*resolution);
        }
        cell[n_cell]=cdf.cend();
        ur=std::uniform_real_distribution<val_t>(0,cdf.back());
    }
    auto operator() (val_t ran) const {
        idx_t i_cell = ran/resolution;
        auto i=std::upper_bound(cell[i_cell],cell[i_cell+1], ran)-cdf.cbegin();
        // auto i2=std::upper_bound(cdf.cbegin(), cdf.cend(), ran)-cdf.cbegin();
        // assert(i==i2);
        return idxs[i];
    }
    auto get_pdf() const {return ur;}

private:
    dist_t dist;
    dist_t cdf;
    std::vector<idx_t> idxs;
    std::vector<iter_t> cell;
    std::uniform_real_distribution<val_t> ur;
    val_t resolution;
};

class SubSampler{
public:
    SubSampler(val_t rate, UnigramDist const &unigram)
    : unigram{unigram}, uni01{0.0,1.0}, rate_inv{1.0/rate} {}
    template<typename TW, typename GEN>
    auto operator() (TW const &widxs, GEN &gen) {
        TW widxs_subsampled;
        auto is_sampled=[this,&gen](auto p_word){
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
    std::uniform_real_distribution<val_t> uni01;
    val_t rate_inv;
};

struct NegativeSampleDist{
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
    dist_t dist;
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

    std::random_device rd;
    std::mt19937 gen{rd()};
    
    std::map<int, int> m;
    for(int n=0; n<100000; ++n) {
        ++m[sampler(gen)];
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
    std::random_device rd;
    std::mt19937 gen{rd()};
    
    TokenizedSentences dataset{"testset"};
    auto& lines = dataset.val;
    SubSampler sub_sampler{0.0001, unigram};
    for(size_t sidx=0; sidx<lines.size(); ++sidx){
        auto& sent = lines[sidx];
        auto widxs_orig = word2idx.getIndex(sent);
        auto widxs = sub_sampler(widxs_orig, gen);
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
    auto seed = rd();
    std::uniform_real_distribution<val_t> uni01{0.0,1.0};
    std::vector<val_t> wvec_init(voca_size*word_dim);
    auto n=wvec_init.size();
    // tbb::parallel_for(decltype(n){0}, n, [&](decltype(n) i){
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n), 
    [&](tbb::blocked_range<decltype(n)> const &r){
        std::mt19937 gen{seed+r.begin()};
        for(decltype(n) i=r.begin(); i!=r.end(); ++i)
            wvec_init[i]= uni01(gen)-0.5;
    });
    // for(auto &x : wvec_init) x= uni01(gen)-0.5;
    return WordBlock_base<word_dim>{wvec_init};    
}
void test_voca_update(){
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
auto fma_vec = [](int64_t i, auto &out, auto x, auto const &vec){
    out[i] += x * vec[i];
};
auto symm_fma_vec = [](int64_t i,auto x, auto const &vec1, auto const &vec2){
    auto tmp = vec2[i];
    vec2[i] += x*vec1[i];
    vec1[i] += x*tmp;
    // vec2[i] += x*vec1[i];
    // vec1[i] += x*vec2[i];
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
    auto filtered_words=[&word2idx,&freq_filter,&sub_sampler](auto const &sent, auto &gen){
        auto widxs_orig = word2idx.getIndex(sent);
        auto widxs_filtered = freq_filter(widxs_orig);
        auto widxs = sub_sampler(widxs_filtered, gen);
        return widxs;
    };
    auto& lines = dataset.val;    
    // for(auto const &sent:lines){
    auto n=lines.size();
    tbb::parallel_for(decltype(n){0}, n, [&](decltype(n) i){
        std::random_device rd;
        std::mt19937 gen{rd()};
        auto &sent=lines[i];
        auto widxs = filtered_words(sent, gen);
        for(auto self=widxs.cbegin(); self!=widxs.end(); ++self){
            auto widx = *self;
            WordVecContext c_words{self, widxs, 5,5};
            for(auto cidx: c_words.cidxs) {
                auto cnidx = negative_sampler(gen);
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

void test_sampler(){
    Timer timer{};
    H5file file{H5name{"wordvec.h5"}, hdf5::FileMode::read_exist};
    UnigramDist unigram{file, "1b.short_sents.bar.word_key", "1b.short_sents.word_count"};
    timer.here_then_reset("Voca loaded.");
    NegativeSampleDist neg_sample_dist{unigram.prob, 0.75};    
    auto negative_sampler=neg_sample_dist.get_sampler();
    Sampler2 negative_sampler2{neg_sample_dist.dist, 1000000};
    val_t sum_dist{};
    for(auto x:neg_sample_dist.dist) sum_dist+=x;
    std::cout<<sum_dist<<std::endl;
    std::random_device rd;
    std::mt19937 gen{rd()};
    std::uniform_real_distribution<val_t> ur=negative_sampler2.get_pdf();
    timer.here_then_reset("Loop begins.");
    auto sum=0.0;
    auto n=1000000;
    for(int i=0; i<n; ++i){
        // auto widx = negative_sampler(gen);
        auto widx = negative_sampler2(ur(gen));
        sum += widx;
        // sum += ur(gen);
        // std::cout<<unigram.voca.getWord(widx).val<<std::endl;
    }
    std::cout<<sum/n<<std::endl;
    timer.here_then_reset("Loop ends.");
}

using WordBlock = WordBlock_base<word_dim>;
val_t scoring_context(WordVecContext const &context, 
                     WordBlock const &voca_vecs){
    auto w=voca_vecs[context.widx];
    val_t score{0};
    for(auto cidx: context.cidxs) {
        auto c=voca_vecs[cidx];
        score+=std::log(sigmoid(w,c));
    }
    return score;
}
val_t scoring_words(std::vector<idx_t> const &widxs,
                    WordBlock const &voca_vecs){
    val_t score{0};
    for(auto self=widxs.cbegin(); self!=widxs.end(); ++self){
        WordVecContext context{self, widxs, 5,5};
        score+=scoring_context(context, voca_vecs);
    }
    return score;
}

int main(){
    // test_negative_sampling();
    // test_context_words();
    // test_voca_update();
    // test_word2vec_grad_update();
    // test_sampler();
    // return 0;


    Timer timer{};
    // constexpr util::DataType w2vmodel_f_type = util::DataType::sp;
    constexpr int word_dim=100;
    val_t alpha=0.025;

    H5file file{H5name{"data.1M.h5"}, hdf5::FileMode::read_exist};
    UnigramDist unigram{file, "1b.training.1M.word", "1b.training.1M.count"};
    // UnigramDist unigram{file, "1b.short_sents.bar.word_key", "1b.short_sents.word_count"};
    timer.here_then_reset("Voca loaded.");
    NegativeSampleDist neg_sample_dist{unigram.prob, 0.75};    
    auto negative_sampler=neg_sample_dist.get_sampler();
    const Sampler2 negative_sampler2{neg_sample_dist.dist, 100};
    SubSampler sub_sampler{0.0001, unigram};
    OccurrenceFilter freq_filter{5, unigram};
    VocavecsGradientDescent optimizer{alpha};
    VocaIndexMap word2idx = unigram.voca.indexing();
    timer.here_then_reset("Voca indexed.");
    auto voca_size = unigram.voca.size();
    WordBlock voca_vecs=random_WordBlock<word_dim>(voca_size);
    timer.here_then_reset("Initial WordBlock constructed");

    auto filtered_words=[&word2idx,&freq_filter,&sub_sampler](auto const &widxs_orig, auto &gen){
        auto widxs_filtered = freq_filter(widxs_orig);
        auto widxs = sub_sampler(widxs_filtered, gen);
        return widxs;
    };
    
    TokenizedSentences dataset{"1b.trainset.1M"};
    // TokenizedSentences dataset{"1b.trainset"};
    timer.here_then_reset("Train dataset loaded.");

    auto& lines = dataset.val;
    auto n=lines.size();
    std::vector<std::vector<idx_t>> sent_widxs(lines.size());
    tbb::parallel_for(decltype(n){0}, n, [&](decltype(n) i){    
        sent_widxs[i]=word2idx.getIndex(lines[i]);
    });
    auto scoring_sentence=[&](auto const &widxs_orig){
        std::random_device rd{};        
        std::mt19937 gen{rd()};     
        auto widxs = filtered_words(widxs_orig, gen);
        return scoring_words(widxs, voca_vecs);
    };
    auto scoring_dataset=[&](){return parallel_reducer(sent_widxs.cbegin(),sent_widxs.cend(),
                                                       scoring_sentence, val_t{0});};
    timer.here_then_reset("Initial score");
    timer.here_then_reset("Training begins");
    for(int epoch=0; epoch<10; ++epoch){
        print("Score: ");
        print(scoring_dataset());
        print("\n");

        std::random_device rd;
        auto seed = rd();

        // std::mt19937 gen{seed};
        // for(auto const &sent:lines){
        tbb::parallel_for(decltype(n){0}, n, [&](decltype(n) i){
        // Profiling showed that blocked_range doesn't make remarkable speedup.
        // tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,1000), 
        // [&](tbb::blocked_range<decltype(n)> const &r){
            // for(decltype(n) i=r.begin(); i!=r.end(); ++i){
            // auto widxs_orig= word2idx.getIndex(lines[i]);
            VecLoop_void<val_t,word_dim> vecloop_void{};
            auto ur=negative_sampler2.get_pdf();

            auto &widxs_orig=sent_widxs[i];            
            std::mt19937 gen{seed+i};
            auto widxs = filtered_words(widxs_orig, gen);      
            for(auto self=widxs.cbegin(); self!=widxs.end(); ++self){
                auto widx = *self;
                WordVecContext c_words{self, widxs, 5,5};
                auto w=voca_vecs[widx];
                for(auto cidx: c_words.cidxs) {
                    auto c=voca_vecs[cidx];
                    auto x_wc = alpha*(1-sigmoid(w,c));
                    vecloop_void(symm_fma_vec, x_wc, w, c);
                    // vecloop_void(fma_vec, c, x_wc, w);
                    // vecloop_void(fma_vec, w, x_wc, c);
                    assert(x_wc==x_wc);
                    assert(w[0]==w[0]);
                    assert(c[0]==c[0]);
                    // std::cout<<lines[i]<<std::endl;
                    for(int j=0; j<1; ++j){
                        // auto cnidx = ((int)ur(gen)+i)%voca_size;
                        auto cnidx=negative_sampler2(ur(gen));
                        // auto cnidx = negative_sampler(gen);
                        auto cn=voca_vecs[cnidx];
                        //grad_w : (1-sigmoid(w,c)) *c + (sigmoid_plus(w,c)-1) * c_n
                        //grad_c : (1-sigmoid(w,c)) * w 
                        //grad_cn : (sigmoid_plus(w,c)-1) *w
                        auto x_wcn = alpha*(sigmoid_plus(w,cn)-1);
                        assert(x_wcn==x_wcn);
                        vecloop_void(symm_fma_vec, x_wcn, w, cn);
                        // vecloop_void(fma_vec, w, x_wcn, cn);
                        // vecloop_void(fma_vec, cn, x_wcn, w);
                        // print_word(cnidx, unigram);
                        assert(!is_unknown_widx(cnidx));
                        assert(!is_unknown_widx(widx));
                        assert(cn[0]==cn[0]);
                    }
                }
            }
        // }
        // }
        });
        alpha *= 0.8;
    }
    timer.here_then_reset("test_word2vec_grad_update() is finished.");
    H5file h5store{H5name{"trained.h5"}, hdf5::FileMode::rw_exist};
    h5store.overwriteRawData(H5name{"1b.training.1M"}, voca_vecs._val );
    // h5store.overwriteRawData(H5name{"1b.training.100k"}, voca_vecs._val );
    timer.here_then_reset("Wrote word2vecs to disk.");

    return 0;
}

