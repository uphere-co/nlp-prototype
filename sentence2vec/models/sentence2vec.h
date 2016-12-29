#pragma once
#include <string>
#include <vector>
#include <limits>
#include <random>

#include "parser/parser.h"
#include "parser/wordvec.h"

#include "wordrep/voca.h"

#include "utils/parallel.h"
#include "utils/hdf5.h"
#include "utils/math.h"
#include "utils/loop_gen.h"

namespace sent2vec  {

using char_t = char;
using wcount_t = int32_t;
using idx_t = std::size_t;
using val_t = double;

constexpr int word_dim=100;

struct UnigramDist [[deprecated]] {
    using count_t = sent2vec::wcount_t; 

    UnigramDist(util::io::H5file const &h5store, 
                std::string voca_file, std::string count_file);
    val_t    get_prob (wordrep::VocaIndex idx) const;

    wordrep::VocaIndexMap voca;
    std::vector<count_t> count;
    std::vector<val_t> prob;
};

/*
struct OccurrenceFilter{
    OccurrenceFilter(wcount_t cutoff, UnigramDist const &unigram);
    std::vector<idx_t> operator()  (std::vector<idx_t> idxs);

    UnigramDist const &unigram;
    wcount_t cutoff;
};
*/

template<typename T>
class Sampler [[deprecated]]{
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

class Sampler2 [[deprecated]]{
public:
    using dist_t = std::vector<val_t>;
    using iter_t = dist_t::const_iterator;
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

class SubSampler [[deprecated]]{
public:
    SubSampler(val_t rate, UnigramDist const &unigram)
    : unigram{unigram}, rate_inv{1.0/rate} {}
    auto operator() (std::vector<idx_t> const &widxs, val_t ran) const {
        std::vector<idx_t>  widxs_subsampled;
        auto is_sampled=[this](auto p_word,auto ran){
            if(p_word<0.0) return false;
            //x>1 : typical word
            //x<1 : rare word
            auto x = p_word*rate_inv;
            auto p = (std::sqrt(x)+1)/x;
            if(p < ran) return false;
            return true;
        };
        for(auto widx : widxs)
            if(is_sampled(unigram.get_prob(widx), ran))
                widxs_subsampled.push_back(widx);
        return widxs_subsampled;
    }
private:
    UnigramDist const &unigram;
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

struct WordVecContext [[deprecated]] {
    using widxs_t = std::vector<idx_t> ;
    using iter_t = widxs_t::const_iterator; 
    WordVecContext(iter_t self, widxs_t const &widxs, 
                   idx_t left, idx_t right);
    idx_t widx;
    widxs_t cidxs;    
};
struct SentVecContext{
    using widxs_t = std::vector<idx_t> ;
    using iter_t = widxs_t::const_iterator; 
    SentVecContext(idx_t sidx, iter_t self, widxs_t const &widxs, 
                   idx_t left, idx_t right);
    idx_t sidx;
    idx_t widx;
    widxs_t cidxs;    
};


template<int word_dim>
rnn::wordrep::WordBlock_base<word_dim> random_WordBlock(idx_t voca_size){
    //constexpr int word_dim = 100;
    std::random_device rd{};
    auto seed = rd();
    std::uniform_real_distribution<val_t> uni{-0.5,0.5};
    std::vector<val_t> wvec_init(voca_size*word_dim);
    auto n=wvec_init.size();
    // tbb::parallel_for(decltype(n){0}, n, [&](decltype(n) i){
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n), 
    [&](tbb::blocked_range<decltype(n)> const &r){
        std::mt19937 gen{seed+r.begin()};
        for(decltype(n) i=r.begin(); i!=r.end(); ++i)
            wvec_init[i]= uni(gen);
    });
    return rnn::wordrep::WordBlock_base<word_dim>{wvec_init};    
}
template<int word_dim>
rnn::wordrep::WordBlock_base<word_dim> init_WordBlock(idx_t voca_size, val_t val){
    std::vector<val_t> wvec_init(voca_size*word_dim, val);
    return rnn::wordrep::WordBlock_base<word_dim>{wvec_init};    
}

auto sigmoid=[](auto const &x, auto const &y){
    using util::math::dot;
    auto xy = dot(x,y);
    return  1/(1+std::exp(-xy));
};
auto sigmoid_plus=[](auto const &x, auto const &y){
    using util::math::dot;
    auto xy = dot(x,y);
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
};

auto accum_adagrad_factor = [](int64_t i, auto &out, auto x, auto const &vec){
    out[i] += x * vec[i]*vec[i];
};
auto adagrad_update = [](int64_t i, auto &out, auto x, auto const &grad, auto const &adagrad_factor){
    out[i] += x *grad[i]/std::sqrt(adagrad_factor[i]);
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
    util::math::VecLoop_void<val_t,word_dim> vecloop_void{};
};


val_t scoring_context(WordVecContext const &context, 
                     rnn::wordrep::WordBlock_base<word_dim> const &voca_vecs);
val_t scoring_words(std::vector<idx_t> const &widxs,
                    rnn::wordrep::WordBlock_base<word_dim> const &voca_vecs);


}//namespace sent2vec