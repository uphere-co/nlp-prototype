#include "wordrep/wordsim_cache.h"

#include "utils/linear_algebra.h"
#include "utils/loop_gen.h"

namespace wordrep{
namespace similarity{

enum class measure{
    angle,
    inner,
    euclidean,
};

template<measure M>
struct Similarity{
    template<typename T, int64_t dim>
    T operator() (util::span_1d<T,dim> v, util::span_1d<T,dim> q) const;
};
template<>
struct Similarity<measure::angle>{
    template<typename T, int64_t dim>
    T operator() (util::span_1d<T,dim> v, util::span_1d<T,dim> q) const{
        using namespace util::math;
        return dot(v,q)/std::sqrt(dot(v,v)*dot(q,q));
    }
};
template<>
struct Similarity<measure::inner>{
    template<typename T, int64_t dim>
    T operator() (util::span_1d<T,dim> v, util::span_1d<T,dim> q) const{
        using namespace util::math;
        return dot(v,q);
    }
};
template<>
struct Similarity<measure::euclidean>{
    template<typename T, int64_t dim>
    T operator() (util::span_1d<T,dim> v, util::span_1d<T,dim> q) const{
        using namespace util::math;
        VecLoop_void<T,dim> vecloop_void{};
        T distance{};
        vecloop_void([](int64_t i, auto &out, auto const &x, auto const &y) {
                         auto tmp = x[i] - y[i];
                         out += tmp * tmp;
                     },
                     distance, v, q);
        return distance;
    }
};

}//namespace wordrep::similarity

WordSimCache::WordSimCache(voca_info_t const &voca) : voca{voca} {
    auto n= voca.wvecs.size();
    data_t::accessor a;
    distance_caches.insert(a, VocaIndex{});
    a->second = dist_cache_t{n};//For unknown word
}

bool WordSimCache::find(VocaIndex idx) const{
    data_t::const_accessor a;
    return distance_caches.find(a, idx);
}

const WordSimCache::dist_cache_t& WordSimCache::try_find(VocaIndex idx){
    if(!find(idx)) cache({idx});
    return distances(idx);
}
bool WordSimCache::insert(VocaIndex idx, dist_cache_t const &dist){
    data_t::accessor a;
    distance_caches.find(a, idx);
    if(distance_caches.find(a, idx)) return false;
    distance_caches.insert(a, idx);
    a->second = dist;
    return true;
}
const WordSimCache::dist_cache_t& WordSimCache::distances(VocaIndex widx) const {
    data_t::const_accessor a;
    bool is_exist=distance_caches.find(a,widx);
    //TODO:     make cache private method and remove this assert.
    if(!is_exist) assert(0);
    return a->second;
}
void WordSimCache::cache(std::vector<VocaIndex> const &words) {
    auto n= voca.wvecs.size();
    std::vector<VocaIndex> words_to_cache;
    std::vector<dist_cache_t> dists;
    for(auto vidx : words) {
        if(find(vidx)) continue;
        words_to_cache.push_back(vidx);
        dists.push_back(dist_cache_t{n});
    }
    auto n_words = dists.size();

    auto dist_measure = similarity::Similarity<similarity::measure::angle>{};
    tbb::parallel_for(tbb::blocked_range<decltype(n)>(0,n,10000),
                      [&](tbb::blocked_range<decltype(n)> const &r){
                          for(decltype(n) i=r.begin(); i!=r.end(); ++i){
                              for(decltype(n_words)j=0; j!=n_words; ++j ){
                                  auto qidx = words_to_cache[j];
                                  auto q = voca.wvecs[qidx];
                                  auto widx = VocaIndex::from_unsigned(i);
                                  dists[j][widx] = dist_measure(voca.wvecs[widx], q);
                              }
                          }
                      });

    for(decltype(n_words)i=0; i!=n_words; ++i){
        auto vidx=words_to_cache[i];
        insert(vidx,dists[i]);
    }
}

WordSimCache::val_t WordSimCache::max_similarity(VocaIndex widx) const{
    if(!find(widx)) return 0.0;
    auto dists = distances(widx);
    auto beg = dists.val.begin();
    std::partial_sort(beg, beg+2, dists.val.end(), std::greater<val_t>{});
    auto it = beg+1;
    return *it;
}

}//namespace wordrep
