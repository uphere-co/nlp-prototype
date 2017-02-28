#pragma once

#include "wordrep/voca_info.h"

#include "utils/parallel.h"

namespace wordrep{

template<typename TV>
struct Distances{
    Distances() : val{} {}
    Distances(std::size_t n) : val(n) {}
    Distances(std::vector<TV> const &distances)
            : val{distances} {}
    Distances& operator=(Distances const &obj){
        val = std::move(obj.val);
        return *this;
    }
    TV& operator[](VocaIndex vidx) {return val[vidx.val];}
    TV operator[](VocaIndex vidx) const {return val[vidx.val];}
    std::vector<TV> val;
};

class WordSimCache{
public:
    using voca_info_t  = VocaInfo;
    using val_t        = voca_info_t::val_t;
    using dist_cache_t = Distances<val_t>;
    using data_t = tbb::concurrent_hash_map<VocaIndex, dist_cache_t,util::TBBHashCompare<VocaIndex>>;

    struct WordSimOp{
        WordSimOp(WordSimCache& cache)
                : cache{&cache}
        {}
        WordSimCache::val_t score(VocaIndex vidx1, VocaIndex vidx2) const {
            auto it = lookup_cache.find(vidx1);
            assert(it!=lookup_cache.end());
            if(it!=lookup_cache.end())
                return (*it->second)[vidx2];
            return cache->try_find(vidx1)[vidx2];
        }
        WordSimCache::val_t operator()(VocaIndex vidx1, VocaIndex vidx2) const {
            return score(vidx1, vidx2);
        }
        void build_lookup_cache(VocaIndex const& vidx) {
            lookup_cache[vidx]=&cache->try_find(vidx);
        }
        void build_lookup_cache(std::vector<VocaIndex> const& vidxs) {
            for(auto vidx : vidxs)
                build_lookup_cache(vidx);
        }
        WordSimCache* cache;
        std::map<VocaIndex,WordSimCache::dist_cache_t const*> lookup_cache;
    };

    WordSimCache(voca_info_t const &voca);
    void cache(std::vector<VocaIndex> const &words);
    const dist_cache_t& distances(VocaIndex widx) const;
    val_t max_similarity(VocaIndex widx) const;
    auto size() const {return distance_caches.size();}

    WordSimOp get_cached_operator() {
        return WordSimOp(*this);
    }
    bool find(VocaIndex idx) const;
    const dist_cache_t& try_find(VocaIndex idx);
    voca_info_t const &voca;
private:
    bool insert(VocaIndex idx, dist_cache_t const &dists);
    data_t distance_caches;
};

}//namespace wordrep;