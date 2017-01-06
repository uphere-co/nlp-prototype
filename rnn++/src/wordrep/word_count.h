#pragma once

#include "wordrep/word_iter.h"

#include "utils/parallel_algorithm.h"
#include "utils/string.h"

namespace wordrep{

template<typename T>
class WordCounter{
public:
    using WordIter=wordrep::WordIterBase<T>;
    //using WordIter=wordrep::WordIterBase<wordrep::WordUID>;
//using WordIter=WordIterBase<std::string>;

    using key_type   = typename WordIter::key_type;
    using mapped_type = size_t;
    using count_type = std::map<key_type, mapped_type>;
    using map_t = tbb::concurrent_hash_map<key_type,mapped_type,util::TBBHashCompare<key_type>>;

    std::map<key_type,mapped_type>
    to_map() const { return util::to_map(wcs); };
    std::vector<std::pair<key_type,mapped_type>>
    to_pairs() const { return util::to_sorted_pairs(wcs);};

    auto count(std::istream&& is){
        tbb::task_group g;
        while (auto buffer=util::string::read_chunk(is, 200000)) {
            //auto ptr = std::make_unique<std::vector<char>>(buffer.value());
            std::string str{buffer.value().data()};
            g.run([this,str{std::move(str)}]() { //important to copy the str variable.
                count(str);
            });
        }
        g.wait();
        return to_pairs();
//    return counter.to_map();
    }

private:
    void count(std::string str){
        WordIter text{std::move(str)};
        count_type word_counts;
        text.iter([&word_counts](auto& word) {++word_counts[word];});
        for(auto const& elm : word_counts){
            typename map_t::accessor a;
            wcs.insert(a, elm.first);
            a->second += elm.second;
        }
    }

    map_t wcs;
};


}//namespace wordrep

