#pragma once

#include <map>
#include <vector>
#include <memory>
#include <algorithm>

#include "wordrep/indexes.h"
#include "wordrep/words.h"
#include "wordrep/sentence.h"

namespace wordrep {
namespace wiki {

struct Entity {
    Entity(WikidataUID uid, Words words)
            : uid{uid}, words{std::move(words)} {}

    Entity(WikidataUID uid, std::vector<WordUID> words)
            : uid{uid}, words{std::move(words)} {}

    Entity(WordUIDindex const &wordUIDs, std::string line);

    std::string repr(WikidataUIDindex const &wikidataUIDs,
                     WordUIDindex const &wordUIDs) const;

    size_t size() const { return words.size(); }

    friend bool operator<(Entity const &a, Entity const &b) {
        return a.words.uids > b.words.uids;
    }

    friend std::ostream &operator<<(std::ostream &os, Entity const &a);

    WikidataUID uid;
    Words words;
};

std::ostream &operator<<(std::ostream &os, Entity const &a);


struct UIDSortedEntities{
    struct Binary{
        std::string filename;
    };
    UIDSortedEntities(std::vector<Entity> const& items)
    : entities{std::make_unique<tbb::concurrent_vector<Entity>>()} {
        entities->reserve(items.size());
        for(auto& item : items) entities->push_back(item);
        tbb::parallel_sort(entities->begin(), entities->end(), [](auto x, auto y){return x.uid<y.uid;});
    }
    UIDSortedEntities(tbb::concurrent_vector<Entity> items)
            :  entities{std::make_unique<tbb::concurrent_vector<Entity>>(std::move(items))} {
        tbb::parallel_sort(entities->begin(), entities->end(), [](auto x, auto y){return x.uid<y.uid;});
    }
    UIDSortedEntities(std::unique_ptr<tbb::concurrent_vector<Entity>> entities)
            :  entities{std::move(entities)}
    {}
    auto cbegin() const {return entities->cbegin();}
    auto cend() const {return entities->cend();}
    auto begin() const {return entities->cbegin();}
    auto end() const {return entities->cend();}
    void to_file(Binary file) const;

private:
    std::unique_ptr<tbb::concurrent_vector<Entity>> entities;
};
std::unique_ptr<tbb::concurrent_vector<Entity>> read_binary_file(UIDSortedEntities::Binary file);


struct SortedEntities{
    struct Binary{
        std::string filename;
    };
    SortedEntities(Binary file);
    SortedEntities(std::vector<Entity> items) {
        entities.reserve(items.size());
        for(auto& item : items) entities.push_back(item);
        tbb::parallel_sort(entities.begin(), entities.end());
    }
    SortedEntities(tbb::concurrent_vector<Entity>&& items)
    :  entities{std::move(items)}
    {}
    auto cbegin() const {return entities.cbegin();}
    auto cend() const {return entities.cend();}
    auto begin() const {return entities.begin();}
    auto end() const {return entities.end();}
    void to_file(Binary file) const;
    UIDSortedEntities to_uid_sorted() const{
        return {entities};
    }

private:
    tbb::concurrent_vector<Entity> entities;
};



struct AmbiguousUID {
    bool is_match(WikidataUID uid) const {
        return std::find(candidates.begin(),candidates.end(), uid)!=candidates.end();
    }
    std::vector<WikidataUID> candidates;
};

struct AmbiguousEntity{
    size_t offset;
    size_t len;
    AmbiguousUID uid;

    ConsecutiveTokens map_to_sent(Sentence const& sent) const {
        return {sent.front()+offset, len};
    }
    friend bool operator==(AmbiguousEntity const& x, AmbiguousEntity const& y){
        for(auto& xx : x.uid.candidates )
            for(auto& yy : y.uid.candidates )
                if(xx==yy) return true;
        return false;
    }
    friend bool operator!=(AmbiguousEntity const& x, AmbiguousEntity const& y){
        return !(x==y);
    }
};

struct Synonyms{
    WikidataUID uid;
    std::vector<Words> reprs;
};

}//namespace wordrep::wiki
}//namespace wordrep
