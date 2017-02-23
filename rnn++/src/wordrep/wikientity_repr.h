#pragma once

#include <map>
#include <vector>

#include "wordrep/indexes.h"
#include "wordrep/words.h"

namespace wordrep{
namespace wiki{

struct Entity{
    Entity(WikidataUID uid, Words words)
            : uid{uid}, words{std::move(words)}
    {}
    Entity(WikidataUID uid, std::vector<WordUID> words)
            : uid{uid}, words{std::move(words)}
    {}
    Entity(WordUIDindex const& wordUIDs, std::string line);

    std::string repr(WikidataUIDindex const& wikidataUIDs,
                     WordUIDindex const& wordUIDs) const;
    size_t size() const{return words.size();}

    friend bool operator< (Entity const& a, Entity const& b){
        return a.words.uids > b.words.uids;
    }
    friend std::ostream& operator<< (std::ostream& os, Entity const& a);

    WikidataUID uid;
    Words words;
};
std::ostream& operator<< (std::ostream& os, Entity const& a);

struct Synonyms{
    WikidataUID uid;
    std::vector<Words> reprs;
};


struct AmbiguousEntity{
    size_t offset;
    size_t len;
    std::vector<WikidataUID> uids;

    ConsecutiveTokens map_to_sent(Sentence const& sent) const {
        return {sent.front()+offset, len};
    }
    friend bool operator==(AmbiguousEntity const& x, AmbiguousEntity const& y){
        for(auto& xx : x.uids )
            for(auto& yy : y.uids )
                if(xx==yy) return true;
        return false;
    }
    friend bool operator!=(AmbiguousEntity const& x, AmbiguousEntity const& y){
        return !(x==y);
    }
};

struct EntityReprs{
    using dict_type = std::map<WikidataUID, std::vector<Words>>;
    using value_type = dict_type::value_type;
    struct OpCompare{
        OpCompare(EntityReprs const& self) : dict{self} {}
        bool exact_match(WikidataUID uid, std::vector<WordUID> qwords) const {
            auto it = dict.reprs.find(uid);
            if(it==dict.reprs.cend()) return false;
            OpEntityCompare op{*it};
            return op.exact_match(qwords);
        }
        template<typename TI>
        size_t exact_match(WikidataUID uid, TI beg, TI end) const {
            auto it = dict.reprs.find(uid);
            if(it ==dict.reprs.cend()) return 0;
            OpEntityCompare op{*it};
            return op.exact_match(beg,end);
        }
        EntityReprs const& dict;
    };
    struct OpEntityCompare{
        OpEntityCompare(value_type const& reprs) : reprs{reprs} {}
        bool exact_match(std::vector<WordUID> qwords) const {
            for(auto words : reprs.second) if(words.uids == qwords) return true;
            return false;
        }
        template<typename TI>
        size_t exact_match(TI beg, TI end) const {
            for(auto words : reprs.second){
                auto match=words.size();
                auto q=beg;
                for(auto uid : words.uids){
                    if(*q != uid || q==end) {
                        match=0;
                        break;
                    }
                    ++q;
                }
                if(match) return match;
            }
            return 0;
        }
        value_type reprs;
    };
    struct OpAmbiguousEntityCompare{
        bool exact_match(std::vector<WordUID> qwords) const {
            for(auto& op : ops)
                if(op.exact_match(qwords)) return true;
            return false;
        }
        template<typename TI>
        size_t exact_match(TI beg, TI end) const {
            for(auto& op : ops) {
                auto n = op.exact_match(beg, end);
                if (n) return n;
            }
            return 0;
        }
        std::vector<OpEntityCompare> ops;
    };

    EntityReprs(std::vector<Entity> const& entities){
        for(auto& entity : entities)
            reprs[entity.uid].push_back(entity.words);
    }
    OpCompare get_comparison_operator() const{
        return {*this};
    }
    OpEntityCompare get_comparison_operator(WikidataUID uid) const{
        auto it = reprs.find(uid);
        if(it==reprs.cend()) assert(0);
        return {*it};
    }
    OpAmbiguousEntityCompare get_comparison_operator(AmbiguousEntity entity) const{
        OpAmbiguousEntityCompare op{};
        for(auto uid : entity.uids) op.ops.push_back(get_comparison_operator(uid));
        return op;
    }
    Entity operator[](WikidataUID uid) const;
    Synonyms get_synonyms(WikidataUID uid) const;

    dict_type reprs;
};

}//namespace wordrep::wiki
}//namespace wordrep