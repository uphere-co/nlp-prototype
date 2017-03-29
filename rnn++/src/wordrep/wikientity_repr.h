#pragma once

#include "wordrep/wikientity_base.h"

namespace wordrep{
namespace wiki{

struct EntityReprs{
    struct OpCompare{
        OpCompare(EntityReprs const& self) : dict{self} {}
        bool exact_match(WikidataUID uid, std::vector<WordUID> qwords) const {
            auto mit = dict.find(uid);
            if(!mit) return false;
            auto synonym = mit.value();
            OpEntityCompare op{synonym};
            return op.exact_match(qwords);
        }
        template<typename TI>
        size_t exact_match(WikidataUID uid, TI beg, TI end) const {
            auto mit = dict.find(uid);
            if(!mit) return 0;
            auto synonym = mit.value();
            OpEntityCompare op{synonym};
            return op.exact_match(beg,end);
        }
        EntityReprs const& dict;
    };
    struct OpEntityCompare{
        OpEntityCompare(Synonyms const& reprs) : synonyms{reprs} {}
        bool exact_match(std::vector<WordUID> qwords) const {
            for(auto repr : synonyms.reprs) if(repr.uids == qwords) return true;
            return false;
        }
        template<typename TI>
        size_t exact_match(TI beg, TI end) const {
            for(auto repr : synonyms.reprs){
                auto match=repr.size();
                auto q=beg;
                for(auto uid : repr.uids){
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
        bool isin(Sentence const& sent) const {
            auto end = sent.iter_words().end();
            for(auto it=sent.iter_words().begin(); it!=end; ++it)
                if(exact_match(it,end))
                    return true;
            return false;
        }
        Synonyms synonyms;
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
        bool isin(Sentence const& sent) const {
            for(auto& op : ops) {
                if(op.isin(sent)) return true;
            }
            return false;
        }
        std::vector<OpEntityCompare> ops;
    };
    struct OpContainAllAmbiguousEntities{
        bool isin(Sentence const& sent) const{
            for(auto& op : ops)
                if(!op.isin(sent))
                    return false;
            return true;

        }
        std::vector<OpAmbiguousEntityCompare> ops;
    };

    EntityReprs(UIDSortedEntities const& entities);
    OpCompare get_comparison_operator() const{
        return {*this};
    }
    OpEntityCompare get_comparison_operator(WikidataUID uid) const{
        auto mit = find(uid);
        if(!mit) assert(0);
        auto it = mit.value();
        return {it};
    }
    OpAmbiguousEntityCompare get_comparison_operator(AmbiguousUID const& entity) const{
        OpAmbiguousEntityCompare op{};
        for(auto uid : entity.candidates) op.ops.push_back(get_comparison_operator(uid));
        return op;
    }
    OpContainAllAmbiguousEntities get_comparison_operator(std::vector<AmbiguousUID> const& uids) const{
        OpContainAllAmbiguousEntities op{};
        for(auto uid : uids) op.ops.push_back(get_comparison_operator(uid));
        return op;
    }
    Entity operator[](WikidataUID uid) const;
    Synonyms get_synonyms(WikidataUID uid) const;
    std::optional<Synonyms> find(WikidataUID uid) const;
private:
    UIDSortedEntities const& dict;
};

}//namespace wordrep::wiki
}//namespace wordrep
