#include "wordrep/wikientity_repr.h"

#include "utils/parallel.h"

namespace wordrep{
namespace wiki {

EntityReprs::EntityReprs(UIDSortedEntities const& entities)
        : dict{entities}
{}
Entity EntityReprs::operator[](WikidataUID uid) const {
    auto ms = find(uid);
    assert(ms);
    auto synonym = ms.value();
    return {synonym.uid, synonym.reprs.front()};
}

Synonyms EntityReprs::get_synonyms(WikidataUID uid) const {
    return find(uid).value();
}
std::optional<Synonyms> EntityReprs::find(WikidataUID uid) const {
    auto less = [uid](auto& rhs){return uid <rhs.uid;};
    auto eq   = [uid](auto& rhs){return uid==rhs.uid;};
    auto mit= util::binary_find_block(dict.cbegin(), dict.cend(), eq, less);
    if(!mit) return {};
    auto it = mit.value();
    Synonyms synonyms{uid,{}};
    for(auto e = it.first; e!= it.second; ++e)
        synonyms.reprs.push_back(e->words);
    return synonyms;
}

}//namespace wordrep::wiki
}//namespace wordrep
