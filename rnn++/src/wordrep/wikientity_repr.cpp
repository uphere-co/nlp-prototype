#include "wordrep/wikientity_repr.h"

namespace wordrep{
namespace wiki {

Entity EntityReprs::operator[](WikidataUID uid) const {
    auto it = reprs.find(uid);
    assert(it != reprs.end());
    return {uid, it->second.front()};
}

Synonyms EntityReprs::get_synonyms(WikidataUID uid) const {
    Synonyms synonyms{};
    auto it = reprs.find(uid);
    assert(it != reprs.end());
    for (auto words : it->second) synonyms.reprs.emplace_back(std::move(words));
    return synonyms;
}

}//namespace wordrep::wiki
}//namespace wordrep
