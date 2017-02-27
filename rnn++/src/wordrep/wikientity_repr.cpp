#include "wordrep/wikientity_repr.h"

#include "utils/parallel.h"

namespace wordrep{
namespace wiki {

EntityReprs::EntityReprs(std::vector<Entity> entities)
        : dict{std::move(entities)}{
    auto less = [](Entity const& x, Entity const& y){return x.uid<y.uid;};
    tbb::parallel_sort(dict.begin(), dict.end(), less);
}
Entity EntityReprs::operator[](WikidataUID uid) const {
    auto synonym = find(uid).value();
    return {synonym.uid, synonym.reprs.front()};
}

Synonyms EntityReprs::get_synonyms(WikidataUID uid) const {
    return find(uid).value();
}
std::optional<Synonyms> EntityReprs::find(WikidataUID uid) const {
    auto less = [uid](auto& rhs){return uid <rhs.uid;};
    auto eq   = [uid](auto& rhs){return uid==rhs.uid;};
    auto mit= util::binary_find_block(dict, eq, less);
    if(!mit) return {};
    auto it = mit.value();
    Synonyms synonyms{uid,{}};
    for(auto e = it.first; e!= it.second; ++e)
        synonyms.reprs.push_back(e->words);
    return synonyms;
}

}//namespace wordrep::wiki
}//namespace wordrep
