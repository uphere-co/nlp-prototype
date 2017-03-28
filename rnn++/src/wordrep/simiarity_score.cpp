#include "wordrep/simiarity_score.h"

#include <sstream>
#include <fmt/format.h>

#include "wordrep/word_uid.h"

namespace wordrep{

std::string DepPair::repr(WordUIDindex const& wordUIDs) const{
    std::stringstream ss;
    ss << fmt::format("{}:dep {}:gov", wordUIDs[word_dep], wordUIDs[word_gov]);
    return ss.str();
}

std::string Scoring::AmbiguousEntity::repr(DepParsedTokens const& dict, WordUIDindex const& wordUIDs) const {
    std::stringstream ss;
    ss << fmt::format("{}:dep {}:gov", idxs.repr(dict, wordUIDs), wordUIDs[word_gov]);
    return ss.str();
}

std::vector<wiki::AmbiguousUID> Scoring::SentenceToScored::all_named_entities() const{
    std::vector<wiki::AmbiguousUID> uids;
    for(auto& entity : entities)
        uids.push_back(entity.uid);
    return uids;
}

void Scoring::SentenceToScored::filter_false_named_entity(wiki::OpNamedEntity const &op,
                                                          POSUIDindex const& posUIDs){
    for(auto it=entities.begin(); it<entities.end();){
        auto& entity = *it;
        if(entity.idxs.size()!=1) {
            for(auto iit = entity.uid.candidates.begin();iit!=entity.uid.candidates.end();){
                auto& uid=*iit;
                if(!op.is_named_entity(uid)){
                    std::swap(uid, entity.uid.candidates.back());
                    entity.uid.candidates.pop_back();
                    continue;
                } else{
                    ++iit;
                }
            }
            for(auto iit = entity.candidates.begin();iit!=entity.candidates.end();){
                auto& elm=*iit;
                if(!op.is_named_entity(elm.uid)){
                    std::swap(elm, entity.candidates.back());
                    entity.candidates.pop_back();
                    continue;
                } else{
                    ++iit;
                }
            }
            if(entity.candidates.empty()){
                for(auto idx : entity.idxs){
                    words.push_back({orig,idx});
                }
                std::swap(entity, entities.back());
                entities.pop_back();
            } else{
                ++it;
            }
        } else{
            auto idx = entity.idxs.front();
            auto is_noun = [&posUIDs](auto pos){
                auto NN = posUIDs["NN"];
                auto NNS = posUIDs["NNS"];
                auto NNP = posUIDs["NNP"];
                auto NNPS = posUIDs["NNPS"];
                return pos==NN||pos==NNS||pos==NNP||pos==NNPS;
            };
            auto pos = this->orig.dict->pos(idx);
            if(is_noun(pos)) {
                ++it;
            } else{
                words.push_back({orig,idx});
                std::swap(entity, entities.back());
                entities.pop_back();
            }
        }
    }
}

std::string Scoring::SentenceToScored::repr(WordUIDindex const& wordUIDs) const{
    std::stringstream ss;
    for(auto& entity : entities) ss << fmt::format("[{}] ", entity.repr(*orig.dict, wordUIDs));
    for(auto& word : words) ss << fmt::format("({}) ", word.repr(wordUIDs));
    return ss.str();
}

}//namespace wordrep
