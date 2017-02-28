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

}//namespace wordrep
