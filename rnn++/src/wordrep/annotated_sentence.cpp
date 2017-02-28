#include "wordrep/annotated_sentence.h"

#include <sstream>
#include <fmt/printf.h>

#include "wordrep/word_uid.h"
#include "wordrep/dep_parsed.h"
#include "wordrep/wikientity_repr.h"

namespace wordrep{

std::string AnnotatedSentence::repr(wiki::EntityReprs const &entity_reprs,
                                           WikidataUIDindex const &wikidataUIDs,
                                           WordUIDindex const &wordUIDs) const {
    std::stringstream ss;
    for(auto token : tokens) {
        token.val.match([&ss, &entity_reprs, &wordUIDs, &wikidataUIDs](Token::UnresolvedWikiEntity w) {
                            fmt::print(ss, " (");
                            for (auto uid : w.uid.candidates)
                                fmt::print(ss, "{} ", entity_reprs[uid].repr(wikidataUIDs, wordUIDs));
                            fmt::print(ss, ")");
                        },
                        [this, &ss, &wordUIDs](DPTokenIndex idx) {
                            fmt::print(ss, "{} ", wordUIDs[sent.dict->word_uid(idx)]);
                        });
    }
    return ss.str();
}

}//namespace wordrep
