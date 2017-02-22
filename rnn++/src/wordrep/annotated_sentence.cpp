#include "wordrep/annotated_sentence.h"

#include <fmt/printf.h>

#include "wordrep/word_uid.h"
#include "wordrep/wikientity_repr.h"

namespace wordrep{

std::string AnnotatedSentence::Token::repr(wiki::EntityReprs const &entity_reprs,
                                           WikidataUIDindex const &wikidataUIDs,
                                           WordUIDindex const &wordUIDs) const {
    std::stringstream ss;
    val.match([&ss,&entity_reprs,&wordUIDs,&wikidataUIDs](UnresolvedWikiEntity w) {
                  fmt::print(ss," (");
                  for (auto uid : w.uids)
                      fmt::print(ss,"{} ", entity_reprs[uid].repr(wikidataUIDs, wordUIDs));
                  fmt::print(ss,")");
              },
              [this,&ss,&wordUIDs](Word w) {
                  fmt::print(ss,"{} ", wordUIDs[w.dict->word_uid(w.idx)]);
              });
    return ss.str();
}

}//namespace wordrep
