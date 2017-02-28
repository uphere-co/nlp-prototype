#include "wordrep/wikientity_base.h"

#include <sstream>

#include <fmt/printf.h>

#include "wordrep/word_iter.h"

#include "utils/string.h"

namespace wordrep{
namespace wiki {

Entity::Entity(WordUIDindex const &wordUIDs, std::string line)
        : words{{}} {
    auto tokens = util::string::split(line, "\t");
    if (tokens.size() != 2) {
        fmt::print("{}\n", line);
        assert(0);
    }
    uid = WikidataUIDindex::get_uid(tokens[0]);
    WordIterBase<std::string> word_iter{tokens[1]};
    word_iter.iter([this, &wordUIDs](auto w) { words.uids.push_back(wordUIDs[w]); });
}

std::string Entity::repr(WikidataUIDindex const &wikidataUIDs,
                         WordUIDindex const &wordUIDs) const {
    std::stringstream ss;
    ss << wikidataUIDs[uid] << " " << words.repr(wordUIDs);
    return ss.str();
}

std::ostream &operator<<(std::ostream &os, Entity const &a) {
    fmt::print(os, "{}\t", a.uid);
    for (auto word: a.words.uids) fmt::print(os, " {}", word);
    return os;
}

}//namespace wordrep::wiki
}//namespace wordrep
