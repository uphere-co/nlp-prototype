#include "wordrep/wikientity_repr.h"

#include <sstream>

#include <fmt/printf.h>

#include "wordrep/word_iter.h"

namespace wordrep{

Entity::Entity(WordUIDindex const& wordUIDs, std::string line) {
    auto tokens = util::string::split(line, "\t");
    if(tokens.size()!=2){
        fmt::print("{}\n", line);
        assert(0);
    }
    uid = WikidataUIDindex::get_uid(tokens[0]);
    WordIterBase<std::string> word_iter{tokens[1]};
    word_iter.iter([this,&wordUIDs](auto w){words.push_back(wordUIDs[w]);});
//        for(auto w : util::string::split(tokens[1], " "))

}

std::string Entity::repr(WikidataUIDindex const& wikidataUIDs,
                         WordUIDindex const& wordUIDs) const{
    std::stringstream ss;
    ss << wikidataUIDs[uid];
    for(auto word : words) ss << " " << wordUIDs[word];
    return ss.str();
}
std::ostream& operator<< (std::ostream& os, Entity const& a){
    fmt::print(os, "{}\t", a.uid);
    for(auto word: a.words) fmt::print(os, " {}", word);
    return os;
}


Entity WikidataEntityReprs::operator[](WikidataUID uid) const{
    auto it = reprs.find(uid);
    assert(it!=reprs.end());
    return {uid,it->second.front()};
}
Synonyms WikidataEntityReprs::get_synonyms(WikidataUID uid) const{
    Synonyms synonyms{};
    auto it = reprs.find(uid);
    assert(it!=reprs.end());
    for(auto words : it->second) synonyms.reprs.emplace_back(std::move(words));
    return synonyms;
}

}//namespace wordrep
