#include "wordrep/words.h"

#include <ostream>
#include <sstream>

#include "wordrep/dep_parsed.h"

namespace wordrep{


WordsRepr::WordsRepr(Words const& words, WordUIDindex const& wordUIDs)
        :words{words}, wordUIDs{wordUIDs}
{}
std::ostream& operator<<(std::ostream& os, WordsRepr const& self){
    for(auto uid : self.words.uids){
        os << self.wordUIDs[uid]<< " ";
    }
    return os;
}

std::string ConsecutiveTokens::repr(DepParsedTokens const& dict, WordUIDindex const& wordUIDs) const{
    std::stringstream ss;
    for(auto idx : *this){
        ss << wordUIDs[dict.word_uid(idx)]<< " ";
    }
    return ss.str();
}

DPTokenIndex ConsecutiveTokens::dep_token_idx(DepParsedTokens const& dict) const {
    auto positions = util::map(*this, [&dict](auto idx) { return dict.word_pos(idx); });
    for (auto idx : *this) {
        auto mh = dict.head_pos(idx);
        if (!mh) continue;
        auto head = mh.value();
        if (util::isin(positions, head)) continue;
        return idx;
    }
    return front();
}

}//namespace wordrep
