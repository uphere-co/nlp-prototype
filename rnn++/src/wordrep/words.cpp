#include "wordrep/words.h"

#include <ostream>

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

}//namespace wordrep