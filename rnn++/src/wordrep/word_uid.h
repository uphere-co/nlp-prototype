#pragma once

#include <string>
#include <unordered_map>
#include <vector>

#include "utils/parallel.h"

#include "wordrep/indexes.h"

namespace wordrep {

struct UIDIndexBinary{
    std::string val;
};

template<typename TUID>
class UIDIndex{
public:
    using uid_t = TUID;
    UIDIndex(std::string file);
    UIDIndex(UIDIndexBinary filename);
    UIDIndex(UIDIndex &&org)
            : uid2word{std::move(org.uid2word)}
    {}
    UIDIndex(UIDIndex const& org)
            : uid2word{org.uid2word}
    {}
    bool isin(uid_t uid) const;
    uid_t operator[] (std::string const &word) const;
    std::string operator[](uid_t uid) const;
    uid_t uid(std::string const &word) const;
    std::string str(uid_t uid) const;
    void write_to_text(std::string filename) const;
    auto size() const {return uid2word.size();}
    std::vector<uid_t> get_uids() const;

    static uid_t get_uid(std::string const &word);

    void to_file(UIDIndexBinary filename) const;

private:
    tbb::concurrent_vector<std::pair<uid_t,std::string>> uid2word;
};

//forward declarations.
using WordUIDindex = UIDIndex<WordUID>;
using POSUIDindex = UIDIndex<POSUID>;
using ArcLabelUIDindex = UIDIndex<ArcLabelUID>;
using WikidataUIDindex = UIDIndex<WikidataUID>;

std::string the_unknown_word();
WordUID the_unknown_word_uid();

}//namespace wordrep
