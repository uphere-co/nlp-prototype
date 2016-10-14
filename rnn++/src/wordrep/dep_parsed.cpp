#include "wordrep/dep_parsed.h"

#include "utils/hdf5.h"

using namespace util::io;

namespace {
template<typename T>
std::vector<T> deserialize(std::vector<typename T::val_t> const& raw){
    std::vector<T> uids;
    for(auto uid : raw) uids.push_back(T{uid});
    return uids;
}
template<typename T>
std::vector<typename T::val_t> serialize(std::vector<T> const& uids){
    std::vector<typename T::val_t> raw;
    for(auto uid : uids) raw.push_back(uid.val);
    return raw;
}
}//nameless namespace

namespace wordrep{
DepParsedTokens::DepParsedTokens(util::io::H5file const &file, std::string prefix)
: sent_idx{deserialize<SentIndex>(file.getRawData<int64_t>(H5name{prefix+".sent_idx"}))},
  word{deserialize<WordUID>(file.getRawData<int64_t>(H5name{prefix+".word"}))},
  word_pidx{deserialize<WordIndex>(file.getRawData<int64_t>(H5name{prefix+".word_pidx"}))},
  head_word{deserialize<WordUID>(file.getRawData<int64_t>(H5name{prefix+".head_word"}))},
  head_pidx{deserialize<WordIndex>(file.getRawData<int64_t>(H5name{prefix+".head_pidx"}))},
  arc_label_raw{file.getRawData<char>(H5name{prefix+".arc_label"})},
  arc_label{util::string::unpack_word_views(arc_label_raw)}
{}

void DepParsedTokens::write_to_disk(std::string filename, std::string prefix) const {
//    H5file outfile{H5name{filename}, hdf5::FileMode::rw_exist};
    H5file outfile{H5name{filename}, hdf5::FileMode::replace};
    outfile.writeRawData(H5name{prefix+".sent_idx"}, serialize(sent_idx));
    outfile.writeRawData(H5name{prefix+".word"},     serialize(word));
    outfile.writeRawData(H5name{prefix+".word_pidx"},serialize(word_pidx));
    outfile.writeRawData(H5name{prefix+".head_word"},serialize(head_word));
    outfile.writeRawData(H5name{prefix+".head_pidx"},serialize(head_pidx));
    outfile.writeRawData(H5name{prefix+".arc_label"},arc_label_raw);
}
std::vector<Sentence> DepParsedTokens::SegmentSentences() const {
    auto beg=sent_idx.cbegin();
    auto end=sent_idx.cend();
    std::vector<Sentence> sents;
    auto it=beg;
    while(it!=end) {
        SentIndex uid{*it};
        DPTokenIndex sbeg{it-beg};
        it = std::find_if_not(it, end, [it](auto x) { return x == *it; });
        DPTokenIndex send{it-beg};
        sents.push_back(Sentence{uid, sbeg, send});
    }
    return sents;
}

}//namespace wordrep

