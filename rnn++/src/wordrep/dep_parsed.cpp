#include <algorithm>

#include "wordrep/dep_parsed.h"

#include "utils/hdf5.h"
#include "utils/string.h"

using namespace util;
using namespace util::io;

namespace wordrep{
DepParsedTokens::DepParsedTokens(util::io::H5file const &file, std::string prefix)
: sent_idx{deserialize<SentIndex>(file.getRawData<int64_t>(H5name{prefix+".sent_idx"}))},
  word{deserialize<VocaIndex>(file.getRawData<int64_t>(H5name{prefix+".word"}))},
  word_pidx{deserialize<WordPosIndex>(file.getRawData<int64_t>(H5name{prefix+".word_pidx"}))},
  head_word{deserialize<VocaIndex>(file.getRawData<int64_t>(H5name{prefix+".head"}))},
  head_pidx{deserialize<WordPosIndex>(file.getRawData<int64_t>(H5name{prefix+".head_pidx"}))},
  arc_label{deserialize<ArcLabel>(file.getRawData<int64_t>(H5name{prefix+".arclabel_uid"}))}
{}

void DepParsedTokens::write_to_disk(std::string filename, std::string prefix) const {
//    H5file outfile{H5name{filename}, hdf5::FileMode::rw_exist};
    H5file outfile{H5name{filename}, hdf5::FileMode::replace};
    outfile.writeRawData(H5name{prefix+".sent_idx"}, serialize(sent_idx));
    outfile.writeRawData(H5name{prefix+".word"}, serialize(word));
    outfile.writeRawData(H5name{prefix+".word_pidx"},serialize(word_pidx));
    outfile.writeRawData(H5name{prefix+".head"}, serialize(head_word));
    outfile.writeRawData(H5name{prefix+".head_pidx"},serialize(head_pidx));
    outfile.writeRawData(H5name{prefix+".arclabel_uid"},serialize(arc_label));
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

