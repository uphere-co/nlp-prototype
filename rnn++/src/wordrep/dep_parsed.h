#pragma once

#include "utils/base_types.h"

#include "wordrep/word_uid.h"
#include "wordrep/voca.h"

namespace util{
namespace io{
struct H5file; //forward declaration.
}//namespace util::io
}//namespace util

namespace wordrep{

struct WordIndexDummy{};
using WordPosIndex = util::IntegerLike<WordIndexDummy>;
struct SentIndexDummy{};
using SentIndex = util::IntegerLike<SentIndexDummy>;
struct ArcLabelDummy{};
using ArcLabel = util::IntegerLike<ArcLabelDummy>;

struct SentPositionDummy{};
using SentPosition = util::IntegerLike<SentPositionDummy>;

struct DPTokenIndexDummy;
using DPTokenIndex = util::IntegerLike<DPTokenIndexDummy>;

struct Sentence{
    Sentence(SentIndex uid, DPTokenIndex beg, DPTokenIndex end)
    : uid{uid}, beg{beg}, end{end} {}
    SentIndex uid;
    DPTokenIndex beg;
    DPTokenIndex end;
};

struct DepParsedTokens{
    DepParsedTokens(util::io::H5file const &file, std::string prefix);

    void write_to_disk(std::string filename, std::string prefix) const;
    std::vector<Sentence> SegmentSentences() const;

    std::vector<SentIndex>    sent_idx;
    std::vector<VocaIndex>    word;
    std::vector<WordPosIndex> word_pidx;
    std::vector<VocaIndex>    head_word;
    std::vector<WordPosIndex> head_pidx;
    std::vector<ArcLabel>     arc_label;
};

}//namespace wordrep
