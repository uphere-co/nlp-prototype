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

struct WordPosIndexDummy{};
using WordPosIndex = util::IntegerLike<WordPosIndexDummy>;
struct SentUIDDummy{};
using SentUID = util::IntegerLike<SentUIDDummy>;
struct ArcLabelDummy{};
using ArcLabel = util::IntegerLike<ArcLabelDummy>;

struct SentPositionDummy{};
using SentPosition = util::IntegerLike<SentPositionDummy>;

struct DPTokenIndexDummy;
using DPTokenIndex = util::IntegerLike<DPTokenIndexDummy>;

struct Sentence{
    Sentence(SentUID uid, DPTokenIndex beg, DPTokenIndex end)
    : uid{uid}, beg{beg}, end{end} {}
    SentUID uid;
    DPTokenIndex beg;
    DPTokenIndex end;
};

struct DepParsedTokens{
    DepParsedTokens(util::io::H5file const &file, std::string prefix);

    void write_to_disk(std::string filename, std::string prefix) const;
    std::vector<Sentence> SegmentSentences() const;

    std::vector<SentUID>    sent_idx;
    std::vector<VocaIndex>    word;
    std::vector<WordPosIndex> word_pidx;
    std::vector<VocaIndex>    head_word;
    std::vector<WordPosIndex> head_pidx;
    std::vector<ArcLabel>     arc_label;
};

namespace ygp{
struct TableUIDDummy{};
using TableUID = util::IntegerLike<TableUIDDummy>;
struct ColumUIDDummy{};
using ColumnUID = util::IntegerLike<ColumUIDDummy>;
struct RowUIDDummy{};
using RowUID    = util::IntegerLike<RowUIDDummy>;
struct SentIndexDummy{};
using SentIndex = util::IntegerLike<SentIndexDummy>;
} //namespace wordrep::ygp

}//namespace wordrep
