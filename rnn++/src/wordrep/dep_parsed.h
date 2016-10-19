#pragma once

#include "utils/base_types.h"
#include "utils/json.h"

#include "wordrep/word_uid.h"
#include "wordrep/voca.h"

namespace util{
namespace io{
struct H5file; //forward declaration.
}//namespace util::io
}//namespace util

namespace wordrep{

struct WordPosIndexDummy{};
using WordPosition = util::IntegerLike<WordPosIndexDummy>;
struct SentPositionDummy{};
using SentPosition = util::IntegerLike<SentPositionDummy>;
struct SentUIDDummy{};
using SentUID = util::IntegerLike<SentUIDDummy>;

struct CharOffsetDummy;
using CharOffset = util::IntegerLike<CharOffsetDummy>;

struct DPTokenIndexDummy;
using DPTokenIndex = util::IntegerLike<DPTokenIndexDummy>;
struct ChunkIndexDummy;
using ChunkIndex = util::IntegerLike<ChunkIndexDummy>;

struct Sentence{
    Sentence(SentUID uid, DPTokenIndex beg, DPTokenIndex end)
    : uid{uid}, beg{beg}, end{end} {}
    SentUID uid;
    DPTokenIndex beg;
    DPTokenIndex end;
};

struct DepParsedTokens{
    DepParsedTokens(util::io::H5file const &file, std::string prefix);
    DepParsedTokens(){}

    void write_to_disk(std::string filename, std::string prefix) const;
    std::vector<Sentence> SegmentSentences() const;

    VocaIndex word(DPTokenIndex idx) const { return words[idx.val];}
    VocaIndex head_word(DPTokenIndex idx) const { return head_words[idx.val];}

private:
    std::vector<SentUID>      sent_uid;
    std::vector<ChunkIndex>   chunk_idx;
    std::vector<SentPosition> sent_idx;
    std::vector<VocaIndex>    words;
    std::vector<WordPosition> word_pidx;
    std::vector<VocaIndex>    head_words;
    std::vector<WordPosition> head_pidx;
    std::vector<CharOffset>   word_beg;
    std::vector<CharOffset>   word_end;
    std::vector<POSUID>       pos;
    std::vector<ArcLabelUID>  arclabel;
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
