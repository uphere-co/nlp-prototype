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
    void append_corenlp_output(WordUIDindex const &wordUIDs,
                               POSUIDindex const &posUIDs,
                               ArcLabelUIDindex const &arclabelUIDs,
                               nlohmann::json const &output);

    WordUID word_uid(DPTokenIndex idx) const { return words_uid[idx.val];}
    VocaIndex word(DPTokenIndex idx) const { return words[idx.val];}
    VocaIndex head_word(DPTokenIndex idx) const { return head_words[idx.val];}

private:
    std::vector<SentUID>      sents_uid;
    std::vector<ChunkIndex>   chunks_idx;
    std::vector<SentPosition> sents_idx;
    std::vector<VocaIndex>    words;
    std::vector<WordUID>      words_uid;
    std::vector<WordPosition> words_pidx;
    std::vector<VocaIndex>    head_words;
    std::vector<WordUID>      heads_uid;
    std::vector<WordPosition> heads_pidx;
    std::vector<CharOffset>   words_beg;
    std::vector<CharOffset>   words_end;
    std::vector<POSUID>       poss;
    std::vector<ArcLabelUID>  arclabels;
    ChunkIndex current_chunk_idx{ChunkIndex::val_t{0}};
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
