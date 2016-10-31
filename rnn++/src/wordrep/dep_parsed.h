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
using SentIndex = util::IntegerLike<SentPositionDummy>;
struct SentUIDDummy{};
using SentUID = util::IntegerLike<SentUIDDummy>;

struct CharOffsetDummy;
using CharOffset = util::IntegerLike<CharOffsetDummy>;

struct DPTokenIndexDummy;
using DPTokenIndex = util::IntegerLike<DPTokenIndexDummy>;
struct ChunkIndexDummy;
using ChunkIndex = util::IntegerLike<ChunkIndexDummy>;

struct RawTexts{
    RawTexts(std::string filename);
    std::string getline(ChunkIndex i) const {return lines[i.val];}

    std::vector<std::string> lines;
};

struct DepParsedTokens; //forward declaration.
struct Sentence{
    Sentence(SentUID uid, DPTokenIndex beg, DPTokenIndex end, DepParsedTokens const *tokens)
            : uid{uid}, beg{beg}, end{end}, tokens{tokens} {}
    SentUID uid;
    DPTokenIndex beg;
    DPTokenIndex end;
    DepParsedTokens const *tokens;
};

struct DepParsedTokens{
    DepParsedTokens(util::io::H5file const &file, std::string prefix);
    DepParsedTokens(){}

    void write_to_disk(std::string filename, std::string prefix) const;
    std::vector<Sentence> IndexSentences() const;
    void append_corenlp_output(WordUIDindex const &wordUIDs,
                               POSUIDindex const &posUIDs,
                               ArcLabelUIDindex const &arclabelUIDs,
                               nlohmann::json const &output);
    void build_voca_index(VocaIndexMap const &voca);
    void build_sent_uid();

    WordUID word_uid(DPTokenIndex idx) const { return words_uid[idx.val];}
    ChunkIndex chunk_idx(DPTokenIndex idx) const {return chunks_idx[idx.val];}
    CharOffset word_beg(DPTokenIndex idx) const {return words_beg[idx.val];}
    CharOffset word_end(DPTokenIndex idx) const {return words_end[idx.val];}
    VocaIndex word(DPTokenIndex idx) const { return words[idx.val];}
    VocaIndex head_word(DPTokenIndex idx) const { return head_words[idx.val];}
    size_t n_tokens() const { return chunks_idx.size();}

//private:
    std::vector<SentUID>      sents_uid;
    std::vector<ChunkIndex>   chunks_idx;
    std::vector<SentIndex>    sents_idx;
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
struct RowIndexDummy{};
using RowIndex    = util::IntegerLike<RowIndexDummy>;
struct RowUIDDummy{};
using RowUID    = util::IntegerLike<RowUIDDummy>;

struct YGPindexer{
    YGPindexer(util::io::H5file const &file, std::string prefix);
    RowIndex row_idx(ChunkIndex idx) const {return chunk2idx[idx.val];}
    RowUID row_uid(ChunkIndex idx) const {return chunk2row_uid[idx.val];}
    ColumnUID column_uid(ChunkIndex idx) const {return chunk2col_uid[idx.val];}

    std::vector<RowIndex> chunk2idx;
    std::vector<RowUID> chunk2row_uid;
    std::vector<ColumnUID> chunk2col_uid;
};

struct YGPdump{
    YGPdump(std::string filename);
    std::string getline(RowUID i) const {return lines[i.val];}

    std::vector<std::string> lines;
};
} //namespace wordrep::ygp

}//namespace wordrep
