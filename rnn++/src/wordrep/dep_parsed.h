#pragma once
#include <map>

#include "data_source/corenlp.h"

#include "utils/base_types.h"
#include "utils/json.h"
#include "utils/persistent_vector.h"

#include "wordrep/word_uid.h"
#include "wordrep/voca.h"
#include "wordrep/indexes.h"

namespace util{
namespace io{
struct H5file; //forward declaration.
}//namespace util::io
}//namespace util

namespace wordrep{

struct RawTexts{
    RawTexts(std::string filename);
    std::string getline(ChunkIndex i) const {return lines[i.val];}

    std::vector<std::string> lines;
};

struct DepParsedTokens; //forward declaration.
struct Sentence{
    Sentence() : tokens{nullptr} {}
    Sentence(SentUID uid, DPTokenIndex beg, DPTokenIndex end, DepParsedTokens const *tokens)
            : uid{uid}, beg{beg}, end{end}, tokens{tokens} {}
    DPTokenIndex front() const {return beg;}
    DPTokenIndex back() const {return end-1;}
    CharOffset beg_offset() const;
    CharOffset end_offset() const;
    SentUID::val_t chrlen() const;
    SentUID uid;
    DPTokenIndex beg;
    DPTokenIndex end;
    DepParsedTokens const *tokens;
};

struct Sentences{
    Sentences(std::vector<Sentence> const &sents) {
        for(auto &sent : sents) uid2sent[sent.uid]=sent;
    }
    Sentence operator[](SentUID uid) const {
        auto it=uid2sent.find(uid);
        assert(it!=uid2sent.end());
        return it->second;
    }
    std::map<SentUID,Sentence> uid2sent{};
};


struct DepParsedTokens{
    template<typename T>
    using vec_t = util::TypedPersistentVector<T>;
    DepParsedTokens(util::io::H5file const &file, std::string prefix);
    DepParsedTokens(std::string prefix);
    DepParsedTokens(){}

    void write_to_disk(std::string filename) const;
    std::vector<Sentence> IndexSentences() const;
    std::vector<SentUID> sentences_in_chunk(Sentence const &sent) const;
    //std::vector<Chunk> IndexChunks() const;
    void append_corenlp_output(WordUIDindex const &wordUIDs,
                               POSUIDindex const &posUIDs,
                               ArcLabelUIDindex const &arclabelUIDs,
                               data::CoreNLPjson const &output);
    void append(DepParsedTokens const &tokens);
    void build_voca_index(VocaIndexMap const &voca);
    std::vector<SentUID> build_sent_uid(SentUID init_uid);

    WordUID    word_uid(DPTokenIndex idx)  const {return words_uid[idx.val];}
    ChunkIndex chunk_idx(DPTokenIndex idx) const {return chunks_idx[idx.val];}
    CharOffset word_beg(DPTokenIndex idx)  const {return words_beg[idx.val];}
    CharOffset word_end(DPTokenIndex idx)  const {return words_end[idx.val];}
    VocaIndex  word(DPTokenIndex idx)      const {return words[idx.val];}
    WordPosition word_pos(DPTokenIndex idx)const {return words_pidx[idx.val];}
    VocaIndex  head_word(DPTokenIndex idx) const {return head_words[idx.val];}
    WordPosition head_pos(DPTokenIndex idx)const {return heads_pidx[idx.val];}
    size_t n_tokens() const { return chunks_idx.size();}

private:
    vec_t<SentUID>      sents_uid;
    vec_t<ChunkIndex>   chunks_idx;
    vec_t<SentIndex>    sents_idx;
    vec_t<VocaIndex>    words;
    vec_t<WordUID>      words_uid;
    vec_t<WordPosition> words_pidx;
    vec_t<VocaIndex>    head_words;
    vec_t<WordUID>      heads_uid;
    vec_t<WordPosition> heads_pidx;
    vec_t<CharOffset>   words_beg;
    vec_t<CharOffset>   words_end;
    vec_t<POSUID>       poss;
    vec_t<ArcLabelUID>  arclabels;
    ChunkIndex current_chunk_idx{ChunkIndex::val_t{0}};
};

}//namespace wordrep

