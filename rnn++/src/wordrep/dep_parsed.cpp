#include <algorithm>
#include <codecvt>
#include <locale>
#include <exception>

#include <fmt/printf.h>

#include "wordrep/dep_parsed.h"

#include "utils/flatbuffers/io.h"
#include "utils/parallel.h"

#include "utils/versioned_name.h"
#include "utils/hdf5.h"
#include "utils/string.h"

namespace {

template<typename T>
void load_binary_file(std::string filename, T& vec){
    namespace fb = util::io::fb;
    fb::deserialize_i64vector(fb::load_binary_file(filename), vec);
};


template<typename T>
void write_to_binary_file(util::TypedPersistentVector<T> const& vec, std::string filename){
    std::vector<int64_t> vs;
    vs.reserve(vec.size());
    for(auto v : vec) vs.push_back(v.val);
    util::io::fb::to_file(vs, util::io::I64Binary{filename});
}
}//nameless namespace


namespace wordrep{

RawTexts::RawTexts(std::string filename)
        :lines(util::string::readlines(filename))
{}

class VersionMismatchException: public std::exception {
    virtual const char* what() const throw() {
        return "Data version mismatches.";
    }
};

DepParsedTokens DepParsedTokens::factory(DepParsedFile const& file){
    wordrep::DepParsedTokens texts{};
    util::parallel_invoke(
            [&texts,&file]() { load_binary_file(fmt::format("{}.sents_uid.i64v", file.name), texts.sents_uid); },
            [&texts,&file]() { load_binary_file(fmt::format("{}.chunks_idx.i64v",file.name), texts.chunks_idx); },
            [&texts,&file]() { load_binary_file(fmt::format("{}.sents_idx.i64v", file.name), texts.sents_idx); },
            [&texts,&file]() { load_binary_file(fmt::format("{}.words.i64v",     file.name), texts.words); },
            [&texts,&file]() { load_binary_file(fmt::format("{}.words_uid.i64v", file.name), texts.words_uid); },
            [&texts,&file]() { load_binary_file(fmt::format("{}.words_pidx.i64v",file.name), texts.words_pidx); },
            [&texts,&file]() { load_binary_file(fmt::format("{}.head_words.i64v",file.name), texts.head_words); },
            [&texts,&file]() { load_binary_file(fmt::format("{}.heads_uid.i64v", file.name), texts.heads_uid); },
            [&texts,&file]() { load_binary_file(fmt::format("{}.heads_pidx.i64v",file.name), texts.heads_pidx); },
            [&texts,&file]() { load_binary_file(fmt::format("{}.words_beg.i64v", file.name), texts.words_beg); },
            [&texts,&file]() { load_binary_file(fmt::format("{}.words_end.i64v", file.name), texts.words_end); },
            [&texts,&file]() { load_binary_file(fmt::format("{}.poss.i64v",      file.name), texts.poss); },
            [&texts,&file]() { load_binary_file(fmt::format("{}.arclabels.i64v", file.name), texts.arclabels); }
    );
    return texts;
}

void DepParsedTokens::to_file(DepParsedFile const& file) const {
    util::parallel_invoke(
        [this,&file](){write_to_binary_file(this->sents_uid,  file.name + ".sents_uid.i64v");},
        [this,&file](){write_to_binary_file(this->chunks_idx, file.name + ".chunks_idx.i64v");},
        [this,&file](){write_to_binary_file(this->sents_idx,  file.name + ".sents_idx.i64v");},
        [this,&file](){write_to_binary_file(this->words,      file.name + ".words.i64v");},
        [this,&file](){write_to_binary_file(this->words_uid,  file.name + ".words_uid.i64v");},
        [this,&file](){write_to_binary_file(this->words_pidx, file.name + ".words_pidx.i64v");},
        [this,&file](){write_to_binary_file(this->head_words, file.name + ".head_words.i64v");},
        [this,&file](){write_to_binary_file(this->heads_uid,  file.name + ".heads_uid.i64v");},
        [this,&file](){write_to_binary_file(this->heads_pidx, file.name + ".heads_pidx.i64v");},
        [this,&file](){write_to_binary_file(this->words_beg,  file.name + ".words_beg.i64v");},
        [this,&file](){write_to_binary_file(this->words_end,  file.name + ".words_end.i64v");},
        [this,&file](){write_to_binary_file(this->poss,       file.name + ".poss.i64v");},
        [this,&file](){write_to_binary_file(this->arclabels,  file.name + ".arclabels.i64v");}
    );
}

void DepParsedTokens::build_voca_index(VocaIndexMap const &voca){
    auto n = words.size();
    for(auto it=words_uid.cbegin()+n; it!=words_uid.cend(); ++it) {
        auto uid = *it;
        words.push_back(voca[uid]);
    }
    for(auto it=heads_uid.cbegin()+n; it!=heads_uid.cend(); ++it) {
        auto uid = *it;
        head_words.push_back(voca[uid]);
    }
}
std::vector<Sentence> DepParsedTokens::IndexSentences() const {
    auto beg=sents_uid.cbegin();
    auto end=sents_uid.cend();
    std::vector<Sentence> sents;
    auto it=beg;
    while(it!=end) {
        SentUID uid{*it};
        DPTokenIndex sbeg{it-beg};
        it = std::find_if_not(it, end, [uid](auto x) { return x == uid; });
        DPTokenIndex send{it-beg};
        sents.push_back(Sentence{uid, sbeg, send, this});
    }
    return sents;
}

std::vector<SentUID> DepParsedTokens::sentences_in_chunk(Sentence const &sent) const{
    std::vector<SentUID> uids;

    auto chk_idx = chunk_idx(sent.front());
//    auto chk_beg = std::find_if_not(chunks_idx.crbegin()+reversed_offset, chunks_idx.crend(),
//                                [chk_idx](auto x) { return x == chk_idx; }).base();
//    auto chk_end = std::find_if_not(chunks_idx.cbegin()+offset, chunks_idx.cend(),
//                                [chk_idx](auto x) { return x == chk_idx; });
    auto first_elm=DPTokenIndex{DPTokenIndex::val_t{0}};
    auto last_elm =DPTokenIndex::from_unsigned(n_tokens());
    auto chk_beg = sent.front();
    for(;chk_beg!=first_elm; --chk_beg)
        if(chunk_idx(chk_beg)!=chk_idx) {++chk_beg;break;}
    auto chk_end = *(sent.end());
    for(;chk_end!=last_elm; ++chk_end)
        if(chunk_idx(chk_end)!=chk_idx) break;
    //std::cerr<<fmt::format("Chunk : {} of {}", chk_beg, chk_end)<<std::endl;
    assert(chunk_idx(chk_beg)==chk_idx);
    auto beg = sents_uid.cbegin() + diff(chk_beg, first_elm);
    auto end = sents_uid.cbegin() + diff(chk_end, first_elm);
    //std::cerr<<fmt::format("TokenIndex : {} of {}", beg_token, end)<<std::endl;
    auto it=beg;
    while(it!=end){
        auto uid=*it;
        uids.push_back(uid);
        it = std::find_if_not(it, end, [uid](auto x) { return x == uid; });
    }
    return uids;
}

void DepParsedTokens::append_corenlp_output(data::CoreNLPjson const &output){
    using util::get_str;
    using util::get_int;

    int64_t sent_idx{0};
    size_t offset{0};
    auto per_tokens = [this,&sent_idx](auto const &token){
        auto after  = get_str(token, "after");
        auto before = get_str(token, "before");
        auto token_beg = get_int(token,"characterOffsetBegin");
        auto token_end = get_int(token,"characterOffsetEnd");
        auto word_pidx = get_int(token,"index")-1;
        auto originalText = get_str(token,"originalText");
        auto pos = get_str(token,"pos");
        auto word = get_str(token,"word");

//            sents_uid;
        chunks_idx.push_back(current_chunk_idx);
        sents_idx.push_back(SentIndex{sent_idx});
//            words;
        words_uid.push_back(WordUIDindex::get_uid(word));
        words_pidx.push_back(WordPosition{word_pidx});
//            head_words;
        heads_uid.push_back(WordUID{});//
        heads_pidx.push_back(WordPosition{});//
        words_beg.push_back(CharOffset{token_beg});
        words_end.push_back(CharOffset{token_end});
        poss.push_back(POSUIDindex::get_uid(pos));
        arclabels.push_back(ArcLabelUID{});//
    };
    auto per_dep_tokens = [this,&offset](auto const &x){
        //dep dependent dependentGloss governor governorGloss
        auto word      = get_str(x,"dependentGloss");
        auto word_pidx = get_int(x,"dependent")-1;
        auto head_word = get_str(x,"governorGloss");
        auto head_pidx = get_int(x,"governor")-1;
        auto arc_label = get_str(x,"dep");

        auto i= word_pidx;
        assert(words_uid[offset+i] == WordUIDindex::get_uid(word));
        assert(words_pidx[offset+i]==WordPosition{word_pidx});
        heads_uid[offset+i]  = WordUIDindex::get_uid(head_word);
        heads_pidx[offset+i] = WordPosition{head_pidx};
        arclabels[offset+i]  = ArcLabelUIDindex::get_uid(arc_label);
    };
    auto pre_per_sent = [&offset,this](auto) {
        offset = sents_idx.size();
    };
    auto post_per_sent = [&sent_idx](auto) {
        ++sent_idx;
    };
    output.iter_sent(pre_per_sent, per_tokens, per_dep_tokens, post_per_sent);
    ++current_chunk_idx;
}

void DepParsedTokens::append(DepParsedTokens const &tokens){
    if(!tokens.n_tokens()) return;
    util::append(sents_uid, tokens.sents_uid);
    auto tmp = tokens.chunks_idx.get();
    for(auto &x : tmp) x += current_chunk_idx;
    current_chunk_idx = tmp.back()+1;
    util::append(chunks_idx, tmp);
    util::append(sents_idx, tokens.sents_idx);
    util::append(words,     tokens.words);
    util::append(words_uid, tokens.words_uid);
    util::append(words_pidx,tokens.words_pidx);
    util::append(head_words,tokens.head_words);
    util::append(heads_uid, tokens.heads_uid);
    util::append(heads_pidx,tokens.heads_pidx);
    util::append(words_beg, tokens.words_beg);
    util::append(words_end, tokens.words_end);
    util::append(poss,      tokens.poss);
    util::append(arclabels, tokens.arclabels);
}

std::vector<SentUID>  DepParsedTokens::build_sent_uid(SentUID init_uid){
    auto n = sents_uid.size();
    auto beg=sents_idx.cbegin()+n;
    auto end=sents_idx.cend();
    auto chunk_beg=chunks_idx.cbegin()+n;
    auto it=beg;
    auto it_chunk=chunk_beg;
    decltype(sents_uid) new_uids{{}, sents_uid.get_name()};
    if(it==end) return new_uids.get();
    SentIndex current_idx{*it};
    ChunkIndex current_chunk{*it_chunk};
    SentUID current_uid = n>0? sents_uid.back()+1: init_uid;
    new_uids.push_back(current_uid);
    while(it!=end) {
        if( *it == current_idx && *it_chunk==current_chunk) {sents_uid.push_back(current_uid);}
        else {
            current_idx=*it;
            current_chunk = *it_chunk;
            sents_uid.push_back(++current_uid);
            new_uids.push_back(current_uid);
        }
        ++it;
        ++it_chunk;
    }
    return new_uids.get();
}


}//namespace wordrep

