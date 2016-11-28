#include <algorithm>
#include <codecvt>
#include <locale>

#include "fmt/printf.h"
#include "pqxx/pqxx"

#include "wordrep/dep_parsed.h"

#include "utils/hdf5.h"
#include "utils/string.h"

using namespace util;
using namespace util::io;

namespace wordrep{

RawTexts::RawTexts(std::string filename)
        :lines(util::string::readlines(filename))
{}

CharOffset Sentence::beg_offset() const {return tokens->word_beg(front());}
CharOffset Sentence::end_offset() const {return tokens->word_end(back());}
SentUID::val_t Sentence::chrlen() const{ return util::diff(end_offset(), beg_offset());}

DepParsedTokens::DepParsedTokens(util::io::H5file const &file, std::string prefix)
        : sents_uid {file,prefix+".sent_uid"},
          chunks_idx{file,prefix+".chunk_idx"},
          sents_idx {file,prefix+".sent_idx"},
          words     {file,prefix+".word"},
          words_uid {file,prefix+".word_uid"},
          words_pidx{file,prefix+".word_pidx"},
          head_words{file,prefix+".head"},
          heads_uid {file,prefix+".head_uid"},
          heads_pidx{file,prefix+".head_pidx"},
          words_beg {file,prefix+".word_beg"},
          words_end {file,prefix+".word_end"},
          poss      {file,prefix+".pos_uid"},
          arclabels {file,prefix+".arclabel_uid"}
{}

DepParsedTokens::DepParsedTokens(std::string prefix)
        : sents_uid {{},prefix+".sent_uid"},
          chunks_idx{{},prefix+".chunk_idx"},
          sents_idx {{},prefix+".sent_idx"},
          words     {{},prefix+".word"},
          words_uid {{},prefix+".word_uid"},
          words_pidx{{},prefix+".word_pidx"},
          head_words{{},prefix+".head"},
          heads_uid {{},prefix+".head_uid"},
          heads_pidx{{},prefix+".head_pidx"},
          words_beg {{},prefix+".word_beg"},
          words_end {{},prefix+".word_end"},
          poss      {{},prefix+".pos_uid"},
          arclabels {{},prefix+".arclabel_uid"}
{}

void DepParsedTokens::write_to_disk(std::string filename) const {
//    H5file outfile{H5name{filename}, hdf5::FileMode::rw_exist};
    H5file outfile{H5name{filename}, hdf5::FileMode::replace};
    sents_uid.write(outfile);
    chunks_idx.write(outfile);
    sents_idx.write(outfile);
    words_uid.write(outfile);
    words.write(outfile);
    words_pidx.write(outfile);
    heads_uid.write(outfile);
    head_words.write(outfile);
    heads_pidx.write(outfile);
    words_beg.write(outfile);
    words_end.write(outfile);
    poss.write(outfile);
    arclabels.write(outfile);
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

    auto chk_idx = chunk_idx(sent.beg);
//    auto chk_beg = std::find_if_not(chunks_idx.crbegin()+reversed_offset, chunks_idx.crend(),
//                                [chk_idx](auto x) { return x == chk_idx; }).base();
//    auto chk_end = std::find_if_not(chunks_idx.cbegin()+offset, chunks_idx.cend(),
//                                [chk_idx](auto x) { return x == chk_idx; });
    auto first_elm=DPTokenIndex{DPTokenIndex::val_t{0}};
    auto last_elm =DPTokenIndex::from_unsigned(n_tokens());
    auto chk_beg = sent.beg;
    for(;chk_beg!=first_elm; --chk_beg)
        if(chunk_idx(chk_beg)!=chk_idx) {++chk_beg;break;}
    auto chk_end = sent.end;
    for(;chk_end!=last_elm; ++chk_end)
        if(chunk_idx(chk_end)!=chk_idx) break;
    //std::cerr<<fmt::format("Chunk : {} of {}", chk_beg.val, chk_end.val)<<std::endl;
    assert(chunk_idx(chk_beg)==chk_idx);
    auto beg = sents_uid.cbegin() + diff(chk_beg, first_elm);
    auto end = sents_uid.cbegin() + diff(chk_end, first_elm);
    //std::cerr<<fmt::format("TokenIndex : {} of {}", beg->val, end->val)<<std::endl;
    auto it=beg;
    while(it!=end){
        auto uid=*it;
        uids.push_back(uid);
        it = std::find_if_not(it, end, [uid](auto x) { return x == uid; });
    }
    return uids;
}

void DepParsedTokens::append_corenlp_output(WordUIDindex const &wordUIDs,
                                            POSUIDindex const &posUIDs,
                                            ArcLabelUIDindex const &arclabelUIDs,
                                            data::CoreNLPjson const &output){

    int64_t sent_idx{0};
    size_t offset{0};
    auto per_tokens = [this,&sent_idx,&wordUIDs, &posUIDs](auto const &token){
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
        words_uid.push_back(wordUIDs[word]);
        words_pidx.push_back(WordPosition{word_pidx});
//            head_words;
        heads_uid.push_back(WordUID{});//
        heads_pidx.push_back(WordPosition{});//
        words_beg.push_back(CharOffset{token_beg});
        words_end.push_back(CharOffset{token_end});
        poss.push_back(posUIDs[pos]);
        arclabels.push_back(ArcLabelUID{});//
    };
    auto per_dep_tokens = [this,&offset,&wordUIDs,&arclabelUIDs](auto const &x){
        //dep dependent dependentGloss governor governorGloss
        auto word      = get_str(x,"dependentGloss");
        auto word_pidx = get_int(x,"dependent")-1;
        auto head_word = get_str(x,"governorGloss");
        auto head_pidx = get_int(x,"governor")-1;
        auto arc_label = get_str(x,"dep");

        auto i= word_pidx;
        assert(words_uid[offset+i] == wordUIDs[word]);
        assert(words_pidx[offset+i]==WordPosition{word_pidx});
        heads_uid[offset+i] = wordUIDs[head_word];
        heads_pidx[offset+i] = WordPosition{head_pidx};
        arclabels[offset+i] = arclabelUIDs[arc_label];
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
    util::append(sents_uid, tokens.sents_uid);
    auto tmp = tokens.chunks_idx;
    for(auto &x : tmp.get()) x += current_chunk_idx;
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

