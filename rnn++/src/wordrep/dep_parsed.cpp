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
: sents_uid{deserialize<SentUID>(file.getRawData<int64_t>(H5name{prefix+".sent_uid"}))},
  chunks_idx{deserialize<ChunkIndex>(file.getRawData<int64_t>(H5name{prefix+".chunk_idx"}))},
  sents_idx{deserialize<SentIndex>(file.getRawData<int64_t>(H5name{prefix+".sent_idx"}))},
  words{deserialize<VocaIndex>(file.getRawData<int64_t>(H5name{prefix+".word"}))},
  words_uid{deserialize<WordUID>(file.getRawData<int64_t>(H5name{prefix+".word_uid"}))},
  words_pidx{deserialize<WordPosition>(file.getRawData<int64_t>(H5name{prefix+".word_pidx"}))},
  head_words{deserialize<VocaIndex>(file.getRawData<int64_t>(H5name{prefix+".head"}))},
  heads_uid{deserialize<WordUID>(file.getRawData<int64_t>(H5name{prefix+".head_uid"}))},
  heads_pidx{deserialize<WordPosition>(file.getRawData<int64_t>(H5name{prefix+".head_pidx"}))},
  words_beg{deserialize<CharOffset>(file.getRawData<int64_t>(H5name{prefix+".word_beg"}))},
  words_end{deserialize<CharOffset>(file.getRawData<int64_t>(H5name{prefix+".word_end"}))},
  poss{deserialize<POSUID>(file.getRawData<int64_t>(H5name{prefix+".pos_uid"}))},
  arclabels{deserialize<ArcLabelUID>(file.getRawData<int64_t>(H5name{prefix+".arclabel_uid"}))}
{}

void DepParsedTokens::write_to_disk(std::string filename, std::string prefix) const {
//    H5file outfile{H5name{filename}, hdf5::FileMode::rw_exist};
    H5file outfile{H5name{filename}, hdf5::FileMode::replace};
    outfile.writeRawData(H5name{prefix+".sent_uid"}, serialize(sents_uid));
    outfile.writeRawData(H5name{prefix+".chunk_idx"}, serialize(chunks_idx));
    outfile.writeRawData(H5name{prefix+".sent_idx"}, serialize(sents_idx));
    outfile.writeRawData(H5name{prefix+".word_uid"}, serialize(words_uid));
    outfile.writeRawData(H5name{prefix+".word"}, serialize(words));
    outfile.writeRawData(H5name{prefix+".word_pidx"},serialize(words_pidx));
    outfile.writeRawData(H5name{prefix+".head_uid"}, serialize(heads_uid));
    outfile.writeRawData(H5name{prefix+".head"}, serialize(head_words));
    outfile.writeRawData(H5name{prefix+".head_pidx"},serialize(heads_pidx));
    outfile.writeRawData(H5name{prefix+".word_beg"},serialize(words_beg));
    outfile.writeRawData(H5name{prefix+".word_end"},serialize(words_end));
    outfile.writeRawData(H5name{prefix+".pos_uid"},serialize(poss));
    outfile.writeRawData(H5name{prefix+".arclabel_uid"},serialize(arclabels));
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
                                            nlohmann::json const &output){

    int64_t sent_idx{0};
    for(auto const& sent_json : output["sentences"] ){
        auto offset = sents_idx.size();
        for(auto const &token : sent_json["tokens"]){
            //after before characterOffsetBegin characterOffsetEnd index originalText pos word
            auto after  = token["after"].get<std::string>();
            auto before = token["before"].get<std::string>();
            auto token_beg = token["characterOffsetBegin"].get<int64_t>();
            auto token_end = token["characterOffsetEnd"].get<int64_t>();
            auto word_pidx = token["index"].get<int64_t>()-1;
            auto originalText = token["originalText"].get<std::string>();
            auto pos = token["pos"].get<std::string>();
            auto word = token["word"].get<std::string>();


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
        }
        for(auto const &x : sent_json["basicDependencies"]){
            //dep dependent dependentGloss governor governorGloss
            auto word      = x["dependentGloss"].get<std::string>();
            auto word_pidx = x["dependent"].get<int64_t>()-1;
            auto head_word = x["governorGloss"].get<std::string>();
            auto head_pidx = x["governor"].get<int64_t>()-1;
            auto arc_label = x["dep"].get<std::string>();

            auto i= word_pidx;
            assert(words_uid[offset+i] == wordUIDs[word]);
            assert(words_pidx[offset+i]==WordPosition{word_pidx});
            heads_uid[offset+i] = wordUIDs[head_word];
            heads_pidx[offset+i] = WordPosition{head_pidx};
            arclabels[offset+i] = arclabelUIDs[arc_label];

        }
        ++sent_idx;
    }
    ++current_chunk_idx;
}

std::vector<SentUID>  DepParsedTokens::build_sent_uid(SentUID init_uid){
    auto n = sents_uid.size();
    auto beg=sents_idx.cbegin()+n;
    auto end=sents_idx.cend();
    auto chunk_beg=chunks_idx.cbegin()+n;
    auto it=beg;
    auto it_chunk=chunk_beg;
    decltype(sents_uid) new_uids;
    if(it==end) return new_uids;
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
    return new_uids;
}


}//namespace wordrep

