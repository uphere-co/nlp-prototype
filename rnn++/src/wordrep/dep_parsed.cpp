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

    auto offset=sent.beg.val;
    auto reversed_offset=n_tokens()-offset;
    auto chk_idx = chunk_idx(sent.beg);
//    auto chk_beg = std::find_if_not(chunks_idx.crbegin()+reversed_offset, chunks_idx.crend(),
//                                [chk_idx](auto x) { return x == chk_idx; }).base();
//    auto chk_end = std::find_if_not(chunks_idx.cbegin()+offset, chunks_idx.cend(),
//                                [chk_idx](auto x) { return x == chk_idx; });
    auto first_elm=DPTokenIndex{DPTokenIndex::val_t{0}};
    auto last_elm =DPTokenIndex::from_unsigned(n_tokens());
    auto chk_beg = sent.beg;
    for(;chk_beg!=first_elm; --chk_beg)
        if(chunk_idx(chk_beg)!=chk_idx) break;
    auto chk_end = sent.end;
    for(;chk_end!=last_elm; ++chk_end)
        if(chunk_idx(chk_end)!=chk_idx) break;
    //std::cerr<<fmt::format("Chunk : {} of {}", chk_beg.val, chk_end.val)<<std::endl;
    assert(chunk_idx(chk_beg)!=chk_idx);
    chk_beg++;
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

namespace ygp{
YGPindexer::YGPindexer(util::io::H5file const &file, std::string prefix)
        : chunk2idx{util::deserialize<RowIndex>(file.getRawData<int64_t>(H5name{prefix+".chunk2row_idx"}))},
          chunk2row_uid{util::deserialize<RowUID>(file.getRawData<int64_t>(H5name{prefix+".chunk2row"}))},
          chunk2col_uid{util::deserialize<ColumnUID>(file.getRawData<int64_t>(H5name{prefix+".chunk2col"}))}
{
    auto n = chunk2idx.size();
    assert(chunk2row_uid.size()==n);
    assert(chunk2col_uid.size()==n);
    //for(decltype(n)i=0; i!=n; ++i) {
    for(auto it=chunk2idx.cbegin(); it!=chunk2idx.cend(); ){
        auto i = std::distance(chunk2idx.cbegin(), it);
        auto row_idx=*it;
        auto row_uid=chunk2row_uid[i];
        auto col_uid=chunk2col_uid[i];
        map_to_uid[{col_uid,row_idx}]=row_uid;
        it = std::find_if_not(it, chunk2idx.cend(), [it](auto x){return x==*it;});
    }
}

CountryCodeAnnotator::CountryCodeAnnotator(std::string country_list){
    auto countries = util::string::readlines(country_list);
    for(auto const& country : countries) codes[country].push_back(country);
    codes["Korea"].push_back("South Korea");
    codes["China"].push_back("Hong Kong");
}
std::vector<std::string> CountryCodeAnnotator::tag(std::string content) const{
    std::vector<std::string> countries;
    for(auto const& it : codes){
        auto n = content.find(it.first);
        if(n==decltype(content)::npos) continue;
        util::append(countries, it.second);
    }
    return countries;
}

DBbyCountry::DBbyCountry(util::io::H5file const &file, std::string country_list){
    auto countries =util::string::readlines(country_list);
    for(auto country : countries) {
    auto rows=util::deserialize<ygp::RowUID>(file.getRawData<int64_t>(H5name{country+".row_uid"}));
    auto sents=util::deserialize<SentUID>(file.getRawData<int64_t>(H5name{country+".sent_uid"}));
    rows_by_country[country]=rows;
    sents_by_country[country]=sents;
    }
}

YGPdb::YGPdb(std::string column_uids){
    auto lines = util::string::readlines(column_uids);
    assert(ColumnUID{}==ColumnUID{0});
    for(auto line : lines){
        auto cols = util::string::split(line, ".");
        tables.push_back(cols[0]);
        columns.push_back(cols[1]);
        index_cols.push_back(cols[2]);
    }
}

std::string YGPdb::raw_text(ColumnUID col_uid, RowIndex idx) const{
    pqxx::connection C{"dbname=C291145_gbi_test host=bill.uphere.he"};
    pqxx::work W(C);
    auto query=fmt::format("SELECT {} FROM {} where {}={};",
                           column(col_uid), table(col_uid), index_col(col_uid), idx.val);
    auto body= W.exec(query);
    W.commit();
    return body[0][0].c_str();
}

void annotation_on_result(nlohmann::json const &config, nlohmann::json &answers){
    ygp::YGPdb ygpdb{config["column_uids_dump"].get<std::string>()};
    for(auto &answer : answers){
        //answer["result_sent_uid"].push_back(sent.uid.val);
        //answer["result_row_uid"].push_back(row_uid.val);
        auto col_uids = answer["result_column_uid"];
        auto row_idxs = answer["result_row_idx"];
        auto offsets = answer["result_offset"];
        auto n = col_uids.size();
        for(decltype(n)i=0; i!=n; ++i){
            ygp::ColumnUID col_uid{col_uids[i].get<ygp::ColumnUID::val_t>()};
            ygp::RowIndex  row_idx{row_idxs[i].get<ygp::RowIndex::val_t>()};
            auto offset_beg = offsets[i][0].get<int64_t>();
            auto offset_end = offsets[i][1].get<int64_t>();

            auto row_str = ygpdb.raw_text(col_uid, row_idx);
            auto substr = util::string::substring_unicode_offset(row_str, offset_beg, offset_end);
            answer["result_DEBUG"].push_back(substr);
            answer["result_row_DEBUG"].push_back(row_str);
        }
    }
}

}//namespace ygp


}//namespace wordrep


