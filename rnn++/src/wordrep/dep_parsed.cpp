#include <algorithm>

#include "wordrep/dep_parsed.h"

#include "utils/hdf5.h"
#include "utils/string.h"

using namespace util;
using namespace util::io;

namespace wordrep{
DepParsedTokens::DepParsedTokens(util::io::H5file const &file, std::string prefix)
: sents_uid{deserialize<SentUID>(file.getRawData<int64_t>(H5name{prefix+".sent_uid"}))},
//  chunks_idx{deserialize<ChunkIndex>(file.getRawData<int64_t>(H5name{prefix+".chunk_idx"}))},
  sents_idx{deserialize<SentPosition>(file.getRawData<int64_t>(H5name{prefix+".sent_idx"}))},
  words_uid{deserialize<WordUID>(file.getRawData<int64_t>(H5name{prefix+".word_uid"}))},
  words{deserialize<VocaIndex>(file.getRawData<int64_t>(H5name{prefix+".word"}))},
  words_pidx{deserialize<WordPosition>(file.getRawData<int64_t>(H5name{prefix+".word_pidx"}))},
  heads_uid{deserialize<WordUID>(file.getRawData<int64_t>(H5name{prefix+".head_uid"}))},
  head_words{deserialize<VocaIndex>(file.getRawData<int64_t>(H5name{prefix+".head"}))},
  heads_pidx{deserialize<WordPosition>(file.getRawData<int64_t>(H5name{prefix+".head_pidx"}))},
//  words_beg{deserialize<CharOffset>(file.getRawData<int64_t>(H5name{prefix+".word_beg"}))},
//  words_end{deserialize<CharOffset>(file.getRawData<int64_t>(H5name{prefix+".word_end"}))},
  poss{deserialize<POSUID>(file.getRawData<int64_t>(H5name{prefix+".pos_uid"}))},
  arclabels{deserialize<ArcLabelUID>(file.getRawData<int64_t>(H5name{prefix+".arclabel_uid"}))}
{}

void DepParsedTokens::write_to_disk(std::string filename, std::string prefix) const {
//    H5file outfile{H5name{filename}, hdf5::FileMode::rw_exist};
    H5file outfile{H5name{filename}, hdf5::FileMode::replace};
//    outfile.writeRawData(H5name{prefix+".sent_uid"}, serialize(sents_uid));
    outfile.writeRawData(H5name{prefix+".chunk_idx"}, serialize(chunks_idx));
    outfile.writeRawData(H5name{prefix+".sent_idx"}, serialize(sents_idx));
    outfile.writeRawData(H5name{prefix+".word_uid"}, serialize(words_uid));
//    outfile.writeRawData(H5name{prefix+".word"}, serialize(words));
    outfile.writeRawData(H5name{prefix+".word_pidx"},serialize(words_pidx));
    outfile.writeRawData(H5name{prefix+".head_uid"}, serialize(heads_uid));
//    outfile.writeRawData(H5name{prefix+".head"}, serialize(head_words));
    outfile.writeRawData(H5name{prefix+".head_pidx"},serialize(heads_pidx));
    outfile.writeRawData(H5name{prefix+".word_beg"},serialize(words_beg));
    outfile.writeRawData(H5name{prefix+".word_end"},serialize(words_end));
    outfile.writeRawData(H5name{prefix+".pos_uid"},serialize(poss));
    outfile.writeRawData(H5name{prefix+".arclabel_uid"},serialize(arclabels));
}

std::vector<Sentence> DepParsedTokens::SegmentSentences() const {
    auto beg=sents_uid.cbegin();
    auto end=sents_uid.cend();
    std::vector<Sentence> sents;
    auto it=beg;
    while(it!=end) {
        SentUID uid{*it};
        DPTokenIndex sbeg{it-beg};
        it = std::find_if_not(it, end, [it](auto x) { return x == *it; });
        DPTokenIndex send{it-beg};
        sents.push_back(Sentence{uid, sbeg, send});
    }
    return sents;
}

void DepParsedTokens::append_corenlp_output(nlohmann::json const &output){
    WordUIDindex wordUIDs{"/home/jihuni/word2vec/ygp/words.uid"};
    POSUIDindex posUIDs{"/home/jihuni/word2vec/ygp/pos.uid"};
    ArcLabelUIDindex arclabelUIDs{"/home/jihuni/word2vec/ygp/dep.uid"};

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
            sents_idx.push_back(SentPosition{sent_idx});
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
        for(auto const &x : sent_json["basic-dependencies"]){
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
}//namespace wordrep

