#include <vector>
#include <algorithm>
#include <src/parser/voca.h>

#include "fmt/printf.h"

#include "parser/voca.h"

#include "utils/json.h"
#include "utils/hdf5.h"
#include "utils/profiling.h"
#include "utils/span.h"
#include "utils/string.h"

using namespace util::io;

struct SentUIndex{
//    SentUIndex(int64_t val) : val{val}{}
    SentUIndex(std::ptrdiff_t val) : val{val}{}
    int64_t val;
};
struct WordUIndex{
//    WordUIndex(int64_t val) : val{val}{}
    WordUIndex(std::ptrdiff_t val) : val{val}{}
    int64_t val;
};
struct SentPosition{int64_t val;};
struct Sentence{
    Sentence(SentUIndex uid, WordUIndex beg, WordUIndex end)
    : uid{uid}, beg{beg}, end{end} {}
    SentUIndex uid;
    WordUIndex beg;
    WordUIndex end;
};

struct WordIndex{int64_t val;};
struct VocaIndex{int64_t val;};
struct ParsedWord{
    ParsedWord(util::io::H5file const &file, std::string prefix)
    : sent_idx{file.getRawData<int64_t>(H5name{prefix+".sent_idx"})},
      word_raw{file.getRawData<char>(H5name{prefix+".word"})},
      word{util::string::unpack_word_views(word_raw)},
      idx_word{file.getRawData<int64_t>(H5name{prefix+".word_pidx"})},
      head_word_raw{file.getRawData<char>(H5name{prefix+".head_word"})},
      head_word{util::string::unpack_word_views(head_word_raw)},
      idx_head{file.getRawData<int64_t>(H5name{prefix+".head_pidx"})},
      arc_label_raw{file.getRawData<char>(H5name{prefix+".arc_label"})},
      arc_label{util::string::unpack_word_views(arc_label_raw)}
    {}
    std::vector<int64_t>     sent_idx;
    std::vector<char>        word_raw;
    std::vector<const char*> word;
    std::vector<int64_t>     idx_word;
    std::vector<char>        head_word_raw;
    std::vector<const char*> head_word;
    std::vector<int64_t>     idx_head;
    std::vector<char>        arc_label_raw;
    std::vector<const char*> arc_label;
};

std::vector<int64_t> get_voca_idxs(rnn::wordrep::VocaIndexMap const &word2idx,
                                   std::vector<const char*> words){
    std::vector<int64_t> vidxs;
    for(auto x : words) {
        int64_t idx = word2idx.getIndex(rnn::wordrep::Word{x});
        vidxs.push_back(idx);
    }
    return vidxs;
}

struct ParsedWordIdx{
    ParsedWordIdx(ParsedWord const &words, rnn::wordrep::VocaIndexMap const &word2idx)
    : sent_idx{words.sent_idx},
      word{get_voca_idxs(word2idx, words.word)},
      word_pidx{words.idx_word},
      head_word{get_voca_idxs(word2idx, words.head_word)},
      head_pidx{words.idx_head},
      arc_label_raw{util::string::pack_words(words.arc_label)},
      arc_label{util::string::unpack_word_views(arc_label_raw)}
    {}
    ParsedWordIdx(util::io::H5file const &file, std::string prefix)
    : sent_idx{file.getRawData<int64_t>(H5name{prefix+".sent_idx"})},
      word{file.getRawData<int64_t>(H5name{prefix+".word"})},
      word_pidx{file.getRawData<int64_t>(H5name{prefix+".word_pidx"})},
      head_word{file.getRawData<int64_t>(H5name{prefix+".head_word"})},
      head_pidx{file.getRawData<int64_t>(H5name{prefix+".head_pidx"})},
      arc_label_raw{file.getRawData<char>(H5name{prefix+".arc_label"})},
      arc_label{util::string::unpack_word_views(arc_label_raw)}
    {}

    void write_to_disk(std::string filename, std::string prefix) const {
        H5file outfile{H5name{filename}, hdf5::FileMode::replace};
        outfile.writeRawData(H5name{prefix+".sent_idx"}, sent_idx);
        outfile.writeRawData(H5name{prefix+".word"},     word);
        outfile.writeRawData(H5name{prefix+".word_pidx"},word_pidx);
        outfile.writeRawData(H5name{prefix+".head_word"},head_word);
        outfile.writeRawData(H5name{prefix+".head_pidx"},head_pidx);
        outfile.writeRawData(H5name{prefix+".arc_label"},arc_label_raw);
    }

    std::vector<Sentence> SegmentSentences() const {
        auto beg=sent_idx.cbegin();
        auto end=sent_idx.cend();
        std::vector<Sentence> sents;
        auto it=beg;
        while(it!=end) {
            SentUIndex uid{*it};
            WordUIndex sbeg{it-beg};
            it = std::find_if_not(it, end, [it](auto x) { return x == *it; });
            WordUIndex send{it-beg};
            sents.push_back(Sentence{uid, sbeg, send});
        }
        return sents;
    }

    std::vector<int64_t>     sent_idx;
    std::vector<int64_t>     word;
    std::vector<int64_t>     word_pidx;
    std::vector<int64_t>     head_word;
    std::vector<int64_t>     head_pidx;
    std::vector<char>        arc_label_raw;
    std::vector<const char*> arc_label;
};

void convert_h5py_to_native(){
    auto voca = rnn::wordrep::load_voca("news.h5", "news.en.words");
    auto word2idx = voca.indexing();
    H5file infile{H5name{"news.Google.h5"}, hdf5::FileMode::read_exist};
    ParsedWord news{infile, "test"};
    ParsedWordIdx news_indexed{news, word2idx};
    news_indexed.write_to_disk("news.dep.h5", "test");
}
//TODO : arc_label index, Sent end/beg

struct DepParsedQuery{
    DepParsedQuery(nlohmann::json const &sent, rnn::wordrep::VocaIndexMap const &word2idx)
    : len{sent["basic-dependencies"].size()}, word(len), word_pidx(len), head_word(len), head_pidx(len), arc_label(len){
//        fmt::print("len : {}\n", len);
        for(auto const&x : sent["basic-dependencies"]) {
            auto i = x["dependent"].get<int64_t>() - 1;
            word[i] = word2idx.getIndex(rnn::wordrep::Word{x["dependentGloss"].get<std::string>()});
            word_pidx[i] = x["dependent"];
            head_word[i] = word2idx.getIndex(rnn::wordrep::Word{x["governorGloss"].get<std::string>()});
            head_pidx[i] = x["governor"];
            arc_label[i]= x["dep"];
//            fmt::print("{} {} {} {} {}\n", word[i], word_pidx[i], head_word[i], head_pidx[i], arc_label[i]);
        }
//        fmt::print("\n");
//        for(auto &x :sent["tokens"]) fmt::print("{} {}\n", x["pos"].get<std::string>(), x["word"].get<std::string>());
    }

    std::size_t len;
    std::vector<int64_t> word;
    std::vector<int64_t> word_pidx;
    std::vector<int64_t> head_word;
    std::vector<int64_t> head_pidx;
    std::vector<std::string> arc_label;
};
int main(){
//    convert_h5py_to_native();
    util::Timer timer{};
    auto voca = rnn::wordrep::load_voca("news.h5", "news.en.words");
    auto word2idx = voca.indexing();

    H5file infile{H5name{"news.dep.h5"}, hdf5::FileMode::read_exist};
    ParsedWordIdx news_indexed{infile, "test"};
    timer.here_then_reset("Data loaded.");
    std::vector<Sentence> sents = news_indexed.SegmentSentences();

    timer.here_then_reset("Sentences are reconstructed.\nEngine is ready.");

    auto query_json = R"({"cutoffs": [[0.8, 0.0, 1.0, 0.7, 0.0]], "sentences": [{"tokens": [{"index": 1, "word": "Startups", "after": " ", "pos": "NNS", "characterOffsetEnd": 8, "characterOffsetBegin": 0, "originalText": "Startups", "before": ""}, {"index": 2, "word": "that", "after": " ", "pos": "WDT", "characterOffsetEnd": 13, "characterOffsetBegin": 9, "originalText": "that", "before": " "}, {"index": 3, "word": "Google", "after": " ", "pos": "NNP", "characterOffsetEnd": 20, "characterOffsetBegin": 14, "originalText": "Google", "before": " "}, {"index": 4, "word": "bought", "after": "", "pos": "VBD", "characterOffsetEnd": 27, "characterOffsetBegin": 21, "originalText": "bought", "before": " "}], "index": 0, "basic-dependencies": [{"dep": "ROOT", "dependent": 4, "governorGloss": "ROOT", "governor": 0, "dependentGloss": "bought"}, {"dep": "dobj", "dependent": 1, "governorGloss": "bought", "governor": 4, "dependentGloss": "Startups"}, {"dep": "det", "dependent": 2, "governorGloss": "Google", "governor": 3, "dependentGloss": "that"}, {"dep": "nsubj", "dependent": 3, "governorGloss": "bought", "governor": 4, "dependentGloss": "Google"}], "parse": "SENTENCE_SKIPPED_OR_UNPARSABLE", "collapsed-dependencies": [{"dep": "ROOT", "dependent": 4, "governorGloss": "ROOT", "governor": 0, "dependentGloss": "bought"}, {"dep": "dobj", "dependent": 1, "governorGloss": "bought", "governor": 4, "dependentGloss": "Startups"}, {"dep": "det", "dependent": 2, "governorGloss": "Google", "governor": 3, "dependentGloss": "that"}, {"dep": "nsubj", "dependent": 3, "governorGloss": "bought", "governor": 4, "dependentGloss": "Google"}], "collapsed-ccprocessed-dependencies": [{"dep": "ROOT", "dependent": 4, "governorGloss": "ROOT", "governor": 0, "dependentGloss": "bought"}, {"dep": "dobj", "dependent": 1, "governorGloss": "bought", "governor": 4, "dependentGloss": "Startups"}, {"dep": "det", "dependent": 2, "governorGloss": "Google", "governor": 3, "dependentGloss": "that"}, {"dep": "nsubj", "dependent": 3, "governorGloss": "bought", "governor": 4, "dependentGloss": "Google"}]}]})"_json;
    nlohmann::json& sent_json = query_json["sentences"][0];
    std::vector<double> cutoff = query_json["cutoffs"][0];
    DepParsedQuery query{sent_json, word2idx};

    auto google_idx = word2idx.getIndex(rnn::wordrep::Word{"Google"});
    auto bought_idx = word2idx.getIndex(rnn::wordrep::Word{"bought"});
    auto startup_idx = word2idx.getIndex(rnn::wordrep::Word{"startup"});
//    for(decltype(n_sent) sent_idx=0; sent_idx!=n_sent;++sent_idx) {
    for(auto sent: sents){
        auto beg=sent.beg.val;
        auto end=sent.end.val;
        //find google bought
        bool google_bought = false;
        bool startup = false;
        for(auto i=beg; i<end; ++i) {   
            if(news_indexed.word[i] == google_idx && news_indexed.head_word[i] == bought_idx)
                google_bought = true;
            if( news_indexed.word[i]==startup_idx)
                startup = true;
        }
        if(google_bought && startup)
            fmt::print("{:<10}\n", sent.uid.val);
    }
    timer.here_then_reset("Queries are answered.");
    return 0;

//    for(decltype(n_sent) sent_idx=0; sent_idx!=n_sent;++sent_idx){
//        auto beg=sent_beg[sent_idx];
//        auto end=sent_end[sent_idx];
//        for(auto i=beg; i<end; ++i) {
//            fmt::print("{:<10} {:<10} {:<10} {:<10} : ", sent_idx, i, beg, end);
////            fmt::print("{:<10} ", sent_idx);
//            fmt::print("{:<10} {:<2}  {:<10} {:<2}  {}\n",
//                       voca.getWord(news_indexed.word[i]).val, news_indexed.word_pidx[i],
//                       voca.getWord(news_indexed.head_word[i]).val,
//                       news_indexed.head_pidx[i], news_indexed.arc_label[i]);
//        }
//        fmt::print("{}\n",end-beg);
//    }

    return 0;
}
