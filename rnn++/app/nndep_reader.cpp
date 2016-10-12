#include <vector>
#include <algorithm>
#include <src/parser/voca.h>

#include "fmt/printf.h"

#include "parser/voca.h"
#include "parser/parser.h"

#include "utils/json.h"
#include "utils/hdf5.h"
#include "utils/profiling.h"
#include "utils/span.h"
#include "utils/string.h"

using namespace util::io;

struct SentUIndex{
    SentUIndex(std::ptrdiff_t val) : val{val}{}
    int64_t val;
};
struct WordUIndex{
    WordUIndex(std::ptrdiff_t val) : val{val}{}
    int64_t val;
};
struct SentPosition{int64_t val;};
struct Sentence{
    Sentence(SentUIndex uid, WordUIndex beg, WordUIndex end)
    : uid{uid}, beg{beg}, end{end}{}
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
    DepParsedQuery(std::vector<double> const &cutoff, nlohmann::json const &sent, rnn::wordrep::VocaIndexMap const &word2idx)
    : len{cutoff.size()}, cutoff{cutoff}, word(len), word_pidx(len), head_word(len), head_pidx(len), arc_label(len){
//        fmt::print("len : {}\n", len);
        for(auto const&x : sent["basic-dependencies"]) {
            auto i = x["dependent"].get<int64_t>() - 1;
            word[i] = word2idx.getIndex(rnn::wordrep::Word{x["dependentGloss"].get<std::string>()});
            word_pidx[i] = x["dependent"];
            head_word[i] = word2idx.getIndex(rnn::wordrep::Word{x["governorGloss"].get<std::string>()});
            head_pidx[i] = x["governor"];
            arc_label[i]= x["dep"];
//            fmt::print("{} {} {} {} {}, {}\n", word[i], word_pidx[i], head_word[i], head_pidx[i], arc_label[i], cutoff[i]);
        }
//        fmt::print("\n");
//        for(auto &x :sent["tokens"]) fmt::print("{} {}\n", x["pos"].get<std::string>(), x["word"].get<std::string>());
    }

    bool is_similar(Sentence const &sent, ParsedWordIdx const &words) const {
        std::vector<bool> is_found(len, false);
        auto beg=sent.beg.val;
        auto end=sent.end.val;
        for(auto i=beg; i<end; ++i) {
            auto query_word = words.word[i];
            auto query_head = words.head_word[i];
            for(decltype(len)j=0; j<len; ++j){
                if(cutoff[j]<1.0){
                    if(word[j]==query_word) is_found[j] = true;
                } else {
                    if(word[j]==query_word && head_word[j]==query_head) is_found[j] = true;
                }
            }
        }
        for(decltype(len)j=0; j<len; ++j){
            if(cutoff[j]==0.0) is_found[j] = true;
        }
        return std::all_of(is_found.cbegin(), is_found.cend(), [](bool i){ return i;});
    }

    std::size_t len;
    std::vector<double> cutoff;
    std::vector<int64_t> word;
    std::vector<int64_t> word_pidx;
    std::vector<int64_t> head_word;
    std::vector<int64_t> head_pidx;
    std::vector<std::string> arc_label;
};

struct DepParseSearch{
    using json_t = nlohmann::json;
    using voca_info_t = rnn::simple_model::VocaInfo;
    DepParseSearch(json_t const &config)
    : rnn{config["wordvec_store"], config["voca_name"], config["w2vmodel_name"],
          util::datatype_from_string(config["w2v_float_t"])},
      words{H5file{H5name{config["dep_parsed_store"].get<std::string>()},
                          hdf5::FileMode::read_exist}, config["dep_parsed_text"]},
      sents{words.SegmentSentences()},
      sents_plain{util::string::readlines(config["plain_text"])}
    {}
    json_t process_queries(json_t ask) const {
        nlohmann::json& sent_json = ask["sentences"][0];
        std::vector<double> cutoff = ask["cutoffs"][0];
        DepParsedQuery query{cutoff, sent_json, rnn.word2idx};

        json_t answer{};
        for(auto sent: sents){
            if( query.is_similar(sent, words)) {
                answer["simiar_sents"].push_back(sents_plain[sent.uid.val]);
            }
        }
        return answer;
    }
    voca_info_t rnn;
    ParsedWordIdx words;
    std::vector<Sentence> sents;
    std::vector<std::string> sents_plain;

};

int main(int /*argc*/, char** argv){
//    convert_h5py_to_native();
    auto config = util::load_json(argv[1]);
    util::Timer timer{};
    DepParseSearch engine{config};
    timer.here_then_reset("Data loaded.");
    auto query_json = R"({"cutoffs": [[0.8, 0.0, 1.0, 0.7]], "sentences": [{"tokens": [{"index": 1, "word": "startup", "after": " ", "pos": "NN", "characterOffsetEnd": 7, "characterOffsetBegin": 0, "originalText": "startup", "before": ""}, {"index": 2, "word": "that", "after": " ", "pos": "WDT", "characterOffsetEnd": 12, "characterOffsetBegin": 8, "originalText": "that", "before": " "}, {"index": 3, "word": "Google", "after": " ", "pos": "NNP", "characterOffsetEnd": 19, "characterOffsetBegin": 13, "originalText": "Google", "before": " "}, {"index": 4, "word": "bought", "after": "", "pos": "VBD", "characterOffsetEnd": 26, "characterOffsetBegin": 20, "originalText": "bought", "before": " "}], "index": 0, "basic-dependencies": [{"dep": "ROOT", "dependent": 4, "governorGloss": "ROOT", "governor": 0, "dependentGloss": "bought"}, {"dep": "dobj", "dependent": 1, "governorGloss": "bought", "governor": 4, "dependentGloss": "startup"}, {"dep": "det", "dependent": 2, "governorGloss": "Google", "governor": 3, "dependentGloss": "that"}, {"dep": "nsubj", "dependent": 3, "governorGloss": "bought", "governor": 4, "dependentGloss": "Google"}], "parse": "SENTENCE_SKIPPED_OR_UNPARSABLE", "collapsed-dependencies": [{"dep": "ROOT", "dependent": 4, "governorGloss": "ROOT", "governor": 0, "dependentGloss": "bought"}, {"dep": "dobj", "dependent": 1, "governorGloss": "bought", "governor": 4, "dependentGloss": "startup"}, {"dep": "det", "dependent": 2, "governorGloss": "Google", "governor": 3, "dependentGloss": "that"}, {"dep": "nsubj", "dependent": 3, "governorGloss": "bought", "governor": 4, "dependentGloss": "Google"}], "collapsed-ccprocessed-dependencies": [{"dep": "ROOT", "dependent": 4, "governorGloss": "ROOT", "governor": 0, "dependentGloss": "bought"}, {"dep": "dobj", "dependent": 1, "governorGloss": "bought", "governor": 4, "dependentGloss": "startup"}, {"dep": "det", "dependent": 2, "governorGloss": "Google", "governor": 3, "dependentGloss": "that"}, {"dep": "nsubj", "dependent": 3, "governorGloss": "bought", "governor": 4, "dependentGloss": "Google"}]}]})"_json;
    auto answer = engine.process_queries(query_json);
    timer.here_then_reset("Queries are answered.");
    fmt::print("{}\n", answer.dump(4));
    return 0;
}
