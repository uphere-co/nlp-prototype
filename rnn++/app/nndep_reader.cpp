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

struct ParsedWord{
    ParsedWord(util::io::H5file const &file, std::string prefix)
    : sent_idx{file.getRawData<int64_t>(H5name{prefix+".sent_idx"})},
      word_raw{file.getRawData<char>(H5name{prefix+".word"})},
      word{util::string::unpack_word_views(word_raw)},
      idx_word{file.getRawData<int64_t>(H5name{prefix+".word_pidx"})},
      head_word_raw{file.getRawData<char>(H5name{prefix+".head_word"})},
      head_word{util::string::unpack_word_views(head_word_raw)},
      idx_head{file.getRawData<int64_t>(H5name{prefix+".head_pidx"})},
      pos_raw{file.getRawData<char>(H5name{prefix+".POS"})},
      pos{util::string::unpack_word_views(pos_raw)}
    {}
    std::vector<int64_t>     sent_idx;
    std::vector<char>        word_raw;
    std::vector<const char*> word;
    std::vector<int64_t>     idx_word;
    std::vector<char>        head_word_raw;
    std::vector<const char*> head_word;
    std::vector<int64_t>     idx_head;
    std::vector<char>        pos_raw;
    std::vector<const char*> pos;
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
      pos_raw{util::string::pack_words(words.pos)},
      pos{util::string::unpack_word_views(pos_raw)}
    {}
    ParsedWordIdx(util::io::H5file const &file, std::string prefix)
    : sent_idx{file.getRawData<int64_t>(H5name{prefix+".sent_idx"})},
      word{file.getRawData<int64_t>(H5name{prefix+".word"})},
      word_pidx{file.getRawData<int64_t>(H5name{prefix+".word_pidx"})},
      head_word{file.getRawData<int64_t>(H5name{prefix+".head_word"})},
      head_pidx{file.getRawData<int64_t>(H5name{prefix+".head_pidx"})},
      pos_raw{file.getRawData<char>(H5name{prefix+".POS"})},
      pos{util::string::unpack_word_views(pos_raw)}
    {}

    void write_to_disk(std::string filename, std::string prefix) const {
        H5file outfile{H5name{filename}, hdf5::FileMode::replace};
        outfile.writeRawData(H5name{prefix+".sent_idx"}, sent_idx);
        outfile.writeRawData(H5name{prefix+".word"},     word);
        outfile.writeRawData(H5name{prefix+".word_pidx"},word_pidx);
        outfile.writeRawData(H5name{prefix+".head_word"},head_word);
        outfile.writeRawData(H5name{prefix+".head_pidx"},head_pidx);
        outfile.writeRawData(H5name{prefix+".POS"},      pos_raw);
    }

    std::vector<int64_t>     sent_idx;
    std::vector<int64_t>     word;
    std::vector<int64_t>     word_pidx;
    std::vector<int64_t>     head_word;
    std::vector<int64_t>     head_pidx;
    std::vector<char>        pos_raw;
    std::vector<const char*> pos;
};

//TODO : POS index, Sent end/beg

int main(){
    util::Timer timer{};
    auto voca = rnn::wordrep::load_voca("news.h5", "news.en.words");
    auto word2idx = voca.indexing();

//    H5file infile{H5name{"news.Google.h5"}, hdf5::FileMode::read_exist};
//    ParsedWord news{infile, "test"};
//    ParsedWordIdx news_indexed{news, word2idx};
//    news_indexed.write_to_disk("news.dep.h5", "test");
    H5file infile{H5name{"news.dep.h5"}, hdf5::FileMode::read_exist};
    ParsedWordIdx news_indexed{infile, "test"};

    auto beg=news_indexed.sent_idx.cbegin();
    auto end=news_indexed.sent_idx.cend();
    std::vector<int64_t> sent_beg{};
    std::vector<int64_t> sent_end{};
    auto it=beg;
    while(it!=end) {
        sent_beg.push_back(it-beg);
        it = std::find_if_not(it, end, [it](auto x) { return x == *it; });
        sent_end.push_back(it-beg);
    }
    timer.here_then_reset("Engine is ready.");

    auto n_sent = sent_beg.size();
    auto google_idx = word2idx.getIndex(rnn::wordrep::Word{"Google"});
    auto bought_idx = word2idx.getIndex(rnn::wordrep::Word{"bought"});
    auto startup_idx = word2idx.getIndex(rnn::wordrep::Word{"startup"});
    for(decltype(n_sent) sent_idx=0; sent_idx!=n_sent;++sent_idx) {
        auto beg=sent_beg[sent_idx];
        auto end=sent_end[sent_idx];
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
            fmt::print("{:<10}\n", sent_idx);
    }
    timer.here_then_reset("Queries are answered.");
    return 0;

    for(decltype(n_sent) sent_idx=0; sent_idx!=n_sent;++sent_idx){
        auto beg=sent_beg[sent_idx];
        auto end=sent_end[sent_idx];
        for(auto i=beg; i<end; ++i) {
            fmt::print("{:<10} {:<10} {:<10} {:<10} : ", sent_idx, i, beg, end);
//            fmt::print("{:<10} ", sent_idx);
            fmt::print("{:<10} {:<2}  {:<10} {:<2}  {}\n",
                       voca.getWord(news_indexed.word[i]).val, news_indexed.word_pidx[i],
                       voca.getWord(news_indexed.head_word[i]).val,
                       news_indexed.head_pidx[i], news_indexed.pos[i]);
        }
        fmt::print("{}\n",end-beg);
    }

    return 0;
}