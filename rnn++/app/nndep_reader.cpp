#include <vector>
#include <algorithm>
#include <src/parser/voca.h>

#include "fmt/printf.h"

#include "parser/voca.h"

#include "utils/json.h"
#include "utils/hdf5.h"
#include "utils/span.h"
#include "utils/string.h"

using namespace util::io;

struct ParsedWord{
    ParsedWord(util::io::H5file const &file, std::string prefix)
    : sent_idx{file.getRawData<int64_t>(H5name{prefix+".sent_idx"})},
      word_raw{file.getRawData<char>(H5name{prefix+".word"})},
      word{util::string::unpack_word_views(word_raw)},
      idx_word{file.getRawData<int64_t>(H5name{prefix+".idx_word"})},
      head_word_raw{file.getRawData<char>(H5name{prefix+".head_word"})},
      head_word{util::string::unpack_word_views(head_word_raw)},
      idx_head{file.getRawData<int64_t>(H5name{prefix+".idx_head"})},
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
      idx_word{words.idx_word},
      head_word{get_voca_idxs(word2idx, words.head_word)},
      idx_head{words.idx_head},
      pos_raw{util::string::pack_words(words.pos)},
      pos{util::string::unpack_word_views(pos_raw)}
    {}
    ParsedWordIdx(util::io::H5file const &file, std::string prefix)
    : sent_idx{file.getRawData<int64_t>(H5name{prefix+".sent_idx"})},
      word{file.getRawData<int64_t>(H5name{prefix+".word"})},
      idx_word{file.getRawData<int64_t>(H5name{prefix+".idx_word"})},
      head_word{file.getRawData<int64_t>(H5name{prefix+".head_word"})},
      idx_head{file.getRawData<int64_t>(H5name{prefix+".idx_head"})},
      pos_raw{file.getRawData<char>(H5name{prefix+".POS"})},
      pos{util::string::unpack_word_views(pos_raw)}
    {}

    void write_to_disk(std::string filename, std::string prefix) const {
        H5file outfile{H5name{filename}, hdf5::FileMode::replace};
        outfile.writeRawData(H5name{prefix+".sent_idx"}, sent_idx);
        outfile.writeRawData(H5name{prefix+".word"},     word);
        outfile.writeRawData(H5name{prefix+".idx_word"}, idx_word);
        outfile.writeRawData(H5name{prefix+".head_word"},head_word);
        outfile.writeRawData(H5name{prefix+".idx_head"}, idx_head);
        outfile.writeRawData(H5name{prefix+".POS"},      pos_raw);
    }

    std::vector<int64_t>     sent_idx;
    std::vector<int64_t>     word;
    std::vector<int64_t>     idx_word;
    std::vector<int64_t>     head_word;
    std::vector<int64_t>     idx_head;
    std::vector<char>        pos_raw;
    std::vector<const char*> pos;
};


int main(){
//    H5file infile{H5name{"news.Google.h5"}, hdf5::FileMode::read_exist};
//    ParsedWord news{infile, "test"};
//    ParsedWordIdx news_indexed{news, word2idx};
//    news_indexed.write_to_disk("news.dep.h5", "test");
    auto voca = rnn::wordrep::load_voca("news.h5", "news.en.words");
    auto word2idx = voca.indexing();
    H5file infile{H5name{"news.dep.h5"}, hdf5::FileMode::read_exist};
    ParsedWordIdx news_indexed{infile, "test"};

    auto beg=news_indexed.sent_idx.cbegin();
    auto end=news_indexed.sent_idx.cend();
    std::vector<int64_t> sent_beg{0};
    auto it=beg;
    while(it!=end) {
        it = std::find_if_not(it, end, [it](auto x) { return x == *it; });
        sent_beg.push_back(it-beg);
    }

    for(int64_t is=0; is<sent_beg.size()-1; ++is){
        auto beg=sent_beg[is];
        auto end=sent_beg[is+1];
        for(auto i=beg; i<end; ++i) {
            fmt::print("{} : ", i);
            fmt::print("{:<10} {:<2}  {:<10} {:<2}  {}\n",
                       voca.getWord(news_indexed.word[i]).val, news_indexed.idx_word[i],
                       voca.getWord(news_indexed.head_word[i]).val,
                       news_indexed.idx_head[i], news_indexed.pos[i]);
        }
        fmt::print("{}\n",end-beg);
    }

    return 0;
}