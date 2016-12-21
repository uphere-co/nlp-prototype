#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <cassert>
#include <cctype>
#include <iterator>

#include <xxhashct/xxh64.hpp>
#include <fmt/printf.h>

#include "wordrep/word_uid.h"

#include "utils/string.h"
#include "utils/json.h"
#include "utils/algorithm.h"
#include "utils/profiling.h"
#include "utils/span.h"
#include "utils/parallel.h"

using wordrep::WordUIDindex;
using util::Timer;


template<typename T>
auto hash(T* ptr, size_t len){
    return xxh64::hash(reinterpret_cast<const char*>(ptr), len, 113377);
}

template<typename KEY>
struct TokenHash{
    template<typename T>
    KEY operator() (std::string const &text, T text_beg, T beg, T end) const;
};
template<>
struct TokenHash<std::string>{
    template<typename T>
    std::string operator() (std::string const &text, T text_beg, T beg, T end) const {
        return text.substr(beg-text_beg, end-beg);
    }
    std::string operator() (std::string const &word) const {
        return word;
    }
};
template<>
struct TokenHash<uint64_t>{
    template<typename T>
    uint64_t operator() (std::string const &text, T text_beg, T beg, T end) const {
        return hash(text.data()+(beg-text_beg), end-beg);
    }
    uint64_t operator() (std::string const &word) const {
        return hash(word.data(), std::distance(word.cend(),word.cbegin()));
    }
};

template<typename KEY>
struct WordIterBase{
    using key_type    = KEY;
    using hasher_type = TokenHash<KEY>;
    WordIterBase(std::string text)
            : text_strs{std::move(text)}
    {}
    template<typename OP>
    void iter(OP const &op) const {
        auto text_beg = std::cbegin(text_strs);
        auto text_end = std::cend(text_strs);
        auto beg = std::find_if_not(text_beg, text_end, [](auto x){return std::isspace(x);});
        while(beg!=text_end){
            auto end=std::find_if(beg, text_end, [](auto x){return std::isspace(x);});
            auto word = get_hash(text_strs, text_beg, beg, end);
            op(word);
            if(end==text_end) break;
            beg = std::find_if_not(end, text_end, [](auto x){return std::isspace(x);});
        }
    }

    hasher_type get_hash{};
    std::string text_strs;
};

using WordIter=WordIterBase<uint64_t>;
//using WordIter=WordIterBase<std::string>;
using count_t = std::map<WordIter::key_type, size_t>;

template<typename T>
std::optional<std::vector<char>> read_chunk(T &is, int64_t n_buf){
    std::vector<char> buffer(n_buf);
    is.read(buffer.data(), buffer.size());
    if(!is.gcount()) return {};
    if(is.gcount()==n_buf){
        for(char c=is.get(); is&&c!='\n'; c=is.get())
            buffer.push_back(c);
    }
    buffer.push_back('\0');
    return buffer;
}

class WordCounter{
public:
    using count_type = count_t;
    using map_t = tbb::concurrent_hash_map<count_type::key_type, size_t>;
    void count(std::string const &str){
        WordIter text{str};
        count_type word_counts;
        text.iter([&word_counts](auto& word) {++word_counts[word];});
        for(auto const& elm : word_counts){
            map_t::accessor a;
            wcs.insert(a, elm.first);
            a->second += elm.second;
        }
    }
    count_type get() const {
        Timer timer;
        count_type word_counts;
        for(auto const& elm : wcs){
            word_counts[elm.first] = elm.second;
        }
        timer.here_then_reset("Serialization to std::map is finished.");
        return word_counts;
    };

private:
    map_t wcs;
};

template<typename T>
WordCounter::count_type word_count(T&& is){
    WordCounter counter;
    tbb::task_group g;
    Timer timer;
    while (auto buffer=read_chunk(is, 200000)) {
        std::string str{buffer.value().data()};
        g.run([&counter,str](){ //important to copy the str variable.
            counter.count(str);
        });
    }
    g.wait();
    timer.here_then_reset("Word counting is finished.");
    return counter.get();
}

namespace test{

void string_iterator(){
    WordIterBase<std::string> text{"11 22 33\t\t14 15\n16 17 18   119\t \n 11110\n"};
    std::vector<int64_t> xs{11,22,33,14,15,16,17,18,119,11110};

    std::vector<int64_t> tokens;
    text.iter([&tokens](auto& word){tokens.push_back(std::stoi(word));});
    assert(tokens.size()==xs.size());
    for(auto pair : util::zip(xs,tokens)) assert(pair.first==pair.second);
}

std::string strip(std::string const& str){
    auto beg=std::find_if_not(str.cbegin(), str.cend(), [](auto x){return std::isspace(x);});
    auto end=std::find_if_not(str.crbegin(), str.crend(), [](auto x){return std::isspace(x);});
    return str.substr(beg-str.cbegin(), end.base()-beg);
}

void benchmark(){
    util::Timer timer{};
    auto lines = util::string::readlines("../rnn++/tests/data/sentence.2.corenlp");
    timer.here_then_reset("Finish file readlines.");


    count_t word_counts;
    for(auto &line : lines){
        WordIter text{line};
        text.iter([&word_counts](auto& word){++word_counts[word];});
    }
    timer.here_then_reset("Finish word count / excluding file reading.");
    auto word_counts2 = word_count(std::fstream{"../rnn++/tests/data/sentence.2.corenlp"});
    timer.here_then_reset("Finish parallel word count / including file reading.");

    typename WordIter::hasher_type hasher{};
    count_t word_counts0;
    for(auto &line : lines){
        auto stripped_line = strip(line);
        assert(!stripped_line.empty());
        for(auto&& word : util::string::split(stripped_line, " ")){
            ++word_counts0[hasher(word)];
        }
    }
    timer.here_then_reset("Finish word count.");
    for(auto elm : word_counts0) assert(word_counts[elm.first]==elm.second);
    for(auto elm : word_counts0) assert(word_counts2[elm.first]==elm.second);
}

void reverse_iterator(){
    std::vector<char> buffer(100);
    std::string str = "12 34 56 78";
    for(size_t i=0; i<str.size(); ++i) buffer[i]=str[i];
    auto end=std::find_if(buffer.crbegin(), buffer.crend(), [](auto x){return x!='\0';});
    assert(end!=buffer.crbegin());
    assert(end-buffer.crend() == std::distance(str.cend(),str.cbegin()));
    auto it=std::find_if(buffer.crbegin(), buffer.crend(), [](auto x){return std::isspace(x);});
    assert(it-end==2);
}

void hash(){
    auto seed = 1;
    char cs[10] = "Hello";

    uint64_t hash = xxh64::hash (reinterpret_cast<const char*> (cs), 10, seed);
    assert(sizeof(cs)==10);
    fmt::print("{}\n", hash);
}

}//namespace test

int main(int argc, char** argv){
//    test::reverse_iterator();
//    test::string_iterator();
//    test::benchmark();
//    test::hash();
//    return 0;
    assert(argc>1);
    auto config = util::load_json(argv[1]);
    WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};

    util::Timer timer{};
    auto word_counts = word_count(std::cin);
    timer.here_then_reset("Finish word count.");
    for(auto elm : word_counts)
        fmt::print("{} {}\n", elm.first, elm.second);
    return 0;
}
