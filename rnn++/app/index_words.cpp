#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <cassert>
#include <cctype>
#include <iterator>

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


std::string strip(std::string str){
    return str.substr(str.find_first_not_of(" \n\t"));
}

struct WordIter{
    WordIter(std::string text)
            : text_strs{std::move(text)}, text{gsl::ensure_z(text_strs.data())}
    {}
    template<typename OP>
    void iter(OP const &op) const {
        auto text_beg = std::cbegin(text);
        auto text_end = std::cend(text);
        auto beg = std::find_if_not(text_beg, text_end, [](auto x){return std::isspace(x);});
        while(beg!=text_end){
            auto end=std::find_if(beg, text_end, [](auto x){return std::isspace(x);});
            auto word = text.subspan(beg-text_beg, end-beg);
            op(word);
            if(end==text_end) break;
            beg = std::find_if_not(end, text_end, [](auto x){return std::isspace(x);});
        }
    }

    std::string text_strs;
    gsl::cstring_span<> text;
};

struct WordIter2{
    WordIter2(std::string text)
            : text_strs{std::move(text)}
    {}
    template<typename OP>
    void iter(OP const &op){
        for(auto&& word : util::string::split(strip(text_strs), " ")){
            op(word);
        }
    }

    std::string text_strs;
};

namespace test{

void string_iterator(){
    WordIter text{"11 22 33\t\t14 15\n16 17 18   119\t \n 11110\n"};
    WordIter2 text2{"11 22 33\t\t14 15\n16 17 18   119\t \n 11110\n"};

    Timer timer{};
    text.iter([](auto& word){fmt::print("{}\n", gsl::to_string(word));});
    timer.here_then_reset("span.");
    text2.iter([](auto& word){fmt::print("{}\n", word);});
    timer.here_then_reset("Plain string.");

}

void benchmark(){
    auto lines = util::string::readlines("../rnn++/tests/data/sentence.2.corenlp");

    util::Timer timer{};
    std::map<std::string, size_t> word_counts;
    {
        for(auto &line : lines){
            WordIter text{line};
            text.iter([&word_counts](auto& word){++word_counts[gsl::to_string(word)];});
        }
        timer.here_then_reset("Finish word count.");
    }
    {
        std::map<std::string, size_t> word_counts0;
        for(auto &line : lines){
            WordIter2 text{line};
            text.iter([&word_counts](auto& word){++word_counts[word];});
        }
        for(auto elm : word_counts0) assert(word_counts[elm.first]==elm.second);
        timer.here_then_reset("Finish word count.");
    }
    for(auto elm : word_counts)
        fmt::print("{} {}\n", elm.first, elm.second);
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

}

template<typename T>
std::optional<std::string> getlines(T& is, int n){
    std::string str{};
    std::string line;
    for(int i=0; i!=n; ++i){
        if(!std::getline(is, line)) break;
        str.append(line+"\n");
    }
    if(str.empty()) return {};
    return str;
}

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

void word_count(){
    std::map<std::string, size_t> word_counts;
    for (std::string str; std::getline(std::cin, str);) {
        WordIter text{str};
        text.iter([&word_counts](auto& word){++word_counts[gsl::to_string(word)];});
    }
    for(auto elm : word_counts)
        fmt::print("{} {}\n", elm.first, elm.second);
}

int main(int argc, char** argv){
//    test::reverse_iterator();
//    test::string_iterator();
//    test::benchmark();
//    return 0;
    assert(argc>1);
    auto config = util::load_json(argv[1]);
    WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};

    util::Timer timer{};
    tbb::task_group g;
    using map_t = tbb::concurrent_hash_map<std::string, size_t>;
    map_t wcs;
    while (auto buffer=read_chunk(std::cin, 20000)) {
        std::string str{buffer.value().data()};
        g.run([&wcs,str](){
            WordIter text{str};
            std::map<std::string, size_t> word_counts;
            text.iter([&word_counts](auto& word) {++word_counts[gsl::to_string(word)];});
            for(auto const& elm : word_counts){
                map_t::accessor a;
                wcs.insert(a, elm.first);
                a->second += elm.second;
            }
        });
    }
    g.wait();
    std::map<std::string, size_t> word_counts;
    for(auto const& elm : wcs){
        word_counts[elm.first] = elm.second;
    }
    timer.here_then_reset("Finish word count.");
    for(auto elm : word_counts)
        fmt::print("{} {}\n", elm.first, elm.second);
    return 0;
}
