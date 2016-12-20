#include <iostream>
#include <string>
#include <map>
#include <cassert>
#include <cctype>

#include <fmt/printf.h>

#include "wordrep/word_uid.h"

#include "utils/string.h"
#include "utils/json.h"
#include "utils/algorithm.h"
#include "utils/profiling.h"
#include "utils/span.h"

using wordrep::WordUIDindex;
using util::Timer;


struct WordIter{
    WordIter(std::string text)
            : text_strs{std::move(text)}, text{gsl::ensure_z(text_strs.data())}
    {}
    template<typename OP>
    void iter(OP const &op){
        auto text_beg = std::begin(text);
        auto text_end = std::end(text);
        //for(auto it=beg; it!=end; it = test.find_first_not_of(" \n\t"))
        auto it=text_beg;
        //for(auto end=beg; end!=text_end; ){
        while(it!=text_end){
            auto beg = it;
            it=std::find_if(it, text_end, [](auto x){return std::isspace(x);});
            if(it==text_end) break;
            auto end = it;
            auto word = text.subspan(beg-text_beg, end-beg);
            op(word);
            it=std::find_if_not(it, text_end, [](auto x){return std::isspace(x);});
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
        for(auto&& word : util::string::split(text_strs, " ")){
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
    auto lines = util::string::readlines("news.2014.test");

    util::Timer timer{};

    {
        std::map<std::string, size_t> word_counts;
        for(auto &line : lines){
            for(auto&& word : util::string::split(line, " ")){
                ++word_counts[word];
            }
        }
        timer.here_then_reset("Finish word count.");
    }
    {
        std::map<std::string, size_t> word_counts;
        for(auto &line : lines){
            WordIter text{line};
            text.iter([&word_counts](auto& word){++word_counts[gsl::to_string(word)];});
        }
        timer.here_then_reset("Finish word count.");
    }
    {
        std::map<std::string, size_t> word_counts;
        for(auto &line : lines){
            WordIter2 text{line};
            text.iter([&word_counts](auto& word){++word_counts[word];});
        }
        timer.here_then_reset("Finish word count.");
    }
}

}

int main(int argc, char** argv){
//    test::string_iterator();
    test::benchmark();
    return 0;
    assert(argc>1);
    auto config = util::load_json(argv[1]);
    WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};

    std::map<std::string, size_t> word_counts;
    util::Timer timer{};
    for (std::string line; std::getline(std::cin, line);) {
        for(auto&& word : util::string::split(line, " ")){
            ++word_counts[word];
        }
    }
    timer.here_then_reset("Finish word count.");
    for(auto elm : word_counts)
        fmt::print("{} {}\n", elm.first, elm.second);

    return 0;
}
