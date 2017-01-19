#include <iostream>

#include <fmt/printf.h>

#include "wordrep/word_count.h"

#include "utils/profiling.h"
#include "utils/algorithm.h"

int main(){
    util::Timer timer{};
    wordrep::WordCounter<std::string> word_count;
//    wordrep::WordCounter<wordrep::WordUID> word_count;
    auto word_counts = word_count.count(std::move(std::cin));
    util::sort(word_counts, [](auto& x, auto& y){return x.second>y.second;});
    fmt::print(std::cerr, "{} words.\n", word_counts.size());
    timer.here_then_reset("Finish word count.");
    size_t sum{0};
    for(auto elm : word_counts){
        fmt::print(std::cout, "{:<15}\t{:<15}\n", elm.first, elm.second);
        sum+= elm.second;
    }
    fmt::print(std::cerr, "Total count : {}\n", sum);
    return 0;
}
