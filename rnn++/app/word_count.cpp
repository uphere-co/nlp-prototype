#include <iostream>

#include <fmt/printf.h>

#include "wordrep/word_count.h"

#include "utils/profiling.h"
#include "utils/algorithm.h"

int main(){
    util::Timer timer{};
    wordrep::WordCounter word_count;
    auto word_counts = word_count.count(std::move(std::cin));
    fmt::print(std::cerr, "{} words.\n", word_counts.size());
    util::filter_inplace(word_counts, [](auto v){return v.second>9;});
    fmt::print(std::cerr, "{} words are left after filtering.\n", word_counts.size());
    timer.here_then_reset("Finish word count.");
    for(auto elm : word_counts)
        fmt::print(std::cout, "{} {}\n", elm.first, elm.second);
    return 0;
}
