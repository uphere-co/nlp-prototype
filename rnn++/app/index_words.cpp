#include <iostream>
#include <string>
#include <map>
#include <cassert>

#include <fmt/printf.h>

#include "wordrep/word_uid.h"

#include "utils/string.h"
#include "utils/json.h"

using wordrep::WordUIDindex;

int main(int argc, char** argv){
    assert(argc>1);
    auto config = util::load_json(argv[1]);
    WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};
    std::map<std::string, size_t> word_counts;
    for (std::string line; std::getline(std::cin, line);) {
        for(auto&& word : util::string::split(line, " ")){
            ++word_counts[word];
        }
    }
    for(auto elm : word_counts)
        fmt::print("{} {}\n", elm.first, elm.second);
    return 0;
}
