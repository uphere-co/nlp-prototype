#include <string>
#include <iostream>
#include <map>
#include <cassert>

#include <fmt/printf.h>

#include "wordrep/word_uid.h"

#include "utils/string.h"
#include "utils/persistent_vector.h"

using util::PersistentVector;
using token_t = wordrep::WordUID;
using count_t = size_t;

std::map<token_t,count_t> collect_count(std::istream&& is){
    std::map<token_t,count_t> counts;
    std::string line;
    while(std::getline(is, line)){
        auto elms = util::string::split(line, " ");
        assert(elms.size()==2);
        token_t key{std::stoll(elms[0])};
        count_t count = std::stoll(elms[1]);
        counts[key] += count;
    }
    return counts;
}

int main(int /*argc*/, char** argv) {
    auto dumpfile = argv[1];
    //auto prefix = argv[2];
    auto prefix = "unigram.";
    PersistentVector<token_t,token_t::val_t>   uid{"uid"};
    PersistentVector<count_t,count_t> count{"count"};
    auto counts = collect_count(std::move(std::cin));
    for(auto x : counts) {
        uid.push_back(x.first);
        count.push_back(x.second);
    }
    util::io::H5file file{util::io::H5name{dumpfile}, util::io::hdf5::FileMode::replace};
    uid.write(file,   prefix);
    count.write(file, prefix);
    return 0;
}
