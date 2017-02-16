#include <sstream>
#include <iostream>

#include <fmt/printf.h>

#include "wordrep/word_uid.h"
#include "similarity/config.h"

#include "utils/parallel.h"
#include "utils/profiling.h"
#include "utils/algorithm.h"
#include "utils/string.h"

using util::get_str;
using util::find;
using util::has_key;

struct WikidataEntity{
    WikidataEntity(wordrep::WordUIDindex const& wordUIDs, std::string line)
            : orig{line}{
        auto tokens = util::string::split(line, "\t");
        if(tokens.size()!=2){
            fmt::print("{}\n", line);
            assert(0);
        }
        uid = tokens[0];
        for(auto w : util::string::split(tokens[1], " "))
            words.push_back(wordUIDs[w]);
    }

    friend bool operator< (WikidataEntity const& a, WikidataEntity const& b){
        return a.words > b.words;
    }
    friend std::ostream& operator<< (std::ostream& os, WikidataEntity const& a){
        fmt::print(os, "{}\t", a.uid);
        for(auto word: a.words) fmt::print(os, " {}", word);
        fmt::print(os, "\t{}", a.orig);
        return os;
    }
    std::string orig;
    std::string uid;
    std::vector<wordrep::WordUID> words;
};

void sort_wikidata_entities(wordrep::WordUIDindex const& wordUIDs, std::istream&& is){
    tbb::task_group g;
    tbb::concurrent_vector<WikidataEntity> items;

    util::Timer timer;
    while (auto buffer=util::string::read_chunk(is, 2000000)) {
        auto& chars =  buffer.value();
        g.run([&wordUIDs, &items, chars{std::move(chars)}](){
            std::stringstream ss;
            ss.str(chars.data());
            auto lines = util::string::readlines(std::move(ss));
            for(auto& line : lines)
                items.push_back({wordUIDs, line});
        });
    }
    g.wait();
    timer.here_then_reset("Read all items.");
    tbb::parallel_sort(items.begin(), items.end());
    timer.here_then_reset("Sorted items.");
    for(auto& item : items)
        fmt::print("{}\n", item);
    timer.here_then_reset("Print items.");
    fmt::print(std::cerr, "{} items.\n", items.size());
}

void test_ordering(){
    std::vector<int> a1{1,1};
    std::vector<int> a2{1,2};
    std::vector<int> b{1,2,3};
    std::vector<int> c{2,2,3};
    std::vector<int> d{2,3};
    std::vector<std::vector<int>> vs{d,b,c,a2,a1};
    util::sort(vs);
    assert(vs[0]==a1);
    assert(vs[1]==a2);
    assert(vs[2]==b);
    assert(vs[3]==c);
    assert(vs[4]==d);

}
int main(int argc, char** argv){
//    test_ordering();
//    return 0;

    assert(argc>1);
    auto config_json = util::load_json(argv[1]);
    engine::Config config{config_json};
    engine::SubmoduleFactory factory{config};
    auto wordUIDs = factory.word_uid_index();
    sort_wikidata_entities(wordUIDs, std::move(std::cin));
    return 0;
}
