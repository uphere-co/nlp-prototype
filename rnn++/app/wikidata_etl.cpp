#include <sstream>
#include <iostream>

#include <fmt/printf.h>

#include "wordrep/word_uid.h"
#include "similarity/config.h"


#include "utils/parallel.h"
#include "utils/profiling.h"
#include "utils/algorithm.h"
#include "utils/string.h"
#include "utils/json.h"

using util::get_str;
using util::find;
using util::has_key;

void extract_items(std::istream&& is){
    tbb::task_group g;
    while (auto buffer=util::string::read_chunk(is, 2000000)) {
        auto& chars =  buffer.value();
        g.run([chars{std::move(chars)}](){
            std::stringstream ss;
            ss.str(chars.data());
            auto lines = util::string::readlines(std::move(ss));

            std::stringstream items;
            for(auto& line : lines){
                if(line.back()!=',') continue;
                line.back() = ' ';
                auto elm = util::json_t::parse(line);
                auto id = get_str(elm,"id");
                auto type = get_str(elm,"type");
                auto maybe_label = util::find<std::string>(elm["labels"]["en"], "value");

                std::stringstream head;
                fmt::print(head, "{}\t{}", id, type);
                fmt::print(head, "\tP31");
                if(has_key(elm["claims"], "P31")){
                    for(auto& x : elm["claims"]["P31"]){
                        auto maybe_id = util::find<std::string>(x["mainsnak"]["datavalue"]["value"],"id");
                        if(!maybe_id) {
                            fmt::print(std::cerr, "{}\n", line);
                            continue;
                        }
                        auto id = maybe_id.value();
                        fmt::print(head, " {}", id);
                    }
                }
                fmt::print(head, "\tP279");
                if (has_key(elm["claims"], "P279")){
                    for(auto& x : elm["claims"]["P279"]){
                        auto maybe_id = util::find<std::string>(x["mainsnak"]["datavalue"]["value"],"id");
                        if(!maybe_id) {
                            fmt::print(std::cerr, "{}\n", line);
                            continue;
                        }
                        auto id = maybe_id.value();
                        fmt::print(head, " {}", id);
                    }
                }

                if(maybe_label) {
                    fmt::print(items, "{}\t{}\n", head.str(), maybe_label.value());
                    for(auto& x : elm["aliases"]["en"])
                        fmt::print(items, "{}\t{}\n", id, get_str(x,"value"));
                }
            }
            fmt::print("{}", items.str());
        });
    }
    g.wait();
}



struct NamedEntity{
    NamedEntity(wordrep::WordUIDindex const& wordUIDs, std::string line)
    : orig{line}{
        auto tokens = util::string::split(line);
        tag = tokens[0];
        auto n = tokens.size();
        for(decltype(n)i=1; i!=n; ++i){
            words.push_back(wordUIDs[tokens[i]]);
        }
    }

    friend bool operator< (NamedEntity const& a, NamedEntity const& b){
        return a.words > b.words;
    }
    friend std::ostream& operator<< (std::ostream& os, NamedEntity const& a){
        fmt::print(os, "{}", a.tag);
        for(auto word: a.words) fmt::print(os, " {}", word);
        fmt::print(os, " {}", a.orig);
        return os;
    }
    std::string orig;
    std::string tag;
    std::vector<wordrep::WordUID> words;
};

void index_items(wordrep::WordUIDindex const& wordUIDs, std::istream&& is){
    tbb::task_group g;
    tbb::concurrent_vector<NamedEntity> items;

    util::Timer timer;
    while (auto buffer=util::string::read_chunk(is, 2000000)) {
        auto& chars =  buffer.value();
        g.run([&wordUIDs, &items, chars{std::move(chars)}](){
            std::stringstream ss;
            ss.str(chars.data());
            auto lines = util::string::readlines(std::move(ss));

            for(auto& line : lines) {
                NamedEntity item{wordUIDs, line};
                items.push_back(item);
            }
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
    index_items(wordUIDs, std::move(std::cin));
    //extract_items(std::move(std::cin));
    return 0;
}
