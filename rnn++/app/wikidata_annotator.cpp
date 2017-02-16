#include <sstream>
#include <iostream>
#include <vector>

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
    WikidataEntity(std::string uid, std::vector<wordrep::WordUID> words)
    : uid{uid}, words{words}
    {}
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

void test_greedy_matching(){
    std::vector<WikidataEntity> items =
            {{"A", {1,2}},{"AA", {1,2}},{"B",{1,3}}, {"C",{1,2,3}},{"CC",{1,2,3}},{"CCC",{1,2,3}},
             {"D", {2,3,4}}, {"E",{5}}, {"EE",{5}}, {"EEE",{5}}, {"F",{6,7}},
             {"G", {2,3}}};
    tbb::parallel_sort(items.begin(), items.end());
    for(auto& item : items)
        fmt::print("{}\n", item);
    std::vector<wordrep::WordUID> text = {1,2,3,4,8,9,5,2,3,4,2,3,8,9,3,4,5,6,7};

    fmt::print("Text :");
    for(auto t : text)
        fmt::print(" {}", t);
    fmt::print("\n");
    auto to_reverse = [](auto it){return std::reverse_iterator<decltype(it)>{it};};
    auto offset=0;
    auto i = 0;
    auto beg = items.cbegin();
    auto end = items.cend();
    auto pbeg = beg;
    auto pend = end;
    //fmt::print("Partition : {} {}\n", *beg, *end);
    while(true){

        auto t = text[offset+i];
        auto eq   = [t,i,&items](WikidataEntity const& x){
//            fmt::print("Equality compare : {} and {}\n", t, x);
            if(x.words.size()<=i) return false;
            return t==x.words[i];
        };
        auto less = [t,i](WikidataEntity const& x){
//            fmt::print("Less compare : {} and {}\n", t, x);
            if(x.words.size()<=i) return true;
            return t>x.words[i];
        };
        auto mit = util::binary_find(pbeg, pend, eq, less);
        if(!mit) {
            if(i == 0) {
                ++offset;
            } else {
                for(auto it=pbeg; it!=pend; ++it)
                    if(it->words.size()==i) fmt::print("{} {} : {}\n", offset, i, *it);
                offset += i;
                i = 0;
            }
            if(offset>= text.size()) break;
//            fmt::print("Next state to try : {} {}\n", offset, i);
            pbeg = beg;
            pend = end;
            continue;
        }
        auto it = mit.value();
        pbeg = std::find_if_not(to_reverse(it), to_reverse(pbeg), eq).base();
        pend = std::find_if_not(it, pend, eq);
//        if(offset==0 && i==0){
//            assert(pbeg->uid == "B");
//            assert(pend == end);
//        } else if(offset==0 && i==1){
//            assert(pbeg->uid == "C");
//            assert(pend == end);
//        } else if(offset==0 && i==2){
//            assert(pbeg->uid == "C");
//            assert(pend->uid == "A");
//        } else if(offset==7 && i==0){
//            assert(pbeg->uid == "D");
//            assert(pend->uid == "B");
//        }
        ++i;
    }
}

int main(int argc, char** argv){
//    test_ordering();
    test_greedy_matching();
    return 0;

    assert(argc>1);
    auto config_json = util::load_json(argv[1]);
    engine::Config config{config_json};
    engine::SubmoduleFactory factory{config};
    auto wordUIDs = factory.word_uid_index();
    sort_wikidata_entities(wordUIDs, std::move(std::cin));
    return 0;
}
