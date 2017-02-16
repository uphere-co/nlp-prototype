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
    WikidataEntity(wordrep::WordUIDindex const& wordUIDs, std::string line) {
        auto tokens = util::string::split(line, "\t");
        if(tokens.size()!=2){
            fmt::print("{}\n", line);
            assert(0);
        }
        uid = tokens[0];
        for(auto w : util::string::split(tokens[1], " "))
            words.push_back(wordUIDs[w]);
    }

    std::string repr(wordrep::WordUIDindex const& wordUIDs) const{
        std::stringstream ss;
        for(auto word : words) ss << wordUIDs[word] << " ";
        return ss.str();
    }

    friend bool operator< (WikidataEntity const& a, WikidataEntity const& b){
        return a.words > b.words;
    }
    friend std::ostream& operator<< (std::ostream& os, WikidataEntity const& a){
        fmt::print(os, "{}\t", a.uid);
        for(auto word: a.words) fmt::print(os, " {}", word);
        return os;
    }
    std::string uid;
    std::vector<wordrep::WordUID> words;
};

struct SortedWikidataEntities{
    std::vector<WikidataEntity> entities;
};

SortedWikidataEntities read_wikidata_entities(wordrep::WordUIDindex const& wordUIDs, std::istream&& is){
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
    std::vector<WikidataEntity> entities;
    for(auto&& item : items) entities.push_back(std::move(item));
    return SortedWikidataEntities{std::move(entities)};
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

struct TaggedEntity{
    size_t offset;
    WikidataEntity const& entity;
};
struct GreedyAnnotator{
    GreedyAnnotator(SortedWikidataEntities&& entities)
            : entities{std::move(entities.entities)} {
    }
    GreedyAnnotator(SortedWikidataEntities entities)
            : entities{std::move(entities.entities)} {
    }

    std::vector<TaggedEntity> annotate(std::vector<wordrep::WordUID> const& text) const{
        std::vector<TaggedEntity> tagged;
        auto to_reverse = [](auto it){return std::reverse_iterator<decltype(it)>{it};};
        size_t offset=0;
        size_t i = 0;
        auto beg = entities.cbegin();
        auto end = entities.cend();
        auto pbeg = beg;
        auto pend = end;
        while(true){
            auto t = text[offset+i];
            auto eq   = [t,i,this](WikidataEntity const& x){
                if(x.words.size()<=i) return false;
                return t==x.words[i];
            };
            auto less = [t,i](WikidataEntity const& x){
                if(x.words.size()<=i) return true;
                return t>x.words[i];
            };
            auto mit = util::binary_find(pbeg, pend, eq, less);
            if(!mit) {
                if(i == 0) {
                    ++offset;
                } else {
                    for(auto it=pbeg; it!=pend; ++it)
                        if(it->words.size()==i) tagged.push_back({offset, *it});
                    offset += i;
                    i = 0;
                }
                if(offset>= text.size()) break;
                pbeg = beg;
                pend = end;
                continue;
            }
            auto it = mit.value();
            pbeg = std::find_if_not(to_reverse(it), to_reverse(pbeg), eq).base();
            pend = std::find_if_not(it, pend, eq);
            ++i;
        }
        return tagged;
    }

    std::vector<WikidataEntity> entities;
};
void test_greedy_matching(){
    std::vector<WikidataEntity> items =
            {{"A", {1,2}},{"AA", {1,2}},{"B",{1,3}}, {"C",{1,2,3}},{"CC",{1,2,3}},{"CCC",{1,2,3}},
             {"D", {2,3,4}}, {"E",{5}}, {"EE",{5}}, {"EEE",{5}}, {"F",{6,7}},
             {"G", {2,3}}};
    std::sort(items.begin(), items.end());
    SortedWikidataEntities entities{items};
    std::vector<wordrep::WordUID> text = {1,2,3,4,8,9,5,2,3,4,2,3,8,9,3,4,5,6,7};

    fmt::print("Entities :\n");
    for(auto& item : entities.entities)
        fmt::print("{}\n", item);
    fmt::print("Text :");
    for(auto t : text)
        fmt::print(" {}", t);
    fmt::print("\n");

    GreedyAnnotator annotator{entities};
    auto tags = annotator.annotate(text);
    for(auto tag : tags)
        fmt::print("{} : {}\n", tag.offset, tag.entity);
}

int main(int argc, char** argv){
//    test_ordering();
//    test_greedy_matching();
//    return 0;

    assert(argc>1);
    auto config_json = util::load_json(argv[1]);
    engine::Config config{config_json};
    engine::SubmoduleFactory factory{config};
    auto wordUIDs = factory.word_uid_index();
    util::Timer timer;
    auto entities = read_wikidata_entities(wordUIDs, std::move(std::cin));
    timer.here_then_reset("Read items.");

    std::string query = "Google Voice is a product of Google . BASF vs BASF SE . DeepMind , deepmind , European Union . EU . eu";
    auto words = util::string::split(query, " ");
    std::vector<wordrep::WordUID> text = util::map(words, [&wordUIDs](auto x){return wordUIDs[x];});
    GreedyAnnotator annotator{entities};
    timer.here_then_reset("Build data structures.");
    auto tags = annotator.annotate(text);
    timer.here_then_reset(fmt::format("Annotate a query of {} words.", words.size()));
    for(auto tag : tags)
        fmt::print("{} : {}\n", tag.offset, tag.entity.repr(wordUIDs));

    return 0;
}
