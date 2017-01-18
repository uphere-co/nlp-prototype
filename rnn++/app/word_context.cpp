#include <iostream>

#include <fmt/printf.h>

#include "wordrep/indexed_text.h"
#include "wordrep/word_uid.h"

#include "utils/string.h"
#include "utils/json.h"
#include "utils/versioned_name.h"
#include "utils/parallel.h"
#include "utils/profiling.h"

using wordrep::WordUID;
using wordrep::IndexedTexts;

void get_contexts(IndexedTexts const& texts,
                  wordrep::WordUIDindex const& wordUIDs,
                  std::vector<WordUID> const& words){
    util::Timer timer;
//    auto iter = util::IterChunkIndex_factory(texts.sents_uid.get());
    auto n = IndexedTexts::Index::from_unsigned(texts.sents_uid.size());
    timer.here_then_reset("Data loaded.");
    auto indexed_words = util::zip(texts.words_uid.get(),
                                   util::sequence(IndexedTexts::Index{0}, n));
    std::sort(indexed_words .begin(), indexed_words .end(), [](auto x, auto y){return x.first<y.first;});
    timer.here_then_reset("Sorting words by their UIDs.");

    tbb::concurrent_vector<std::pair<WordUID,std::map<WordUID, size_t>>> contexts_count;
    auto n_word = words.size();
    tbb::parallel_for(decltype(n_word){0}, n_word, [&](auto i){
        auto word = words[i];
        //auto beg=std::find_if(indexed_words.cbegin(),indexed_words.cend(),[word](auto x){return x.first==word;});
        auto it=util::binary_find(indexed_words, [word](auto x){return word==x.first;}, [word](auto x){return word<x.first;});
        if(!it) return;
        auto beg = it.value();
        std::reverse_iterator<decltype(beg)> rbeg{beg};
        rbeg=std::find_if_not(rbeg,indexed_words.crend(),[word](auto x){return x.first==word;});
        beg = rbeg.base();
        auto end=std::find_if_not(beg,indexed_words.cend(),[word](auto x){return x.first==word;});
        std::map<WordUID, size_t> ccount;
        for(auto it=beg; it!=end; ++it){
//            for(auto idx=it->second-5; idx!=it->second+5+1; ++idx)
//                fmt::print("{} ", wordUIDs[texts.word_uid(idx)]);
//            fmt::print("\n");
            for(auto idx=it->second-5; idx!=it->second; ++idx){
                auto cword = texts.word_uid(idx);
                ++ccount[cword];
            }
            for(auto idx=it->second+1; idx!=it->second+1+5; ++idx){
                auto cword = texts.word_uid(idx);
                ++ccount[cword];
            }
        }
        contexts_count.push_back({word, ccount});
        //timer.here_then_reset(fmt::format("Get context words for : {}", wordUIDs[word]));
    });
    timer.here_then_reset(fmt::format("Get context words"));
    for(auto elm : contexts_count){
        auto word = elm.first;
        auto cs = util::to_pairs(elm.second);
        std::sort(cs.begin(), cs.end(), [](auto x, auto y){return x.second>y.second;});
        for(auto c : cs) {
            auto cword = c.first;
            auto count = c.second;
            fmt::print("{:<15} {:<15} {}\n", wordUIDs[word], wordUIDs[cword], count);
        }
    }
}

auto getlines(std::istream&& is){
    std::vector<std::string> lines;
    std::string line;
    while(std::getline(is, line)){
        lines.push_back(line);
    }
    return lines;
}

int main(int argc, char** argv){
    assert(argc>1);
    auto config = util::load_json(argv[1]);

    IndexedTexts texts{util::io::h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname),
                       config["dep_parsed_prefix"]};
    wordrep::WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};

    std::vector<std::string> words=getlines(std::move(std::cin));
    auto wuids = util::map(words, [&wordUIDs](auto word){return wordUIDs[word];});
    get_contexts(texts, wordUIDs, wuids);
    return 0;
}