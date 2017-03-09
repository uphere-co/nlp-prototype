#include <iostream>

#include <fmt/printf.h>

#include "word2vec/word2vec.h"

#include "wordrep/indexed_text.h"
#include "wordrep/word_uid.h"

#include "utils/string.h"
#include "utils/json.h"
#include "utils/versioned_name.h"
#include "utils/parallel.h"
#include "utils/random.h"
#include "utils/profiling.h"

using wordrep::WordUID;
using wordrep::VocaIndex;
using wordrep::IndexedTexts;
using word2vec::UnigramDist;
using word2vec::SubSampler;

struct ContextCount{
    WordUID word;
    std::map<WordUID,size_t> count;
};
void get_contexts(IndexedTexts const& texts,
                  wordrep::WordUIDindex const& wordUIDs,
                  std::vector<WordUID> const& words){
    util::Timer timer;
//    auto iter = util::IterChunkIndex_factory(texts.sents_uid.get());
    auto n = IndexedTexts::Index::from_unsigned(texts.sents_uid.size());
    std::vector<std::pair<WordUID, IndexedTexts::Index>> indexed_words;
    indexed_words.reserve(texts.size());
    IndexedTexts::Index idx{0};
    for(auto word : texts.words_uid) indexed_words.push_back({word,idx++});
    timer.here_then_reset("Prepare data.");
    tbb::parallel_sort(indexed_words.begin(), indexed_words.end(), [](auto x, auto y){return x.first<y.first;});
    timer.here_then_reset("Sort words by their UIDs.");

    util::ConcurrentVector<ContextCount> contexts_count;
    auto n_word = words.size();
    tbb::parallel_for(decltype(n_word){0}, n_word, [&](auto i){
        auto n_gram = 10;
        auto word = words[i];
        //auto beg=std::find_if(indexed_words.cbegin(),indexed_words.cend(),[word](auto x){return x.first==word;});
        auto it=util::binary_find(indexed_words, [word](auto x){return word==x.first;}, [word](auto x){return word<x.first;});
        if(!it) return;
        auto beg = it.value();
        std::reverse_iterator<decltype(beg)> rbeg{beg};
        rbeg=std::find_if_not(rbeg,indexed_words.crend(),[word](auto x){return x.first==word;});
        beg = rbeg.base();
        auto end=std::find_if_not(beg,indexed_words.cend(),[word](auto x){return x.first==word;});
        ContextCount ccount{word, {}};
        for(auto it=beg; it!=end; ++it){
            IndexedTexts::Index cbeg = it->second>IndexedTexts::Index{n_gram} ? it->second-n_gram:0;
            IndexedTexts::Index cend = it->second+1+5<n ? it->second-n_gram:n;
            for(auto idx=cbeg; idx<it->second; ++idx){
                auto cword = texts.word_uid(idx);
                ++ccount.count[cword];
            }
            assert(texts.word_uid(it->second)==word);
            for(auto idx=it->second+1; idx<cend; ++idx){
                auto cword = texts.word_uid(idx);
                ++ccount.count[cword];
            }
        }
        contexts_count.push_back(ccount);
    });
    timer.here_then_reset(fmt::format("Get context words"));
    auto counts = contexts_count.to_vector();
    timer.here_then_reset(fmt::format("Build results"));
    for(auto elm : counts){
        auto word = elm.word;
        for(auto ccount : elm.count){
            auto cword = ccount.first;
            auto count = ccount.second;
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
    util::Timer timer;

    wordrep::WordUIDindex wordUIDs{util::get_str(config,"word_uids_dump")};
    tbb::task_group g;
    std::optional<IndexedTexts> m_texts = {};
    std::optional<std::vector<std::string>> m_words = {};
    std::optional<std::vector<WordUID>> m_wuids = {};
    g.run([&m_texts, &config](){
        m_texts = IndexedTexts{util::io::h5read(util::get_latest_version(util::get_str(config, "dep_parsed_store")).fullname),
                               config["dep_parsed_prefix"]};
    });
    g.run([&m_wuids,&wordUIDs](){
        auto words = getlines(std::move(std::cin));
        m_wuids = util::map(words, [&wordUIDs](auto word){return wordUIDs[word];});
    });
    g.wait();
    timer.here_then_reset("Load data.");
    auto texts = m_texts.value();
    auto wuids = m_wuids.value();
    timer.here_then_reset("Construct data objects.");
    get_contexts(texts, wordUIDs, wuids);
    timer.here_then_reset("Finish.");

    return 0;
}
