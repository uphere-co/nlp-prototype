#include <iostream>

#include <fmt/printf.h>

#include "word2vec/word2vec.h"

#include "wordrep/indexed_text.h"
#include "wordrep/word_uid.h"
#include "wordrep/voca_info.h"

#include "similarity/config.h"

#include "utils/string.h"
#include "utils/json.h"
#include "utils/versioned_name.h"
#include "utils/parallel.h"
#include "utils/random.h"
#include "utils/profiling.h"
#include "utils/linear_algebra.h"

using wordrep::WordUID;
using wordrep::VocaIndex;
using wordrep::IndexedTexts;
using word2vec::UnigramDist;
using word2vec::SubSampler;

struct ContextCount{
    WordUID word;
    std::map<WordUID,size_t> count;
};
std::vector<ContextCount> get_contexts(IndexedTexts const& texts,
                  wordrep::WordUIDindex const& wordUIDs,
                  std::vector<WordUID> const& words,
                  int64_t n_gram = 10){
    util::Timer timer;
//    auto iter = util::IterChunkIndex_factory(texts.sents_uid.get());
    auto n = IndexedTexts::Index::from_unsigned(texts.sents_uid.size());
    std::vector<std::pair<WordUID, IndexedTexts::Index>> indexed_words;
    indexed_words.reserve(texts.size());
    IndexedTexts::Index idx{0};
    for(auto word : texts.words_uid) indexed_words.push_back({word,idx++});
    timer.here_then_reset("get_contexts : Prepare data.");
    tbb::parallel_sort(indexed_words.begin(), indexed_words.end(), [](auto x, auto y){return x.first<y.first;});
    timer.here_then_reset("get_contexts : Sort words by their UIDs.");

    util::ConcurrentVector<ContextCount> contexts_count;
    auto n_word = words.size();
    tbb::parallel_for(decltype(n_word){0}, n_word, [&](auto i){
        auto word = words[i];
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
    timer.here_then_reset("get_contexts : Get context words");
    auto counts = contexts_count.to_vector();
    timer.here_then_reset("get_contexts : Build results");
    return counts;
}


void pruning_voca(){
    auto h5store = "/home/jihuni/word2vec/rss/news.h5";
    auto voca_words_name = "news.en.uids";
    auto voca_vecs_name = "news.en.vecs";
    auto voca_float_typename = "float32";
    auto file_words = "test.voca";
    wordrep::WordUIDindex all_wordUIDs("/home/jihuni/word2vec/rss/all_words");

    auto all_word_uids = wordrep::load_voca(h5store, voca_words_name);
    wordrep::VocaIndexMap all_voca{all_word_uids};

    std::set<WordUID> all_words{all_word_uids.cbegin(),all_word_uids.cend()};

    wordrep::WordUIDindex wordUIDs(file_words);
    auto uids = wordUIDs.get_uids();
    uids.push_back(wordrep::the_unknown_word_uid());

    auto known_uids = util::filter(uids, [&all_words](auto uid){return all_words.find(uid)!=all_words.end();});
    wordrep::VocaIndexMap voca{known_uids};

    auto n_words = known_uids.size();

    using WordBlock = wordrep::WordBlock_base<float,100>;
    WordBlock all_wvecs{wordrep::load_raw_wvec(h5store, voca_vecs_name, voca_float_typename)};

    //WordBlock wvecs{std::vector<WordBlock::val_t>(WordBlock::dim * n_words)};
    std::vector<WordBlock::val_t> raw_vecs;
    raw_vecs.reserve(WordBlock::dim * n_words);
    for(VocaIndex i=0; i!=VocaIndex::from_unsigned(n_words); ++i){
        auto word_uid = voca[i];
        auto a=all_wvecs[all_voca[word_uid]];
        std::copy(a.cbegin(),a.cend(), std::back_inserter(raw_vecs));
        //auto b = wvecs[i];
        //b = a;
//        for(int j=0; j!=100; ++j) b[j]==a[j];
        //for(int j=0; j!=100; ++j) assert(a[j]==b[j]);
    }
    WordBlock wvecs{std::move(raw_vecs)};
    util::io::H5file outfile{{"test.h5"}, util::io::hdf5::FileMode::replace};
    util::TypedPersistentVector<WordUID> new_uids{voca_words_name, std::move(known_uids)};
    new_uids.write(outfile);
    outfile.writeRawData({voca_vecs_name},  util::serialize(wvecs.serialize()));
}

auto getlines(std::istream&& is){
    std::vector<std::string> lines;
    std::string line;
    while(std::getline(is, line)){
        lines.push_back(line);
    }
    return lines;
}

//operator +=
std::vector<wordrep::VocaInfo::val_t>
guess_word_embedding_from_context(wordrep::VocaInfo base, std::vector<ContextCount> words){
    using Vector = util::math::Vector<wordrep::VocaInfo::val_t,wordrep::VocaInfo::dim>;
    std::vector<wordrep::VocaInfo::val_t> wvecs;
    wvecs.reserve(words.size()*wordrep::VocaInfo::dim);
    for(auto context_count : words){
        auto word = context_count.word;
        Vector wvec;
        auto total_count = 0;
        for(auto c : context_count.count){
            auto cword = c.first;
            auto count = c.second;
            Vector cvec = base.wvecs[base.indexmap[cword]];
            cvec *= count;
            total_count += count;
            wvec += cvec;
        }
        wvec *= 1.0/total_count;
        fmt::print("{} \n", wvec.l1_norm());
        util::append(wvecs, wvec._val);
    }
    return wvecs;
}

namespace wordrep{

struct DerivedVoca{
    std::vector<WordUID> known_words;
    std::vector<WordUID> unseen_words;
};
DerivedVoca split_new_words(VocaInfo const& base_voca, std::vector<WordUID> new_words){
    auto base_words = base_voca.indexmap.all_words();
    tbb::parallel_sort(base_words.begin(), base_words.end());

    DerivedVoca new_voca;
    for(auto word : new_words) {
        if (!util::binary_find(base_words, word)) new_voca.unseen_words.push_back(word);
        else new_voca.known_words.push_back(word);
    }
    return new_voca;
//    timer.here_then_reset(fmt::format("Get {} unseen words.", new_words.size()));
}

auto derive_word_embedding(VocaInfo const& base_voca,
                           std::vector<WordUID> known_words, std::vector<ContextCount> unseen_words_context){
    std::vector<wordrep::VocaInfo::val_t> raw_vecs;
    raw_vecs.reserve((known_words.size()+unseen_words_context.size())*wordrep::VocaInfo::dim);
    for(auto word : known_words){
        auto wvec = base_voca.wvecs[base_voca.indexmap[word]];
        std::copy(wvec.cbegin(),wvec.cend(), std::back_inserter(raw_vecs));
    }
    auto wvecs_new_words = guess_word_embedding_from_context(base_voca, unseen_words_context);
    util::append(raw_vecs, wvecs_new_words);
    return raw_vecs;
}
}//namespace wordrep;


int main(int argc, char** argv){
    assert(argc>1);
    auto config = util::load_json(argv[1]);
    engine::SubmoduleFactory factory{{config}};
    util::Timer timer;

    wordrep::WordUIDindex wordUIDs = factory.word_uid_index();
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
    auto words_for_new_voca = m_wuids.value();
    timer.here_then_reset("Construct data objects.");
    auto base_voca = factory.voca_info();
    timer.here_then_reset(fmt::format("Load and sort base voca of {} words.", base_voca.indexmap.size()));

    auto new_voca_words = wordrep::split_new_words(base_voca, words_for_new_voca);
    auto unseen_words_with_context = get_contexts(texts, wordUIDs, new_voca_words.unseen_words);
//    assert(new_words_with_context.size()==new_words.size());
    timer.here_then_reset(fmt::format("Get contexts of {} unseen words.", unseen_words_with_context.size()));
    auto raw_vecs = wordrep::derive_word_embedding(base_voca, new_voca_words.known_words, unseen_words_with_context);
    std::vector<WordUID> words_new_voca =new_voca_words.known_words;
    util::append(words_new_voca, util::map(unseen_words_with_context, [](auto x){return x.word;}));
    util::TypedPersistentVector<WordUID> new_uids{factory.config.value("voca_name"), std::move(words_new_voca)};
    assert(raw_vecs.size()==new_uids.size()*wordrep::VocaInfo::dim);
    util::io::H5file outfile{{"test.h5"}, util::io::hdf5::FileMode::replace};
    new_uids.write(outfile);
    outfile.writeRawData({factory.config.value("w2vmodel_name")},  raw_vecs);

//    for(auto word : new_words){
//        if(util::isin(words_with_c, word)) continue;
//        fmt::print(std::cerr, "{}\n", wordUIDs[word]);
//    }
//    for(auto elm : new_words_with_context){
//        auto word = elm.word;
//        for(auto ccount : elm.count){
//            auto cword = ccount.first;
//            auto count = ccount.second;
//            fmt::print("{:<15} {:<15} {}\n", wordUIDs[word], wordUIDs[cword], count);
//        }
//    }
//    timer.here_then_reset("Print contexts.");
    return 0;
}
