#include <fmt/printf.h>
#include <fmt/format.h>

#include "wordrep/wordvec_deriving.h"

#include "utils/string.h"

#include "utils/versioned_name.h"
#include "utils/parallel.h"
#include "utils/profiling.h"
#include "utils/linear_algebra.h"

namespace wordrep{

std::ostream& operator<<(std::ostream& os, ContextCountRepr const& src){
    for(auto elm : src.self.count){
        os << fmt::format("{} {} : {}\n", src.wordUIDs[src.self.word], src.wordUIDs[elm.first], elm.second);
    }
    return os;
}

std::vector<ContextCount> get_ngram_contexts(IndexedTexts const &texts,
                                             std::vector<WordUID> const &words,
                                             int64_t n_gram){
    util::Timer timer;
//    auto iter = util::IterChunkIndex_factory(texts.sents_uid.get());
    auto n = IndexedTexts::Index::from_unsigned(texts.sents_uid.size());
    std::vector<std::pair<WordUID, IndexedTexts::Index>> indexed_words;
    indexed_words.reserve(texts.size());
    IndexedTexts::Index idx{0};
    for(auto word : texts.words_uid) indexed_words.push_back({word,idx++});
    timer.here_then_reset("get_ngram_contexts : Prepare data.");
    tbb::parallel_sort(indexed_words.begin(), indexed_words.end(), [](auto x, auto y){return x.first<y.first;});
    timer.here_then_reset("get_ngram_contexts : Sort words by their UIDs.");

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
    timer.here_then_reset("get_ngram_contexts : Get context words");
    auto counts = contexts_count.to_vector();
    timer.here_then_reset("get_ngram_contexts : Build results");
    return counts;
}

std::vector<VocaInfo::val_t>
guess_word_embedding_from_context(VocaInfo const& base, std::vector<ContextCount> const& words){
    using Vector = util::math::Vector<VocaInfo::val_t,VocaInfo::dim>;
    std::vector<VocaInfo::val_t> wvecs;
    wvecs.reserve(words.size()*VocaInfo::dim);
    for(auto context_count : words){
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
        util::append(wvecs, wvec._val);
    }
    return wvecs;
}


DerivedVoca split_unseen_words(VocaInfo const &base_voca, std::vector<WordUID> const &new_words){
    auto base_words = base_voca.indexmap.all_words();
    tbb::parallel_sort(base_words.begin(), base_words.end());

    DerivedVoca new_voca;
    for(auto word : new_words) {
        if (!util::binary_find(base_words, word)) new_voca.unseen_words.push_back(word);
        else new_voca.known_words.push_back(word);
    }
    return new_voca;
}

auto derive_word_embedding(VocaInfo const& base_voca,
                           std::vector<WordUID> const& known_words,
                           std::vector<ContextCount> const& unseen_words_context){
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

void write_to_disk(VocaInfo const& base_voca,
                   std::vector<WordUID> const& known_words,
                   std::vector<ContextCount> const& unseen_words_with_context,
                   std::string h5store_name,
                   std::string w2vmodel_name,
                   std::string voca_name){
    auto raw_vecs = wordrep::derive_word_embedding(base_voca, known_words, unseen_words_with_context);
    std::vector<WordUID> words_new_voca =known_words;
    util::append(words_new_voca, util::map(unseen_words_with_context, [](auto x){return x.word;}));
    util::TypedPersistentVector<WordUID> new_uids{voca_name, std::move(words_new_voca)};
    //Add word vector for a special word, -UNKNOWN-.
    auto unknown = base_voca.wvecs[base_voca.indexmap[the_unknown_word_uid()]];
    util::append(raw_vecs, unknown.cbegin(), unknown.cend());
    new_uids.push_back(the_unknown_word_uid());
    assert(raw_vecs.size()==new_uids.size()*wordrep::VocaInfo::dim);
    util::io::H5file outfile{{h5store_name}, util::io::hdf5::FileMode::replace};
    new_uids.write(outfile);
    outfile.writeRawData({w2vmodel_name},  raw_vecs);
}

}//namespace wordrep;

