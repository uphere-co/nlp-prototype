#pragma once

#include <map>

#include "wordrep/indexed_text.h"
#include "wordrep/word_uid.h"
#include "wordrep/voca_info.h"

namespace wordrep{

struct ContextCount;
struct ContextCountRepr{
    ContextCount const& self;
    WordUIDindex const& wordUIDs;
    friend std::ostream& operator<<(std::ostream& os, ContextCountRepr const& src);
};

struct ContextCount{
    ContextCountRepr repr(WordUIDindex const& wordUIDs){
        return {*this, wordUIDs};
    }
    WordUID word;
    std::map<WordUID,size_t> count;
};


std::ostream& operator<<(std::ostream& os, ContextCountRepr const& src);

struct DerivedVoca{
    std::vector<WordUID> known_words;
    std::vector<WordUID> unseen_words;
};

std::vector<ContextCount> get_ngram_contexts(IndexedTexts const &texts,
                                             std::vector<WordUID> const &words,
                                             int64_t n_gram = 10);

std::vector<VocaInfo::val_t>
guess_word_embedding_from_context(VocaInfo const& base, std::vector<ContextCount> const& words);

DerivedVoca split_unseen_words(VocaInfo const &base_voca, std::vector<WordUID> const &new_words);

auto derive_word_embedding(VocaInfo const& base_voca,
                           std::vector<WordUID> const& known_words,
                           std::vector<ContextCount> const& unseen_words_context);

void write_to_disk(VocaInfo const& base_voca,
                   std::vector<WordUID> const& known_words,
                   std::vector<ContextCount> const& unseen_words_with_context,
                   std::string h5store_name,
                   std::string w2vmodel_name,
                   std::string voca_name);

}//namespace wordrep;

