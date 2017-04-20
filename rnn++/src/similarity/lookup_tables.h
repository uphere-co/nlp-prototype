#pragma once

#include <memory>

#include "wordrep/io.h"
#include "wordrep/dep_parsed.h"

#include "utils/base_types.h"
#include "utils/index_range.h"
#include "utils/parallel_algorithm.h"
#include "utils/profiling.h"

namespace dummy{
struct LookupEntityCandidateIndexDummy{};
struct LookupEntityTaggedTokenDummy{};
struct LookupIndexedWordsIndexDummy{};
}//namespace dummy

namespace engine {

struct LookupEntityCandidate{
    using Index = util::IntegerLike<dummy::LookupEntityCandidateIndexDummy>;
    using Range = util::IndexRange<Index>;

    using key_type   = wordrep::WikidataUID;
    using value_type = wordrep::io::EntityCandidate;
    static key_type to_key(value_type const& x) {return x.wiki_uid();};

    static LookupEntityCandidate factory(wordrep::AnnotationData const& tokens){
        util::Timer timer;
        LookupEntityCandidate candidates;

        for(auto const& block : tokens.blocks){
            util::append(*candidates.tokens, block->candidates);
        }
        timer.here_then_reset("Aggregate tokens.");

        tbb::parallel_sort(candidates.tokens->begin(),
                           candidates.tokens->end(),
                           [](auto &x, auto &y){return to_key(x) < to_key(y);});
        timer.here_then_reset("Sort tokens by WikiUID.");

        return candidates;
    }

    Range find(key_type entity) const{
        auto eq   = [entity](auto& x){return entity==to_key(x);};
        auto less = [entity](auto& x){return entity< to_key(x);};
        auto m_pair = util::binary_find_block(*tokens, eq, less);
        if(!m_pair) return {0,0};
        auto beg = m_pair->first  - tokens->cbegin();
        auto end = m_pair->second - tokens->cbegin();
        return {beg,end};
    }
    std::vector<Range> find(wordrep::wiki::AmbiguousUID const& ambi_uid) const{
        return util::map(ambi_uid.candidates, [this](auto uid){return this->find(uid);});
    }
    auto& at(Index idx) const{return tokens->at(idx.val);}
    wordrep::DPTokenIndex token_index(Index idx) const {return at(idx).token_idx();}
    wordrep::WikidataUID  wiki_uid(Index idx)    const {return at(idx).wiki_uid();}
    auto score(Index idx) const {return at(idx).score();}
    auto size() const{return tokens->size();}
private:
    LookupEntityCandidate()
            : tokens{std::make_unique<tbb::concurrent_vector<value_type>>()}
    {}
    std::unique_ptr<tbb::concurrent_vector<value_type>> tokens;
};

struct LookupEntityTaggedToken{
    using Index = util::IntegerLike<dummy::LookupEntityTaggedTokenDummy>;
    using Range = util::IndexRange<Index>;

    using key_type   = wordrep::DPTokenIndex ;
    using value_type = wordrep::io::TaggedToken;
    static key_type to_key(value_type const& x) {return x.token_idx();};

    static LookupEntityTaggedToken factory(wordrep::AnnotationData const& tokens){
        util::Timer timer;
        LookupEntityTaggedToken tagged_entities;

        for(auto const& block : tokens.blocks){
            util::append(*tagged_entities.tokens, block->tagged_tokens);
        }
        timer.here_then_reset("Aggregate tokens.");

        tbb::parallel_sort(tagged_entities.tokens->begin(),
                           tagged_entities.tokens->end(),
                           [](auto &x, auto &y){return to_key(x) < to_key(y);});
        timer.here_then_reset("Sort tokens by WikiUID.");

        return tagged_entities;
    }

    std::optional<wordrep::ConsecutiveTokens> find(key_type idx) const{
        auto eq   = [idx](auto& x){return idx==to_key(x);};
        auto less = [idx](auto& x){return idx< to_key(x);};
        auto m_token = util::binary_find(*tokens, eq, less);
        if(!m_token) return {};
        auto token = m_token.value();
        return {{token->token_idx(), token->token_len()}};
    }
    auto size() const{return tokens->size();}
private:
    LookupEntityTaggedToken()
            : tokens{std::make_unique<tbb::concurrent_vector<value_type>>()}
    {}
    std::unique_ptr<tbb::concurrent_vector<value_type>> tokens;
};


struct LookupIndexedWords{
    using Index = util::IntegerLike<dummy::LookupIndexedWordsIndexDummy>;
    using Range = util::IndexRange<Index>;

    using key_type   = wordrep::WordUID;
    using value_type = wordrep::IndexedWord;
    static key_type to_key(value_type const& x) {return x.word;};

    static LookupIndexedWords factory(wordrep::DepParsedTokens const& texts){
        auto indexed_words = texts.indexed_words();
        tbb::parallel_sort(indexed_words.begin(),
                           indexed_words.end(),
                           [](auto &x, auto &y){return to_key(x) < to_key(y);});
        return {std::move(indexed_words)};
    }

    Range find(key_type word) const{
        auto eq   = [word](auto& x){return word==to_key(x);};
        auto less = [word](auto& x){return word< to_key(x);};
        auto m_pair = util::binary_find_block(sorted_words, eq, less);
        if(!m_pair) return {0,0};
        auto beg = m_pair->first  - sorted_words.cbegin();
        auto end = m_pair->second - sorted_words.cbegin();
        return {beg,end};
    }
    wordrep::DPTokenIndex token_index(Index idx) const { return sorted_words.at(idx.val).idx;}
    LookupIndexedWords(tbb::concurrent_vector<value_type>&& words)
            : sorted_words{std::move(words)}
    {}
private:
    tbb::concurrent_vector<value_type> sorted_words;
};

}//namespace engine;
