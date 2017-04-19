#include <fmt/printf.h>

#include "similarity/config.h"

#include "wordrep/preprocessed_sentences.h"
#include "wordrep/similar_words.h"

#include "tests/wiki/test_dataset.h"

#include "utils/flatbuffers/io.h"
#include "utils/profiling.h"
#include "utils/parallel_algorithm.h"

#include "utils/index_range.h"

namespace engine {
namespace test{

struct LookupEntityCandidateIndexDummy{};

struct LookupEntityCandidate{
    using Index = util::IntegerLike<LookupEntityCandidateIndexDummy>;
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

struct LookupEntityTaggedTokenDummy{};
struct LookupEntityTaggedToken{
    using Index = util::IntegerLike<LookupEntityTaggedTokenDummy>;
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


struct LookupIndexedWordsIndexDummy{};
struct LookupIndexedWords{
    using Index = util::IntegerLike<LookupIndexedWordsIndexDummy>;
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

struct EntityMatchPerSent{
    using Key = wordrep::SentUID;
    using Value = LookupEntityCandidate::Index;

    friend bool operator==(EntityMatchPerSent const& x, EntityMatchPerSent const& y){
        return x.key==y.key;
    }
    friend bool operator<(EntityMatchPerSent const& x, EntityMatchPerSent const& y){
        return x.key<y.key;
    }

    Key key;
    Value val;
};

struct WordMatchPerSent{
    using Key = wordrep::SentUID;
    //using Value = wordrep::SimilarWords::Index;
    using Value = wordrep::DPTokenIndex;

    friend bool operator==(WordMatchPerSent const& x, WordMatchPerSent const& y){
        return x.key==y.key;
    }
    friend bool operator<(WordMatchPerSent const& x, WordMatchPerSent const& y){
        return x.key<y.key;
    }

    Key key;
    Value val;
};

struct MatchedTokenPerSent{
    using Key = wordrep::SentUID;
    struct Value{
        friend bool operator==(Value const& x, Value const& y){
            return x.score==y.score;
        }
        friend bool operator>(Value const& x, Value const& y){
            return x.score>y.score;
        }

        wordrep::ConsecutiveTokens query;
        wordrep::ConsecutiveTokens matched;
        double score;
    };

    friend bool operator==(MatchedTokenPerSent const& x, MatchedTokenPerSent const& y){
        return x.key==y.key;
    }
    friend bool operator<(MatchedTokenPerSent const& x, MatchedTokenPerSent const& y){
        if(x.key==y.key) return x.val>y.val;
        return x.key<y.key;
    }

    Key key;
    Value val;
};

struct MatchedTokenReducer{
    using Key = MatchedTokenPerSent::Key;
    struct Value{
        double score;
        std::vector<MatchedTokenPerSent::Value> tokens;
    };

    static MatchedTokenReducer intersection(std::vector<std::vector<MatchedTokenPerSent>>&& xs){
        MatchedTokenReducer commons;
        if(xs.empty()) return commons;

        for(auto& token : xs.back())
            commons.vals[token.key]={token.val.score, {token.val}};
        xs.pop_back();

        for(auto& tokens : xs){
            commons.drop_complement(tokens);
            for(auto& token : tokens) commons.accum_if(token);
        }
        return commons;
    }

    std::vector<std::pair<Key,Value>> top_n_results(size_t n) const {
        auto results = util::to_pairs(vals);
        auto beg = results.begin();
        auto end = results.end();
        std::partial_sort(beg, beg+n, end, [](auto& x, auto& y){return x.second.score > y.second.score;});
        results.erase(beg+n, end);
        return results;
    }

    void score_filtering(double cutoff=0.0){
        for(auto it = vals.begin(); it != vals.end(); ){
            if(it->second.score > cutoff) {
                ++it;
                continue;
            }
            it = vals.erase(it);
        }
    }
    bool accum_if(MatchedTokenPerSent const &token){
        if(!has_key(token.key)) return false;
        vals[token.key].score += token.val.score;
        vals[token.key].tokens.push_back(token.val);
        return true;
    }
    void drop_complement(std::vector<MatchedTokenPerSent> const& tokens) {
        auto keys = util::map(tokens, [](auto& x){return x.key;});
        util::sort(keys);
        for(auto it = vals.begin(); it != vals.end(); ){
            if(util::binary_find(keys, it->first)) {
                ++it;
                continue;
            }
            it = vals.erase(it);
        }
    }
    std::map<Key,Value> vals;
private:
    bool has_key(Key key) const {
        if(vals.find(key)==vals.cend()) return false;
        return true;
    }
};


auto scoring_dep_word(LookupEntityCandidate const& candidates, EntityMatchPerSent i){
    return candidates.score(i.val);
}

auto governour_word(wordrep::DPTokenIndex idx, wordrep::DepParsedTokens const& texts){
    return texts.head_uid(idx);
}
auto governour_word(wordrep::ConsecutiveTokens const& phrase, wordrep::DepParsedTokens const& texts){
    return governour_word(phrase.dep_token_idx(texts), texts);
}

struct OpWordSim{
    OpWordSim(wordrep::WordUID ref_word,
              wordrep::WordImportance const& word_importance,
              wordrep::SimilarWords const&word_sim)
    : ref_word_importance{word_importance.score(ref_word)},
      op_word_sim{word_sim.get_op_sim(ref_word)}
    {}
    auto gov_scoring(wordrep::WordUID word_gov) const{
        auto word_similarity = op_word_sim.similarity(word_gov);
        //for smoothing the effect of word similarity of governer words.
        return 1 + ref_word_importance * word_similarity;
    }
    auto dep_scoring(wordrep::WordUID word_dep) const{
        auto word_similarity = op_word_sim.similarity(word_dep);
        return ref_word_importance * word_similarity;
    }
    wordrep::WordImportance::val_t ref_word_importance;
    wordrep::SimilarWords::OpSimilarity op_word_sim;
};


int query_sent_processing(int argc, char** argv) {
    assert(argc>1);
    auto config_json = util::load_json(argv[1]);
    engine::SubmoduleFactory factory{{config_json}};
    util::Timer timer;

    wikidata::test::UnittestDataset testset{{config_json}};
    timer.here_then_reset("Load test dataset.");

    std::unique_ptr<wordrep::WordImportance> word_importance;
    std::unique_ptr<wordrep::WordUIDindex> wordUIDs;
    std::unique_ptr<wordrep::DepParsedTokens> texts;
    std::unique_ptr<wikidata::EntityModule> f{};
    std::unique_ptr<wordrep::VocaInfo> voca{};
    std::unique_ptr<wordrep::SimilarWords> word_sim{};
    wordrep::AnnotationData annotated_tokens;
    wordrep::PreprocessedSentences data_sent;

    std::unique_ptr<LookupIndexedWords> words;
    std::vector<wordrep::Sentence> sents;

    auto load_word_uids =[&wordUIDs,&factory](){
        wordUIDs = std::make_unique<wordrep::WordUIDindex>(factory.conf.word_uid);
    };
    auto load_word_scores =[&word_importance,&factory](){
        word_importance = std::make_unique<wordrep::WordImportance>(factory.word_importance());
    };
    auto load_annotation =[&annotated_tokens,&factory](){
        annotated_tokens = factory.load_annotation();
    };
    auto load_indexed_text=[&texts,&sents,&words,&factory](){
        texts = std::make_unique<wordrep::DepParsedTokens>(factory.dep_parsed_tokens());
        util::parallel_invoke(
                [&](){sents = texts->IndexSentences();},
                [&](){words = std::make_unique<LookupIndexedWords>(LookupIndexedWords::factory(*texts));}
        );
    };
    auto load_word_embedding = [&voca,&factory](){
        voca = std::make_unique<wordrep::VocaInfo>(factory.voca_info());
    };
    auto load_wiki_module = [&f,&factory](){
        f = std::make_unique<wikidata::EntityModule>(factory.wikientity_module());
    };
    auto load_wordsim_table = [&word_sim,&factory](){
        word_sim = std::make_unique<wordrep::SimilarWords>(factory.similar_words());
    };

    auto serial_load = [&](){
        load_annotation();
        timer.here_then_reset("load_query_engine::load_annotation");
        load_indexed_text();
        timer.here_then_reset("load_query_engine::load_indexed_text");
        load_wiki_module();
        timer.here_then_reset("load_query_engine::load_wiki_module");
        load_word_embedding();
        timer.here_then_reset("load_query_engine::load_word_embedding");
        load_word_uids();
        timer.here_then_reset("load_query_engine::load_word_uids");
    };
    //serial_load();
//    return 0;
    util::parallel_invoke(load_annotation,
                          load_wordsim_table,
                          load_word_uids,
                          load_wiki_module,
                          load_word_scores,
                          load_word_embedding,
                          load_indexed_text);
    timer.here_then_reset("Concurrent loading of binary files");

    wordrep::Scoring scoring{*word_importance, voca->wvecs};
    wordrep::Scoring::Preprocess scoring_preprocessor{scoring, testset.entity_reprs};
    testset.tokens.build_voca_index(voca->indexmap);
    timer.here_then_reset("Complete to load data structures.");

    auto candidates = LookupEntityCandidate::factory(annotated_tokens);
    timer.here_then_reset("Aggregate WikidataUID sorted entities.");
    auto ner_tagged_tokens = LookupEntityTaggedToken::factory(annotated_tokens);
    timer.here_then_reset("Aggregate DPTokenIndex sorted tagged tokens.");

    auto& sent = testset.sents.front();
    auto tagged_sent = testset.annotator.annotate(sent);
    auto preprocessed_sent = scoring_preprocessor.sentence(tagged_sent);
    preprocessed_sent.filter_false_named_entity(testset.op_named_entity);
    auto named_entities = preprocessed_sent.all_named_entities();
    timer.here_then_reset("Annotate a query sentence.");

    using util::map;
    using util::concat_mapmap;
    using util::append;

    auto pre_results_per_entity = map(preprocessed_sent.entities, [&](auto& e) {
        return concat_mapmap(candidates.find(e.uid), [&](auto i) {
            return EntityMatchPerSent{texts->sent_uid(candidates.token_index(i)), i};
        });
    });
    timer.here_then_reset("Map phase for Wiki entities : lookup named entities.");
    auto keys_per_ambiguous_entity=util::mapmap(pre_results_per_entity, [](auto x){return x.key;});
    timer.here_then_reset("Map phase for Wiki entities : get keys.");
    auto common_keys = util::intersection(keys_per_ambiguous_entity);
    timer.here_then_reset("Map phase for Wiki entities : get intersection of keys.");

    util::sort(common_keys);
    for(auto& matched_indexes : pre_results_per_entity) {
        auto last = std::partition(matched_indexes.begin(), matched_indexes.end(), [&](auto &token) {
            if (util::binary_find(common_keys, token.key)) return true;
            return false;
        });
        matched_indexes.erase(last, matched_indexes.end());
    }
    timer.here_then_reset("Map phase for Wiki entities : filter with common keys.");

    auto get_op_word_sim = [&word_importance, &word_sim](wordrep::WordUID word_gov){
        return OpWordSim{word_gov, *word_importance, *word_sim};
    };
    std::vector<std::vector<MatchedTokenPerSent>> matched_tokens_per_entity;
    assert(pre_results_per_entity.size()==preprocessed_sent.entities.size());
    auto nw = pre_results_per_entity.size();
    for(decltype(nw)i=0; i!=nw; ++i){
        auto& entity = preprocessed_sent.entities.at(i);
        auto& entity_name = entity.idxs;
        auto op_gov_similarity = get_op_word_sim(entity.word_gov);

        auto& entity_occurences = pre_results_per_entity.at(i);
        auto matched_tokens = map(entity_occurences, [&](auto i){
            auto idx = candidates.token_index(i.val);
            //texts, e, op_gov_word_similarity, i
            auto key = texts->sent_uid(idx);
            auto m_words = ner_tagged_tokens.find(idx);
            assert(m_words);
            auto entity_words = m_words.value();
            auto data_word_gov = governour_word(entity_words, *texts);
            auto score_gov = op_gov_similarity.gov_scoring(data_word_gov);
            auto score_dep = candidates.score(i.val);
            auto match_score = score_dep * score_gov;
            return MatchedTokenPerSent{key, {entity_name, entity_words, match_score}};
        });
        timer.here_then_reset("Map phase for Wiki entities : get matched_tokens.");
        util::drop_duplicates(matched_tokens);
        timer.here_then_reset("Map phase for Wiki entities : drop duplicates of matched_tokens.");
        matched_tokens_per_entity.push_back(matched_tokens);
    }
    timer.here_then_reset("Map phase for Wiki entities.");

    auto matched_results = MatchedTokenReducer::intersection(std::move(matched_tokens_per_entity));
    timer.here_then_reset("Map phase for words.");

    //TODO : Remove assumption that query sent contains one or more named entities.
    auto matches_per_word = map(preprocessed_sent.words, [&](auto& dep_pair){
        std::vector<WordMatchPerSent> matches;
        if(word_importance->is_noisy_word(dep_pair.word_dep))
            return matches;
        auto similar_words = word_sim->find(dep_pair.word_dep);
        for(auto simword_idx : similar_words){
            auto similar_word    = word_sim->sim_word(simword_idx);
            auto word_occurences   = words->find(similar_word);
            matches.reserve(matches.size()+word_occurences.size());
            for(auto idx : word_occurences){
                auto token_idx = words->token_index(idx);
                auto key = texts->sent_uid(token_idx);
                if(!util::binary_find(common_keys, key)) continue;
                matches.push_back({key, token_idx});
            }
        }
        return matches;
    });

    assert(matches_per_word.size()==preprocessed_sent.words.size());
    auto n = matches_per_word.size();
    for(decltype(n)i=0; i!=n; ++i){
        auto& dep_pair = preprocessed_sent.words.at(i);
        auto& matches  = matches_per_word.at(i);

        auto op_gov_similarity = get_op_word_sim(dep_pair.word_gov);
        auto op_dep_similarity = get_op_word_sim(dep_pair.word_dep);

        auto matched_tokens = map(matches, [&](auto& match){
            auto token_idx = match.val;
            auto matched_word = texts->word_uid(token_idx);
            auto score_dep = op_dep_similarity.dep_scoring(matched_word);
            auto data_word_gov = texts->head_uid(token_idx);
            auto score_gov = op_gov_similarity.gov_scoring(data_word_gov);
            auto match_score = score_dep * score_gov;
            return MatchedTokenPerSent{match.key, {dep_pair.idx, token_idx, match_score}};
        });
        util::drop_duplicates(matched_tokens);
        for(auto& token : matched_tokens) matched_results.accum_if(token);
    }
    timer.here_then_reset("Reduce phase on word matches.");
    matched_results.score_filtering();
    auto results = matched_results.top_n_results(5);


    fmt::print(std::cerr, "{} tokens in Wiki candidates data.\n", candidates.size());
    fmt::print(std::cerr, "List of Wikidata entities:\n");
    for(auto entity : testset.entities)
        fmt::print("{}\n", entity.repr(testset.wikidataUIDs, testset.wordUIDs));
    fmt::print(std::cerr, "Annotated sentences in test dataset:\n");

    fmt::print(std::cerr, "SENT: {}\n", tagged_sent.sent.repr(*wordUIDs));
    fmt::print(std::cerr, "TAGGED: {}\n", tagged_sent.repr(testset.entity_reprs, testset.wikidataUIDs, testset.wordUIDs));
    fmt::print(std::cerr, "# of named entities : {}\n",named_entities.size());
    for(auto matched : results){
        auto& sent = sents.at(matched.first.val);
        fmt::print("{} : {}\n", matched.second.score, sent.repr(*wordUIDs));
        for(auto token : matched.second.tokens){
            fmt::print("{} : {}\n",
                       token.query.repr(testset.tokens, *wordUIDs),
                       token.matched.repr(*texts, *wordUIDs));
        }
    }
    timer.here_then_reset("Find candidate entities.");

    auto range = word_sim->find(wordUIDs->get_uid("purchased"));
    for(auto idx : range){
        auto word = word_sim->sim_word(idx);
        fmt::print(std::cerr, "{} {} {}\n",
                   wordUIDs->str(word_sim->word(idx)),
                   wordUIDs->str(word),
                   word_sim->similarity(idx));
        auto matched_words = words->find(word);
        for(auto idx: matched_words){
            auto sent_uid = texts->sent_uid(words->token_index(idx));
            auto& sent = sents.at(sent_uid.val);
            assert(sent.isin(word));
        }
    }

    return 0;
}

}//namespace engine::test
}//namespace engine
