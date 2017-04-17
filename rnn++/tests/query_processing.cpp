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

struct MatchedTokenPerSent{
    using Key = wordrep::SentUID;
    struct Value{
        wordrep::ConsecutiveTokens query;
        wordrep::ConsecutiveTokens matched;
        double score;
    };

    Key key;
    Value val;
};

struct MatchedTokenReducer{
    using Key = MatchedTokenPerSent::Key;
    struct Value{
        double score;
        std::vector<MatchedTokenPerSent::Value> tokens;
    };

    void score_filtering(double cutoff=0.0){
        for(auto it = vals.begin(); it != vals.end(); ){
            if(it->second.score > cutoff) {
                ++it;
                continue;
            }
            it = vals.erase(it);
        }
    }
    bool accum(MatchedTokenPerSent const& token){
        if(vals.find(token.key)==vals.cend()) return false;
        vals[token.key].score += token.val.score;
        vals[token.key].tokens.push_back({token.val.query,token.val.matched,token.val.score});
        return true;
    }

    std::map<Key,Value> vals;
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
    timer.here_then_reset("Aggregate to UID sorted tokens.");


    auto& sent = testset.sents.front();
    auto tagged_sent = testset.annotator.annotate(sent);
    auto preprocessed_sent = scoring_preprocessor.sentence(tagged_sent);
    preprocessed_sent.filter_false_named_entity(testset.op_named_entity);
    auto named_entities = preprocessed_sent.all_named_entities();
    timer.here_then_reset("Annotate a query sentence.");

    using util::map;
    using util::concat_map;
    using util::append;

    auto keys_per_ambiguous_entity = map(named_entities, [&](auto& e){
        auto ranges = candidates.find(e);
        return concat_map(ranges, [&](auto i){return texts->sent_uid(candidates.token_index(i));});
    });
    timer.here_then_reset("Map phase for Wiki entities.");

    auto ner_matched_tokens  = util::intersection(keys_per_ambiguous_entity);
    MatchedTokenReducer matched_results;
    for(auto& sent : ner_matched_tokens) matched_results.vals[sent] = {0.0, {}};
    timer.here_then_reset("Map phase for words.");

    for(auto dep_pair : preprocessed_sent.words){
        auto word       = dep_pair.word_dep;
        auto word_score = word_importance->score(word);
        auto word_gov   = dep_pair.word_gov;
        auto similar_words     = word_sim->find(word);
        auto similar_words_gov = word_sim->find(word_gov);
        auto op_gov_word_similarity = [&](auto gov){
            for(auto idx : similar_words_gov){
                if(gov == word_sim->sim_word(idx))
                    return word_sim->similarity(idx);
            }
            return decltype(word_sim->similarity(0)){0.0};
        };
        std::vector<MatchedTokenPerSent> matched_tokens;
        for(auto simword_idx : similar_words){
            auto similar_word    = word_sim->sim_word(simword_idx);
            auto word_similarity = word_sim->similarity(simword_idx);
            auto matched_idxs   = words->find(similar_word);
            for(auto idx : matched_idxs){
                auto token_idx = words->token_index(idx);
                auto sent_uid  = texts->sent_uid(token_idx);
                auto word_gov_similarity = op_gov_word_similarity(texts->head_uid(token_idx));
                matched_tokens.push_back({sent_uid,
                                          {{dep_pair.idx},
                                           {token_idx},
                                           word_score*word_similarity*(0.5+word_gov_similarity)}});
            }
        }
        util::sort(matched_tokens, [](auto&x, auto& y){return x.val.score>y.val.score;});
        auto last = std::unique(matched_tokens.begin(), matched_tokens.end(), [](auto&x, auto& y){return x.key==y.key;});
        matched_tokens.erase(last, matched_tokens.end());
        for(auto& token : matched_tokens) matched_results.accum(token);
    }
    timer.here_then_reset("Reduce phase.");
    matched_results.score_filtering();

    fmt::print(std::cerr, "{} tokens in Wiki candidates data.\n", candidates.size());
    fmt::print(std::cerr, "List of Wikidata entities:\n");
    for(auto entity : testset.entities)
        fmt::print("{}\n", entity.repr(testset.wikidataUIDs, testset.wordUIDs));
    fmt::print(std::cerr, "Annotated sentences in test dataset:\n");

    fmt::print(std::cerr, "SENT: {}\n", tagged_sent.sent.repr(*wordUIDs));
    fmt::print(std::cerr, "TAGGED: {}\n", tagged_sent.repr(testset.entity_reprs, testset.wikidataUIDs, testset.wordUIDs));
    fmt::print(std::cerr, "# of named entities : {}\n",named_entities.size());
    for(auto matched : matched_results.vals){
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
