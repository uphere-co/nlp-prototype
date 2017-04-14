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

    static LookupEntityCandidate factory(wordrep::AnnotationData const& tokens){
        util::Timer timer;
        LookupEntityCandidate candidates;

        for(auto const& block : tokens.blocks){
            util::append(*candidates.tokens, block->candidates);
        }
        timer.here_then_reset("Aggregate tokens.");

        tbb::parallel_sort(candidates.tokens->begin(),
                           candidates.tokens->end());
        timer.here_then_reset("Sort tokens by WikiUID.");

        return candidates;
    }

    Range find(wordrep::WikidataUID uid) const{
        auto m_pair = util::binary_find_block(*tokens, wordrep::io::partial_construct(uid));
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
            : tokens{std::make_unique<tbb::concurrent_vector<wordrep::io::EntityCandidate>>()}
    {}
    std::unique_ptr<tbb::concurrent_vector<wordrep::io::EntityCandidate>> tokens;
};

struct LookupIndexedWordsIndexDummy{};
struct LookupIndexedWords{
    static LookupIndexedWords factory(wordrep::DepParsedTokens const& texts){
        auto indexed_words = texts.indexed_words();
        tbb::parallel_sort(indexed_words.begin(), indexed_words.end());
        return {std::move(indexed_words)};
    }
    using Index = util::IntegerLike<LookupIndexedWordsIndexDummy>;
    using Range = util::IndexRange<Index>;

    Range find(wordrep::WordUID word) const{
        auto m_pair = util::binary_find_block(sorted_words, {word, -1});
        if(!m_pair) return {0,0};
        auto beg = m_pair->first  - sorted_words.cbegin();
        auto end = m_pair->second - sorted_words.cbegin();
        return {beg,end};
    }
    wordrep::DPTokenIndex token_index(Index idx) const { return sorted_words.at(idx.val).idx;}
    LookupIndexedWords(tbb::concurrent_vector<wordrep::IndexedWord>&& words)
            : sorted_words{std::move(words)}
    {}
private:
    tbb::concurrent_vector<wordrep::IndexedWord> sorted_words;
};

int load_query_engine_data(int argc, char** argv) {
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


    for(auto dep_pair : preprocessed_sent.words){
        auto word = dep_pair.word_dep;
        if(word != wordUIDs->get_uid("bought")) continue;
        auto range = word_sim->find(word);
        std::vector<wordrep::SentUID> sent_uids;
        for(auto idx : range){
            auto word = word_sim->sim_word(idx);
            auto similarity_word = word_sim->similarity(idx);
            auto matched_words = words->find(word);
            append(sent_uids, map(matched_words, [&](auto idx){return texts->sent_uid(words->token_index(idx));}));
        }
        keys_per_ambiguous_entity.push_back(sent_uids);
    }
    timer.here_then_reset("Map phase for words.");


    auto matched_sents  = util::intersection(keys_per_ambiguous_entity);
    timer.here_then_reset("Reduce phase.");


    fmt::print(std::cerr, "{} tokens in Wiki candidates data.\n", candidates.size());
    fmt::print(std::cerr, "List of Wikidata entities:\n");
    for(auto entity : testset.entities)
        fmt::print("{}\n", entity.repr(testset.wikidataUIDs, testset.wordUIDs));
    fmt::print(std::cerr, "Annotated sentences in test dataset:\n");

    fmt::print(std::cerr, "SENT: {}\n", tagged_sent.sent.repr(*wordUIDs));
    fmt::print(std::cerr, "TAGGED: {}\n", tagged_sent.repr(testset.entity_reprs, testset.wikidataUIDs, testset.wordUIDs));
    fmt::print(std::cerr, "# of named entities : {}\n",named_entities.size());
    for(auto sent_uid : matched_sents){
        auto& sent = sents.at(sent_uid.val);
        fmt::print("{}\n", sent.repr(*wordUIDs));
    }
    timer.here_then_reset("Find candidate entities.");

    return 0;
    auto range = word_sim->find(wordUIDs->get_uid("purchased"));
    for(auto idx : range){
        auto word = word_sim->sim_word(idx);
        fmt::print("{} {} {}\n",
                   wordUIDs->str(word_sim->word(idx)),
                   wordUIDs->str(word),
                   word_sim->similarity(idx));
        auto matched_words = words->find(word);
        for(auto idx: matched_words){
            auto sent_uid = texts->sent_uid(words->token_index(idx));
            auto& sent = sents.at(sent_uid.val);
            fmt::print("{} : {}\n", wordUIDs->str(word), sent.repr(*wordUIDs));
        }
    }

    return 0;
}

}//namespace engine::test
}//namespace engine
