#include <fmt/printf.h>

#include "similarity/config.h"
#include "similarity/query_processor.h"

#include "wordrep/preprocessed_sentences.h"

#include "tests/wiki/test_dataset.h"

#include "utils/flatbuffers/io.h"
#include "utils/profiling.h"


namespace engine {
namespace test{

int query_sent_processing(int argc, char** argv) {
    assert(argc>1);
    auto config_json = util::load_json(argv[1]);
    engine::SubmoduleFactory factory{{config_json}};
    util::Timer timer;

    std::unique_ptr<wordrep::VocaInfo> voca{};
    std::unique_ptr<wordrep::WordImportance> word_importance;
    std::unique_ptr<wordrep::WordUIDindex> wordUIDs;

    auto load_word_uids =[&wordUIDs,&factory](){
        wordUIDs = std::make_unique<wordrep::WordUIDindex>(factory.conf.word_uid);
    };
    auto load_word_embedding = [&voca,&factory](){
        voca = std::make_unique<wordrep::VocaInfo>(factory.voca_info());
    };
    auto load_word_scores =[&word_importance,&factory](){
        word_importance = std::make_unique<wordrep::WordImportance>(factory.word_importance());
    };

    util::parallel_invoke(load_word_uids,
                          load_word_embedding,
                          load_word_scores);
    wikidata::test::UnittestDataset testset{{config_json}};
    wordrep::Scoring scoring{*word_importance, voca->wvecs};
    wordrep::Scoring::Preprocess scoring_preprocessor{scoring, testset.entity_reprs};
    testset.tokens.build_voca_index(voca->indexmap);
    timer.here_then_reset("Load test dataset.");

    auto engine = QueryProcessor::factory(factory);
    auto sents = engine.texts->IndexSentences();
    timer.here_then_reset("Load engine.");

    auto& sent = testset.sents.front();
    auto tagged_sent = testset.annotator.annotate(sent);
    auto preprocessed_sent = scoring_preprocessor.sentence(tagged_sent);
    preprocessed_sent.filter_false_named_entity(testset.op_named_entity);
    timer.here_then_reset("Annotate a query sentence.");

    auto matched_results = engine.find_similar_sentences(preprocessed_sent);
    matched_results.score_filtering();
    auto results = matched_results.top_n_results(5);

    auto named_entities = preprocessed_sent.all_named_entities();

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
                       token.matched.repr(*engine.texts, *wordUIDs));
        }
    }
    timer.here_then_reset("Find candidate entities.");
    return 0;
}

}//namespace engine::test
}//namespace engine
