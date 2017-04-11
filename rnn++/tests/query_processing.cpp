#include <fmt/printf.h>

#include "similarity/config.h"

#include "wordrep/preprocessed_sentences.h"

#include "tests/wiki/test_dataset.h"

#include "utils/flatbuffers/io.h"
#include "utils/profiling.h"

namespace engine {
namespace test{

int load_query_engine_data(int argc, char** argv) {
    assert(argc>1);
    namespace fb = util::io::fb;
    auto config_json = util::load_json(argv[1]);
    engine::SubmoduleFactory factory{{config_json}};
    util::Timer timer;

    wikidata::test::UnittestDataset testset{{config_json}};
    timer.here_then_reset("Load test dataset.");

    std::unique_ptr<wordrep::WordUIDindex> wordUIDs;
    std::unique_ptr<wordrep::DepParsedTokens> texts;
    std::unique_ptr<wikidata::EntityModule> f{};
    std::unique_ptr<wordrep::VocaInfo> voca{};
    wordrep::AnnotationData annotated_tokens;

    auto load_word_uids =[&wordUIDs,&factory](){
        wordUIDs = std::make_unique<wordrep::WordUIDindex>(factory.conf.word_uid);
    };
    auto load_annotation =[&annotated_tokens,&factory](){
        annotated_tokens = factory.load_annotation();
    };
    auto load_indexed_text=[&texts,&factory](){
        texts = std::make_unique<wordrep::DepParsedTokens>(factory.dep_parsed_tokens());
    };
    auto load_word_embedding = [&voca,&factory](){
        voca = std::make_unique<wordrep::VocaInfo>(factory.voca_info());
    };
    auto load_wiki_module = [&f,&factory](){
        f = std::make_unique<wikidata::EntityModule>(factory.wikientity_module());
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
                          load_word_uids,
//                          load_wiki_module,
//                          load_word_embedding,
                          load_indexed_text);

    timer.here_then_reset("Concurrent loading of binary files");
//    auto sents = texts->IndexSentences();
//    auto data_sent = wordrep::PreprocessedSentences::factory(sents, annotated_tokens);
//    timer.here_then_reset("Post processing of indexed texts.");

    timer.here_then_reset("Complete to load data structures.");

    fmt::print(std::cerr, "List of Wikidata entities:\n");
    for(auto entity : testset.entities)
        fmt::print(std::cerr, "{}\n", entity.repr(testset.wikidataUIDs, testset.wordUIDs));
    fmt::print(std::cerr, "Annotated sentences in test dataset:\n");
    for (auto sent : testset.sents) {
        fmt::print(std::cerr, "{}\n", sent.repr(*wordUIDs));
        auto tagged_sent = testset.annotator.annotate(sent);
        fmt::print(std::cerr, "{}\n", tagged_sent.repr(testset.entity_reprs, testset.wikidataUIDs, testset.wordUIDs));
    }

    return 0;
}

}//namespace engine::test
}//namespace engine
