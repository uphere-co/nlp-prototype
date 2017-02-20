#include <cassert>

#include <fmt/printf.h>

#include "wiki/wikidata.h"

#include "similarity/config.h"

#include "wordrep/word_uid.h"

#include "utils/profiling.h"
#include "utils/algorithm.h"
#include "utils/string.h"
#include "utils/json.h"


//using util::get_str;
//using util::find;
//using util::has_key;

namespace wikidata{
namespace test {

void integer_list_ordering(){
    std::vector<int> a1{1,1};
    std::vector<int> a2{1,2};
    std::vector<int> b{1,2,3};
    std::vector<int> c{2,2,3};
    std::vector<int> d{2,3};
    std::vector<std::vector<int>> vs{d,b,c,a2,a1};
    util::sort(vs);
    assert(vs[0]==a1);
    assert(vs[1]==a2);
    assert(vs[2]==b);
    assert(vs[3]==c);
    assert(vs[4]==d);
}

void greedy_matching() {
    std::vector<Entity> items =
            {{1,   {1, 2}},
             {11,  {1, 2}},
             {2,   {1, 3}},
             {3,   {1, 2, 3}},
             {33,  {1, 2, 3}},
             {333, {1, 2, 3}},
             {4,   {2, 3, 4}},
             {5,   {5}},
             {55,  {5}},
             {555, {5}},
             {6,   {6, 7}},
             {7,   {2, 3}}};
    std::sort(items.begin(), items.end());
    SortedEntities entities{items};
    std::vector<wordrep::WordUID> text = {1, 2, 3, 4, 8, 9, 5, 2, 3, 4, 2, 3, 8, 9, 3, 4, 5, 6, 7};

    fmt::print("Entities :\n");
    for (auto &item : entities.entities)
        fmt::print("{}\n", item);
    fmt::print("Text :");
    for (auto t : text)
        fmt::print(" {}", t);
    fmt::print("\n");

    GreedyAnnotator annotator{entities};
    auto tags = annotator.annotate(text);
    for (auto tag : tags)
        fmt::print("{} : {}\n", tag.offset, tag.entity);
}

void uid_lookup_benchmark() {
    util::Timer timer;
    wordrep::WikidataUIDindex wikidataUIDs{"wikidata.uid"};
    fmt::print("{}\n", wikidataUIDs.size());
    timer.here_then_reset("Read Wikidata UIDs.");

    std::vector<std::string> uid_strs = {"Q1", "Q256", "Q102", "Q105", "Q109", "Q10871621"};
    auto uids = util::map(uid_strs, [&wikidataUIDs](auto uid) { return wikidataUIDs[uid]; });
    for (size_t i = 0; i != uid_strs.size(); ++i) {
        fmt::print("{} {}\n", uid_strs[i], wikidataUIDs[uids[i]]);
        //assert(uid_strs[i]==wikidataUIDs[uids[i]]);
    }
    timer.here_then_reset("Finish comparisons.");
}

void compare_wordUIDs_and_WikidataUID(int argc, char** argv){
    util::Timer timer;
    auto config_json = util::load_json(argv[1]);
    std::string query = util::string::read_whole(argv[2]);


    using wordrep::WordUID;
    using wordrep::WikidataUID;

    engine::Config config{config_json};
    engine::SubmoduleFactory factory{config};
    auto wordUIDs = factory.word_uid_index();
    timer.here_then_reset("Load word UIDs.");
    wordrep::WikidataUIDindex wikidataUIDs{"../rnn++/tests/data/wikidata.test.uid"};
    timer.here_then_reset("Load Wikidata UIDs.");
    auto entities = read_wikidata_entities(wordUIDs, "../rnn++/tests/data/wikidata.test.entities");
    timer.here_then_reset("Read items.");

    auto words = util::string::split(query, " ");
    std::vector<WordUID> text = util::map(words, [&wordUIDs](auto x){return wordUIDs[x];});
    GreedyAnnotator annotator{entities};

    timer.here_then_reset("Build data structures.");
    auto tags = annotator.annotate(text);
    timer.here_then_reset(fmt::format("Annotate a query of {} words.", words.size()));
    for(auto tag : tags)
        fmt::print("{} : {}\n", tag.offset, tag.entity.repr(wikidataUIDs, wordUIDs));


    EntityReprs entity_reprs{entities.entities};

    auto exact_match= entity_reprs.get_exact_match_operator();
    auto& ws = wordUIDs;
    auto& ds = wikidataUIDs;
    assert(exact_match(ds["Q1"], {ws["artificial"], ws["intelligence"]}));
    assert(exact_match(ds["Q1"], {ws["AI"]}));
    assert(!exact_match(ds["Q1"], {ws["natural"], ws["language"], ws["processing"]}));
    assert(!exact_match(ds["Q1"], {ws["NLP"]}));

    assert(!exact_match(ds["Q2"], {ws["artificial"], ws["intelligence"]}));
    assert(!exact_match(ds["Q2"], {ws["AI"]}));
    assert(exact_match(ds["Q2"], {ws["natural"], ws["language"], ws["processing"]}));
    assert(exact_match(ds["Q2"], {ws["NLP"]}));

    assert(exact_match(ds["Q3"], {ws["Google"]}));

    assert(!exact_match(ds["Q17948719427"], {ws["artificial"], ws["intelligence"]}));
    assert(!exact_match(ds["Q17948719427"], {ws["AI"]}));
    assert(!exact_match(ds["Q17948719427"], {ws["natural"], ws["language"], ws["processing"]}));
    assert(!exact_match(ds["Q17948719427"], {ws["NLP"]}));
}

void annotate_sentence(int argc, char** argv){
    util::Timer timer;
    auto config_json = util::load_json(argv[1]);
    std::string query = util::string::read_whole(argv[2]);

    using wordrep::WordUID;
    using wordrep::WikidataUID;

    engine::Config config{config_json};
    engine::SubmoduleFactory factory{config};
    auto wordUIDs = factory.word_uid_index();
    auto posUIDs = factory.pos_uid_index();
    auto arclabelUIDs = factory.arclabel_uid_index();
    timer.here_then_reset("Load word UIDs.");

    wordrep::WikidataUIDindex wikidataUIDs{"../rnn++/tests/data/wikidata.test.uid"};
    timer.here_then_reset("Load Wikidata UIDs.");
    auto entities = read_wikidata_entities(wordUIDs, "../rnn++/tests/data/wikidata.test.entities");
    timer.here_then_reset("Read items.");
    GreedyAnnotator annotator{entities};

    EntityReprs entity_reprs{entities.entities};
    auto exact_match= entity_reprs.get_exact_match_operator();
    auto& ws = wordUIDs;
    auto& ds = wikidataUIDs;
    timer.here_then_reset("Build data structures.");

    data::CoreNLPjson test_input{std::string{"../rnn++/tests/data/sentence.1.corenlp"}};
    data::CoreNLPjson test_input2{std::string{"../rnn++/tests/data/sentence.2.corenlp"}};
    wordrep::DepParsedTokens tokens{};
    tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, test_input);
    tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, test_input2);
    tokens.build_sent_uid(0);
    auto sents = tokens.IndexSentences();
    timer.here_then_reset("Prepare test data.");

    fmt::print(std::cerr, "List of Wikidata entities:\n");
    for(auto entity : entities.entities)
        fmt::print(std::cerr, "{}\n", entity.repr(wikidataUIDs, wordUIDs));

    for (auto sent : sents) {
        fmt::print(std::cerr, "{}\n", sent.repr(wordUIDs));
        auto tagged_sent = annotator.annotate(sent);
        for(auto token : tagged_sent.tokens){
            token.token.match([&wordUIDs](WordUID w){fmt::print("{} ", wordUIDs[w]);},
                              [&wikidataUIDs](AmbiguousEntity w){
                                  fmt::print("(");
                                  for(auto uid : w.uids) fmt::print("{} ", wikidataUIDs[uid]);
                                  fmt::print(") ");
                              });
        }
        fmt::print("\n");
    }
}

void test_all(int argc, char** argv) {
    integer_list_ordering();
    greedy_matching();
//    uid_lookup_benchmark();
    compare_wordUIDs_and_WikidataUID(argc, argv);
    annotate_sentence(argc,argv);
}

}//namespace wikidata::test
}//namespace wikidata

int main(int argc, char** argv){
    util::Timer timer;

    wikidata::test::test_all(argc, argv);
    return 0;

    assert(argc>2);
    auto config_json = util::load_json(argv[1]);
    std::string query = util::string::read_whole(argv[2]);

    engine::Config config{config_json};
    engine::SubmoduleFactory factory{config};
    auto wordUIDs = factory.word_uid_index();
    timer.here_then_reset("Load word UIDs.");
    wordrep::WikidataUIDindex wikidataUIDs{"wikidata.uid"};
    timer.here_then_reset("Load Wikidata UIDs.");
    auto entities = wikidata::read_wikidata_entities(wordUIDs, std::move(std::cin));
    timer.here_then_reset("Read items.");

    auto words = util::string::split(query, " ");
    std::vector<wordrep::WordUID> text = util::map(words, [&wordUIDs](auto x){return wordUIDs[x];});
    wikidata::GreedyAnnotator annotator{entities};
    timer.here_then_reset("Build data structures.");
    auto tags = annotator.annotate(text);
    timer.here_then_reset(fmt::format("Annotate a query of {} words.", words.size()));
    for(auto tag : tags)
        fmt::print("{} : {}\n", tag.offset, tag.entity.repr(wikidataUIDs, wordUIDs));
    return 0;
}
