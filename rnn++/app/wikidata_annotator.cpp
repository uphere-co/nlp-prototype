#include <cassert>

#include <fmt/printf.h>
#include <src/similarity/query_engine.h>

#include "wiki/wikidata.h"

#include "similarity/config.h"
#include "similarity/query_engine.h"

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

struct UnittestDataset{
    UnittestDataset(engine::Config const& config)
    : factory{config},
      wordUIDs{factory.word_uid_index()},
      entities{read_wikidata_entities(wordUIDs, "../rnn++/tests/data/wikidata.test.entities")},
      entity_reprs{entities.entities},
      annotator{entities} {
        auto posUIDs = factory.pos_uid_index();
        auto arclabelUIDs = factory.arclabel_uid_index();
        std::vector<std::string> jsons = {"../rnn++/tests/data/sentence.1.corenlp",
                                          "../rnn++/tests/data/sentence.2.corenlp",
                                          "../rnn++/tests/data/sentence.3.corenlp",
                                          "../rnn++/tests/data/sentence.4.corenlp"};
        for(auto& json : jsons)
            tokens.append_corenlp_output(wordUIDs, posUIDs, arclabelUIDs, data::CoreNLPjson{json});
        tokens.build_sent_uid(0);
        sents = tokens.IndexSentences();
    }
    engine::SubmoduleFactory factory;
    wordrep::WordUIDindex wordUIDs;
    SortedEntities entities;
    EntityReprs entity_reprs;
    GreedyAnnotator annotator;
    wordrep::DepParsedTokens tokens{};
    std::vector<wordrep::Sentence> sents{};
    wordrep::WikidataUIDindex wikidataUIDs{"../rnn++/tests/data/wikidata.test.uid"};

};

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
    EntityReprs entity_reprs{entities.entities};
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
        fmt::print("{} {} : {}\n", tag.offset, tag.len,  tag.uid);
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
    assert(argc>2);
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
    EntityReprs entity_reprs{entities.entities};
    GreedyAnnotator annotator{entities};

    timer.here_then_reset("Build data structures.");
    auto tags = annotator.annotate(text);
    fmt::print("query:\n{}", query);
    timer.here_then_reset(fmt::format("Annotate a query of {} words.", words.size()));
    for(auto tag : tags)
        fmt::print("{} {} : {}\n", tag.offset, tag.len, entity_reprs[tag.uid].repr(wikidataUIDs, wordUIDs));

    auto op= entity_reprs.get_comparison_operator();
    auto& ws = wordUIDs;
    auto& ds = wikidataUIDs;
    assert(op.exact_match(ds["Q1"], {ws["artificial"], ws["intelligence"]}));
    assert(op.exact_match(ds["Q1"], {ws["AI"]}));
    assert(!op.exact_match(ds["Q1"], {ws["natural"], ws["language"], ws["processing"]}));
    assert(!op.exact_match(ds["Q1"], {ws["NLP"]}));

    assert(!op.exact_match(ds["Q2"], {ws["artificial"], ws["intelligence"]}));
    assert(!op.exact_match(ds["Q2"], {ws["AI"]}));
    assert(op.exact_match(ds["Q2"], {ws["natural"], ws["language"], ws["processing"]}));
    assert(op.exact_match(ds["Q2"], {ws["NLP"]}));

    assert(op.exact_match(ds["Q3"], {ws["Google"]}));

    assert(!op.exact_match(ds["Q17948719427"], {ws["artificial"], ws["intelligence"]}));
    assert(!op.exact_match(ds["Q17948719427"], {ws["AI"]}));
    assert(!op.exact_match(ds["Q17948719427"], {ws["natural"], ws["language"], ws["processing"]}));
    assert(!op.exact_match(ds["Q17948719427"], {ws["NLP"]}));
}

void annotate_sentence(int argc, char** argv){
    util::Timer timer;
    assert(argc>1);
    auto config_json = util::load_json(argv[1]);

    UnittestDataset testset{{config_json}};
    auto& entity_reprs = testset.entity_reprs;
    auto& wikidataUIDs = testset.wikidataUIDs;
    auto& wordUIDs     = testset.wordUIDs;
    timer.here_then_reset("Prepare test data.");

    fmt::print(std::cerr, "List of Wikidata entities:\n");
    for(auto entity : testset.entities.entities)
        fmt::print(std::cerr, "{}\n", entity.repr(wikidataUIDs, wordUIDs));

    for (auto sent : testset.sents) {
        fmt::print(std::cerr, "{}\n", sent.repr(wordUIDs));
        auto tagged_sent = testset.annotator.annotate(sent);
        for(auto token : tagged_sent.tokens){
            fmt::print("{}",token.repr(entity_reprs, wikidataUIDs, wordUIDs));
        }
        fmt::print("\n");
    }
}

void operation_wikiuid_on_sentence(int argc, char** argv){
    util::Timer timer;
    assert(argc>1);
    auto config_json = util::load_json(argv[1]);

    UnittestDataset testset{{config_json}};
    auto& entity_reprs = testset.entity_reprs;
    auto& wikidataUIDs = testset.wikidataUIDs;
    auto& wordUIDs     = testset.wordUIDs;
    auto& tokens       = testset.tokens;

    //chrome_os=Q79531, NLP=q2
    auto chrome_os = wikidataUIDs["Q79531"];
    auto nlp = wikidataUIDs["Q2"];
    auto google = wikidataUIDs["Q3"];
    fmt::print(std::cerr, "List of Wikidata entities:\n");
    for(auto entity : testset.entities.entities)
        fmt::print(std::cerr, "{}\n", entity.repr(wikidataUIDs, wordUIDs));

    auto op= entity_reprs.get_comparison_operator();
    auto op_contain_chrome_os = entity_reprs.get_comparison_operator(chrome_os);
    auto op_contain_nlp = entity_reprs.get_comparison_operator(nlp);
    auto op_contain_google = entity_reprs.get_comparison_operator(google);
    for (auto sent : testset.sents) {
        auto iter_words = sent.iter_words();
        auto end = iter_words.end();
        for(auto it=iter_words.begin(); it!=end; ++it){
            fmt::print(std::cerr, "{}({}_{}_{}) ", wordUIDs[*it],
                       op.exact_match(chrome_os, it, end),
                       op_contain_chrome_os.exact_match(it, end),
                       op_contain_nlp.exact_match(it, end));
        }
        fmt::print(std::cerr, "\nsent:{} {}\n", sent.front(), sent.back());
        //TODO: should be empty if we don't match "Google" and "Google Chrome".
        assert(!is_contain(sent, op_contain_google).empty());
        auto xs = is_contain(sent, op_contain_chrome_os);
        for(auto x : xs){
            fmt::print(std::cerr, "{} {}:", x.idx, x.len);
            for(decltype(x.len)i=0; i<x.len; ++i){
                fmt::print(std::cerr, " ({} {} {})", wordUIDs[tokens.word_uid(x.idx+i)],
                           tokens.word_pos(x.idx+i), tokens.head_pos(x.idx+i).value());
            }
            fmt::print(std::cerr, "\n");
            auto heads = head_word(tokens, x);
            for(auto head : heads) fmt::print(std::cerr, "{} ", head);
            fmt::print(std::cerr, "\n");
        }
        assert(is_contain(sent, op_contain_nlp).empty());
    }
}

void operation_ambiguous_entity_on_sentence(int argc, char** argv){
    assert(argc>1);
    auto config_json = util::load_json(argv[1]);

    UnittestDataset testset{{config_json}};
    auto& entity_reprs = testset.entity_reprs;
    auto& wikidataUIDs = testset.wikidataUIDs;
    auto& wordUIDs     = testset.wordUIDs;

    auto tagged_sent = testset.annotator.annotate(testset.sents[0]);
    auto tagged_sent1 = testset.annotator.annotate(testset.sents[1]);
    auto es =tagged_sent.get_entities();
    auto es1 = tagged_sent1.get_entities();
    assert(es==es1);
    assert(es[0]!=es1[1]);
    auto& test_sent = testset.sents[1];
    for(auto token : tagged_sent.tokens){
        fmt::print("{}", token.repr(entity_reprs, wikidataUIDs, wordUIDs));
        token.val.match([&test_sent,&entity_reprs,&wikidataUIDs](AmbiguousEntity w){
                              auto op=entity_reprs.get_comparison_operator(w);
                              auto matched_tokens = is_contain(test_sent, op);
                              fmt::print("{} : {}\n", wikidataUIDs[w.entities.front().uid], matched_tokens.size());
                          },
                          [](auto ){});
    }
    fmt::print("\n");
}

void ambiguous_entity_match_scoring(int argc, char** argv){
    util::Timer timer;
    assert(argc>1);
    auto config_json = util::load_json(argv[1]);

    UnittestDataset testset{{config_json}};
    auto& entity_reprs = testset.entity_reprs;
    auto& wikidataUIDs = testset.wikidataUIDs;
    auto& wordUIDs     = testset.wordUIDs;
    auto& annotator    = testset.annotator;
    engine::SubmoduleFactory factory{{config_json}};
    auto word_importance = factory.word_importance();
    auto voca = factory.voca_info();
    auto word_sim_cache = engine::WordSimCache{voca};

    auto tagged_sent0  = annotator.annotate(testset.sents[0]);
    auto tagged_sent1  = annotator.annotate(testset.sents[1]);
    auto tagged_query0 = annotator.annotate(testset.sents[2]);
    auto tagged_query1 = annotator.annotate(testset.sents[3]);

    for(auto idx : testset.sents[2])
        fmt::print("{}({}) ", wordUIDs[testset.tokens.word_uid(idx)],
                   wordUIDs[testset.tokens.head_uid(idx)]);
    fmt::print("\n");
    for(auto& token : tagged_sent0)
        fmt::print("{}", token.repr(entity_reprs, wikidataUIDs, wordUIDs));
    fmt::print("\n");

    auto uids = {"Q1","Q2","Q3","Q79531"};
    auto qs = util::map(uids,[&entity_reprs,&wikidataUIDs](auto uid){
        return entity_reprs.get_synonyms(wikidataUIDs[uid]);});
    for(auto& q : qs){
        for(auto words : q.reprs) {
            fmt::print("{}\n",words.repr(wordUIDs));
            for(auto word : words.uids)
                fmt::print("{} : {}\n", wordUIDs[word], word_importance.score(word));
        }
    }

    auto words = {"DeepMind","deepmind","Obma","Obama", "afoidsfjlaksjd"};
    for(auto word_str : words){
        auto word= wordUIDs[word_str];
        auto vidx = voca.indexmap[word];
        fmt::print("{} {} : {}:score {}:vidx\n",
                   word_str, wordUIDs[word],
                   word_importance.score(word), vidx);
    }
}

void test_all(int argc, char** argv) {
    integer_list_ordering();
    greedy_matching();
//    uid_lookup_benchmark();
    compare_wordUIDs_and_WikidataUID(argc, argv);
    annotate_sentence(argc,argv);
    operation_wikiuid_on_sentence(argc,argv);
    operation_ambiguous_entity_on_sentence(argc,argv);
    ambiguous_entity_match_scoring(argc,argv);
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
    wordrep::WikidataUIDindex is_ne{"wikidata.uid.ne"};
    timer.here_then_reset("Load Wikidata UIDs.");

    auto entities = wikidata::read_wikidata_entities(wordUIDs, std::move(std::cin));
    timer.here_then_reset("Read items.");
    //TODO: Constructing EntityReprs with 14M entities takes more than 20s!
    wikidata::EntityReprs entity_reprs{entities.entities};
    timer.here_then_reset("Build data structures.");
    wikidata::GreedyAnnotator annotator{std::move(entities)}; //Move. It took a few seconds, otherwise.
    timer.here_then_reset("Build data structures.");
    auto words = util::string::split(query, " ");
    std::vector<wordrep::WordUID> text = util::map(words, [&wordUIDs](auto x){return wordUIDs[x];});
    auto tags = annotator.annotate(text);
    timer.here_then_reset(fmt::format("Annotate a query of {} words.", words.size()));
    for(auto tag : tags)
//        fmt::print("{} {} : {}\n", tag.offset, tag.len, wikidataUIDs[tag.uid]);
        fmt::print("{} {} : {}\n", tag.offset, tag.len, entity_reprs[tag.uid].repr(wikidataUIDs, wordUIDs));
    return 0;
}
