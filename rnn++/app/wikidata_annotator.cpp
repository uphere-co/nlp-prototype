#include <cassert>

#include <fmt/printf.h>
#include <src/similarity/query_engine.h>

#include "wiki/wikidata.h"

#include "similarity/config.h"
#include "similarity/query_engine.h"

#include "wordrep/word_uid.h"
#include "wordrep/wikientity.h"
#include "wordrep/simiarity_score.h"

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
      entities{read_wikidata_entities(wordUIDs, "../rnn++/tests/data/wikidata.test.entities")},
      annotator{entities},
      wordUIDs{factory.word_uid_index()},
      entity_reprs{entities.entities},
      op_acronym{wordUIDs},
      op_named_entity{"../rnn++/tests/data/wikidata.test.uid.named_entities",
                      wordUIDs, entity_reprs} {
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
    SortedEntities entities;
    GreedyAnnotator annotator;
    wordrep::WordUIDindex wordUIDs;
    wordrep::wiki::EntityReprs entity_reprs;
    wordrep::wiki::OpAcronym op_acronym;
    wordrep::wiki::OpNamedEntity op_named_entity;
    wordrep::WikidataUIDindex wikidataUIDs{"../rnn++/tests/data/wikidata.test.uid"};
    wordrep::DepParsedTokens tokens{};
    std::vector<wordrep::Sentence> sents{};

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

    using wordrep::wiki::Entity;
    assert(!(Entity{1,{1,2}}<Entity{1,{1,2}}));
    assert(!(Entity{11,{1,2}}<Entity{1,{1,2}}));
    assert(!(Entity{1,{1,2}}<Entity{11,{1,2}}));
    assert((Entity{1,{1,3}}<Entity{2,{1,2}}));

    assert((Entity{1,{1,3}}<Entity{2,{1,1,1}}));
    assert((Entity{1,{1,3,1}}<Entity{2,{1,3}}));
    assert((Entity{1,{1,1,2}}<Entity{2,{1,1,1}}));
    assert((Entity{1,{2,1,1}}<Entity{2,{1,2,2}}));
}

void greedy_matching() {
    std::vector<wordrep::wiki::Entity> items =
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
             {7,   {2, 3}},
             {8,   {5, 6, 8}},
             {9,   {9}},
             {10,  {9,10}}};
    std::sort(items.begin(), items.end());
    SortedEntities entities{items};
    std::vector<wordrep::WordUID> text = {1, 2, 3, 4, 8, 9, 5, 2, 3, 4, 2, 3, 8, 9, 3, 4, 5, 6, 7};
    wordrep::wiki::EntityReprs entity_reprs{entities.entities};
    fmt::print("Entities :\n");
    for (auto &item : entities.entities)
        fmt::print("{}\n", item);
    fmt::print("Text :");
    for (auto t : text)
        fmt::print(" {}", t);
    fmt::print("\n");

    GreedyAnnotator annotator{entities};
    auto tags = annotator.annotate(text);
    auto uids = util::map(tags, [](auto tag){return tag.uid;});
    assert(util::isin(uids, {6}));
    assert(!util::isin(uids, {8}));
    for (auto tag : tags)
        fmt::print("{} {} : {}\n", tag.offset, tag.len,  tag.uid);
    {
        std::vector<wordrep::WordUID> text = {9,10};
        auto tags = annotator.annotate(text);
        auto uids = util::map(tags, [](auto tag){return tag.uid;});
        assert(!util::isin(uids, {9}));
        assert(util::isin(uids, {10}));
    }
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

void compare_wordUIDs_and_WikidataUID(util::json_t const& config_json,
                                      std::string query){
    util::Timer timer;

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
    wordrep::wiki::EntityReprs entity_reprs{entities.entities};
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

void annotate_sentence(util::json_t const& config_json){
    std::cerr << "Test: wikidata::test::annotate_sentence"<<std::endl;
    util::Timer timer;

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
        fmt::print("{}\n", tagged_sent.repr(entity_reprs, wikidataUIDs, wordUIDs));
    }
}

void operation_wikiuid_on_sentence(util::json_t const& config_json){
    std::cerr << "Test: wikidata::test::operation_wikiuid_on_sentence"<<std::endl;
    util::Timer timer;

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
            auto heads = head_word_pos(tokens, x);
            for(auto head : heads) fmt::print(std::cerr, "{} ", head);
            fmt::print(std::cerr, "\n");
        }
//        assert(is_contain(sent, op_contain_nlp).empty());
    }
}

void operation_ambiguous_entity_on_sentence(util::json_t const& config_json){
    std::cerr << "Test: wikidata::test::operation_ambiguous_entity_on_sentence"<<std::endl;

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
    fmt::print("{}\n", tagged_sent.repr(entity_reprs, wikidataUIDs, wordUIDs));
    for(auto token : tagged_sent.tokens){
        token.val.match([&test_sent,&entity_reprs,&wikidataUIDs](wordrep::wiki::AmbiguousEntity w){
                              auto op=entity_reprs.get_comparison_operator(w.uid);
                              auto matched_tokens = is_contain(test_sent, op);
                              fmt::print("{} : {}\n", wikidataUIDs[w.uid.candidates.front()], matched_tokens.size());
                          },
                          [](auto ){});
    }
    fmt::print("\n");
}

void ambiguous_entity_match_scoring(util::json_t const& config_json){
    std::cerr << "Test: wikidata::test::ambiguous_entity_match_scoring"<<std::endl;
    util::Timer timer;

    UnittestDataset testset{{config_json}};
    auto& entity_reprs = testset.entity_reprs;
    auto& wikidataUIDs = testset.wikidataUIDs;
    auto& wordUIDs     = testset.wordUIDs;
    auto& annotator    = testset.annotator;
    auto& tokens       = testset.tokens;
    engine::SubmoduleFactory factory{{config_json}};
    auto word_importance = factory.word_importance();
//    auto voca = factory.voca_info();
//    auto word_sim_cache = engine::WordSimCache{voca};

//    auto words = {"DeepMind","deepmind","Obma","Obama", "afoidsfjlaksjd"};
//    for(auto word_str : words){
//        auto word= wordUIDs[word_str];
//        auto vidx = voca.indexmap[word];
//        fmt::print("{} {} : {}:score {}:vidx\n",
//                   word_str, wordUIDs[word],
//                   word_importance.score(word), vidx);
//    }
    for(auto idx : testset.sents[2])
        fmt::print("{}({}) ", wordUIDs[testset.tokens.word_uid(idx)],
                   wordUIDs[testset.tokens.head_uid(idx)]);
    fmt::print("\n");
    fmt::print("{}\n", annotator.annotate(testset.sents[0]).repr(entity_reprs, wikidataUIDs, wordUIDs));

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

    for(auto sent: testset.sents){
        auto tagged_sent = annotator.annotate(sent);
        auto entities = tagged_sent.get_entities();
        for(auto& ambiguous_entity : entities){
            for(auto uid : ambiguous_entity.uid.candidates){
                auto entity = entity_reprs[uid];
                fmt::print("{} : ",entity.repr(wikidataUIDs, wordUIDs));
                for(auto idx : ambiguous_entity.words){
                    fmt::print(" ({}:{})", wordUIDs[tokens.word_uid(idx)],wordUIDs[tokens.head_uid(idx)]);
                }
                fmt::print("\n");
            }
        }
    }
}

void ambiguous_entity_equality(){
    //e1 is one of {0,1,2};
    wordrep::wiki::AmbiguousEntity e1{0,2,{{0,1,2}}};
    //e2 is one of {0,5};
    wordrep::wiki::AmbiguousEntity e2{10,2,{{0,5}}};
    //e3 is one of {2,11};
    wordrep::wiki::AmbiguousEntity e3{0,2,{{2,11}}};
    assert(e1==e2);
    assert(e1==e3);
    assert(e2!=e3);
}
void test_all(int argc, char** argv) {
    assert(argc>2);
    auto config_json = util::load_json(argv[1]);
    std::string query = util::string::read_whole(argv[2]);

    integer_list_ordering();
    greedy_matching();
//    uid_lookup_benchmark();
    compare_wordUIDs_and_WikidataUID(config_json, query);
    annotate_sentence(config_json);
    operation_wikiuid_on_sentence(config_json);
    operation_ambiguous_entity_on_sentence(config_json);
    ambiguous_entity_match_scoring(config_json);
    ambiguous_entity_equality();
}

}//namespace wikidata::test
}//namespace wikidata

namespace wordrep{
namespace test{

void acronyms_check(util::json_t const& config_json) {
    std::cerr << "Test: wordrep::test::acrynyms_check" << std::endl;
    util::Timer timer;

    wikidata::test::UnittestDataset testset{{config_json}};
    auto& entity_reprs = testset.entity_reprs;
    auto& wikidataUIDs = testset.wikidataUIDs;
    auto& wordUIDs     = testset.wordUIDs;
    auto& acronymOps = testset.op_acronym;

    auto ai = entity_reprs.get_synonyms(wikidataUIDs["Q1"]);
    auto nlp = entity_reprs.get_synonyms(wikidataUIDs["Q2"]);
    auto google = entity_reprs.get_synonyms(wikidataUIDs["Q4"]);
    auto deepmind = entity_reprs.get_synonyms(wikidataUIDs["Q5"]);
    auto chromeOS = entity_reprs.get_synonyms(wikidataUIDs["Q79531"]);

    assert(acronymOps.is_acronyms(ai));
    assert(wordUIDs["AI"]==acronymOps.to_acronyms(ai));
    assert(acronymOps.is_acronyms(nlp));
    assert(wordUIDs["NLP"]==acronymOps.to_acronyms(nlp));
    assert(!acronymOps.is_acronyms(google));
    assert(!acronymOps.is_acronyms(deepmind));
    assert(!acronymOps.is_acronyms(chromeOS));
}
void named_entity_check(util::json_t const& config_json) {
    std::cerr << "Test: wordrep::test::named_entity_check" << std::endl;
    util::Timer timer;

    wikidata::test::UnittestDataset testset{{config_json}};
    auto& wikidataUIDs = testset.wikidataUIDs;
    auto& op_named_entity = testset.op_named_entity;

    auto ai       = wikidataUIDs["Q1"];
    auto nlp      = wikidataUIDs["Q2"];
    auto google   = wikidataUIDs["Q3"];
    auto google2  = wikidataUIDs["Q4"];
    auto deepmind = wikidataUIDs["Q5"];
    auto week     = wikidataUIDs["Q23387"];
    auto chromeOS = wikidataUIDs["Q79531"];
    auto basf    = wikidataUIDs["Q9401"];
    auto lc      = wikidataUIDs["Q131454"];
    auto boa     = wikidataUIDs["Q487907"];
    auto facebook= wikidataUIDs["Q380"];

    assert(op_named_entity.is_named_entity(ai));
    assert(op_named_entity.is_named_entity(nlp));
    assert(op_named_entity.is_named_entity(google));
    assert(op_named_entity.is_named_entity(google2));
    assert(op_named_entity.is_named_entity(deepmind));
    assert(!op_named_entity.is_named_entity(week));
    assert(op_named_entity.is_named_entity(chromeOS));
    assert(op_named_entity.is_named_entity(basf));
    //TODO:Make fails test to success
    assert(op_named_entity.is_named_entity(lc));//partially successed : Library of Congress
//    assert(op_named_entity.is_named_entity(boa));//will fail : Bank of America
//    assert(op_named_entity.is_named_entity(facebook));//will fail : Facebook

}

void representative_repr_of_query(util::json_t const& config_json) {
    std::cerr << "Test: wordrep::test::representative_repr_of_query" << std::endl;
    util::Timer timer;

    wikidata::test::UnittestDataset testset{{config_json}};
    auto& entity_reprs = testset.entity_reprs;
    auto& wikidataUIDs = testset.wikidataUIDs;
    auto& wordUIDs     = testset.wordUIDs;
    auto& tokens       = testset.tokens;
    engine::SubmoduleFactory factory{{config_json}};
    auto word_importance = factory.word_importance();
    auto voca = factory.voca_info();
    tokens.build_voca_index(voca.indexmap);

    Scoring scoring{word_importance, voca.wvecs};

    auto entity_uid = wikidataUIDs["Q2"];
    auto entity = entity_reprs.get_synonyms(entity_uid);
    for(auto words : entity.reprs){
        fmt::print("{} : {}\n", words.repr(wordUIDs), scoring.phrase(words));
    }
    auto repr = scoring.max_score_repr(entity);
    assert(scoring.phrase(repr) == scoring.phrase(entity));
    auto& w = wordUIDs;
    std::vector<WordUID> words = {w["natural"],w["language"],w["processing"]};
    assert(repr == Words{std::move(words)});
    fmt::print("{} : {}\n", repr.repr(wordUIDs), scoring.phrase(repr));
}

void scoring_words(util::json_t const& config_json){
    std::cerr << "Test: wordrep::test::scoring_words"<<std::endl;
    util::Timer timer;

    wikidata::test::UnittestDataset testset{{config_json}};
    auto& wikidataUIDs = testset.wikidataUIDs;
    auto& entity_reprs = testset.entity_reprs;
    auto& wordUIDs     = testset.wordUIDs;
    auto& annotator    = testset.annotator;
    auto& tokens       = testset.tokens;
    auto& op_named_entity = testset.op_named_entity;
    engine::SubmoduleFactory factory{{config_json}};
    auto word_importance = factory.word_importance();
    auto voca = factory.voca_info();
    tokens.build_voca_index(voca.indexmap);

    Scoring scoring{word_importance, voca.wvecs};

    auto tsent1 = annotator.annotate(testset.sents[0]);
    auto tsent2 = annotator.annotate(testset.sents[2]);
    fmt::print("{}\n",tsent1.sent.repr(wordUIDs));
    fmt::print("{}\n",tsent2.sent.repr(wordUIDs));
    for(auto entity1 : tsent1.get_entities()){
        for(auto uid: entity1.uid.candidates) fmt::print("{} ", wikidataUIDs[uid]);
        fmt::print("\n");
        auto idx1 = entity1.words.front();
        DepPair dep_pair1{tsent1.sent, idx1};
        for(auto entity2 : tsent2.get_entities()){
            for(auto uid: entity2.uid.candidates) fmt::print("{} ", wikidataUIDs[uid]);
            fmt::print("\n");
            auto idx2 = entity2.words.front();
            DepPair dep_pair2{tsent2.sent, idx2};

            wordrep::Words words{{dep_pair1.word_dep,dep_pair1.word_gov,dep_pair2.word_dep,dep_pair2.word_gov}};
            fmt::print("{} : {}\n",words.repr(wordUIDs), scoring.similarity(dep_pair1, dep_pair2));
        }
    }
    fmt::print("\n");

    auto& w=wordUIDs;
    Words words{{w["European"],w["Union"]}};
    fmt::print("{} : {}\n", words.repr(wordUIDs), scoring.phrase(words));

    Scoring::Preprocess scoring_preprocessor{scoring, entity_reprs, op_named_entity};

    fmt::print("{}\n",tsent1.sent.repr(wordUIDs));
    auto sent_to_scored1 = scoring_preprocessor.sentence(tsent1);
    for(auto& x : sent_to_scored1.entities){
        for(auto entity : x.candidates) {
            auto idx = x.idxs.dep_token_idx(tokens);
            fmt::print("{}:{} dep:{} gov:{}\t{}\t: {}\n",
                       tokens.word_pos(idx),
                       x.idxs.size(),
                       wordUIDs[tokens.word_uid(idx)],
                       wordUIDs[tokens.head_uid(idx)],
                       scoring.max_score_repr(entity_reprs.get_synonyms(entity.uid)).repr(wordUIDs),
                       entity.score);
        }
    }
    for(auto& w : sent_to_scored1.words){
        wordrep::Words words{{w.word_dep,w.word_gov}};
        fmt::print("{}\t:{}:dep\t{}:gov\n",words.repr(wordUIDs),
                   word_importance.score(w.word_dep),word_importance.score(w.word_gov));
    }
    fmt::print("\n");

    fmt::print("{}\n",tsent2.sent.repr(wordUIDs));
    auto sent_to_scored2 = scoring_preprocessor.sentence(tsent2);
    for(auto& x : sent_to_scored2.entities){
        for(auto entity : x.candidates) {
            auto idx = x.idxs.dep_token_idx(tokens);
            fmt::print("{}:{} dep:{} gov:{}\t{}\t: {}\n",
                       tokens.word_pos(idx),
                       x.idxs.size(),
                       wordUIDs[tokens.word_uid(idx)],
                       wordUIDs[tokens.head_uid(idx)],
                       scoring.max_score_repr(entity_reprs.get_synonyms(entity.uid)).repr(wordUIDs),
                       entity.score);
        }
    }
    for(auto& w : sent_to_scored2.words){
        wordrep::Words words{{w.word_dep,w.word_gov}};
        fmt::print("{}\t:{}:dep\t{}:gov\n",words.repr(wordUIDs),
                   word_importance.score(w.word_dep),word_importance.score(w.word_gov));
    }
    fmt::print("\n");
    for(auto& x : sent_to_scored1.entities){
        for(auto& y : sent_to_scored2.entities){
            fmt::print("{} vs {} : {}\n", x.repr(tokens, wordUIDs), y.repr(tokens, wordUIDs),
                       scoring.similarity(x, y));
        }
    }

    for(auto& x : sent_to_scored2.words){
        for(auto& y : sent_to_scored1.words){
            fmt::print("{} vs {} : {}\n", x.repr(wordUIDs), y.repr(wordUIDs),
                       scoring.similarity(x, y));
        }
    }
    fmt::print("\nQuery=sent1, Data=sent2. Base matches:\n");
    for(auto& x : sent_to_scored1.entities){
        auto m_best_match = scoring.similarity(x,sent_to_scored2);
        if(!m_best_match) continue;
        auto best_match = m_best_match.value();
        auto dep_idx = best_match.data.dep_token_idx(tokens);
        auto word_gov = tokens.head_uid(dep_idx);
        fmt::print("{} vs {}:dep {}:gov : {}\n", x.repr(tokens, wordUIDs),
                   best_match.data.repr(tokens,wordUIDs), wordUIDs[word_gov],
                   best_match.score);
    }
    for(auto& x : sent_to_scored1.words){
        auto m_best_match = scoring.similarity(x,sent_to_scored2);
        if(!m_best_match) continue;
        auto best_match = m_best_match.value();
        auto dep_idx = best_match.data.dep_token_idx(tokens);
        auto word_gov = tokens.head_uid(dep_idx);
        fmt::print("{} vs {}:dep {}:gov : {}\n", x.repr(wordUIDs),
                   best_match.data.repr(tokens,wordUIDs), wordUIDs[word_gov],
                   best_match.score);
    }
    fmt::print("\nQuery=sent2, Data=sent1. Matched results:\n");

    auto op2 = entity_reprs.get_comparison_operator(sent_to_scored2.all_named_entities());
    auto op1 = entity_reprs.get_comparison_operator(sent_to_scored1.all_named_entities());
    timer.here_then_reset("Prepare sentence prefiltering.");
    assert(op2.isin(sent_to_scored1.orig));
    assert(!op1.isin(sent_to_scored2.orig));
    timer.here_then_reset("Check sentences contain all named entities in query.");
    auto op_query_similarity = scoring.op_sentence_similarity(sent_to_scored2);
    auto scored_sent1 = op_query_similarity.score(sent_to_scored1);
    for(auto entity : scored_sent1.entities){
        if(!entity.second) continue;
        auto score = entity.second.value();
        fmt::print("{:<5} : {:<25} - {:<25}\n", score.score, entity.first.repr(tokens, wordUIDs),
                    score.data.repr(tokens,wordUIDs));
    }
    for(auto word : scored_sent1.words){
        if(!word.second) continue;
        auto score = word.second.value();
        fmt::print("{:<5} : {:<25} - {:<25}\n", score.score, word.first.repr(wordUIDs),
                   score.data.repr(tokens,wordUIDs));
    }
};

void test_all(int argc, char** argv){
    assert(argc>2);
    auto config_json = util::load_json(argv[1]);
    acronyms_check(config_json);
    named_entity_check(config_json);
    representative_repr_of_query(config_json);
    scoring_words(config_json);
}

}//namespace wordrep::test
}//namespace wordrep

int main(int argc, char** argv){
    util::Timer timer;

//    wikidata::test::test_all(argc, argv);
//    wordrep::test::test_all(argc,argv);
//    return 0;

    assert(argc>2);
    auto config_json = util::load_json(argv[1]);
    std::string query = util::string::strip(util::string::read_whole(argv[2]));

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
//    wordrep::wiki::EntityReprs entity_reprs{entities.entities};
    timer.here_then_reset("Build data structures.");
    wikidata::GreedyAnnotator annotator{std::move(entities)}; //Move. It took a few seconds, otherwise.
    timer.here_then_reset("Build data structures.");
    auto words = util::string::split(query, " ");
    std::vector<wordrep::WordUID> text = util::map(words, [&wordUIDs](auto x){return wordUIDs[x];});

    auto tags = annotator.annotate(text);
    timer.here_then_reset(fmt::format("Annotate a query of {} words.", words.size()));
    for(auto tag : tags)
        fmt::print("{} {} : {}\n", tag.offset, tag.len, wikidataUIDs[tag.uid]);
//        fmt::print("{} {} : {}\n", tag.offset, tag.len, entity_reprs[tag.uid].repr(wikidataUIDs, wordUIDs));
    return 0;
}
