#include <cassert>

#include <fmt/printf.h>
#include <src/similarity/query_engine.h>

#include "wiki/wikidata.h"
#include "wiki/property_triple.h"

#include "similarity/config.h"
#include "similarity/query_engine.h"

#include "wordrep/word_uid.h"
#include "wordrep/wikientity.h"
#include "wordrep/simiarity_score.h"

#include "utils/profiling.h"
#include "utils/algorithm.h"
#include "utils/string.h"
#include "utils/json.h"

#include "wordrep/flatbuffers/io.h"
//using util::get_str;
//using util::find;
//using util::has_key;


namespace wikidata{
namespace test {

struct UnittestDataset{
    UnittestDataset(engine::Config const& config)
    : factory{config},
      entities{read_wikidata_entities(wordUIDs, "../rnn++/tests/data/wikidata.test.entities")},
      entities_by_uid{entities.to_uid_sorted()},
      annotator{entities},
      p_dict{"../rnn++/tests/data/wikidata.test.properties"},
      wordUIDs{factory.word_uid_index()},
      entity_reprs{entities_by_uid},
      op_acronym{wordUIDs},
      wikidataUIDs{"../rnn++/tests/data/wikidata.test.uid"},
      wiki_ne_UIDs{"../rnn++/tests/data/wikidata.test.uid.named_entities"},
      op_named_entity{wiki_ne_UIDs, wordUIDs, entity_reprs} {
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
    wordrep::wiki::SortedEntities entities;
    wordrep::wiki::UIDSortedEntities entities_by_uid;
    GreedyAnnotator annotator;
    PropertyTable p_dict;
    wordrep::WordUIDindex wordUIDs;
    wordrep::wiki::EntityReprs entity_reprs;
    wordrep::wiki::OpAcronym op_acronym;
    wordrep::WikidataUIDindex wikidataUIDs;
    wordrep::WikidataUIDindex wiki_ne_UIDs;
    wordrep::wiki::OpNamedEntity op_named_entity;
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
    wordrep::wiki::SortedEntities entities{items};
    std::vector<wordrep::WordUID> text = {1, 2, 3, 4, 8, 9, 5, 2, 3, 4, 2, 3, 8, 9, 3, 4, 5, 6, 7};
    auto entities_by_uid = entities.to_uid_sorted();
    wordrep::wiki::EntityReprs entity_reprs{entities_by_uid};
    fmt::print("Entities :\n");
    for (auto &item : entities)
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
    auto entities_by_uid = entities.to_uid_sorted();
    wordrep::wiki::EntityReprs entity_reprs{entities_by_uid};
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
    for(auto entity : testset.entities)
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
    for(auto entity : testset.entities)
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

void property_p31_instance_of(util::json_t const& config_json){
    std::cerr<<"Test: wikidata::test::property_p31_instance_of"<<std::endl;
    util::Timer timer;
//    PropertyTable p_dict{util::get_str(config_json,"wikidata_properties")};
//    timer.here_then_reset("Property table loaded.");

    UnittestDataset testset{{config_json}};
    auto& wikidataUIDs = testset.wikidataUIDs;
    auto& p_dict       = testset.p_dict;

    auto nlp = wikidataUIDs["Q2"];
    auto facebook = wikidataUIDs["Q380"];
    auto company = wikidataUIDs["Q4830453"];

    auto op_company = p_dict.get_op_instance_of(company);
    assert(op_company.is_instance(facebook));
    assert(!op_company.is_instance(nlp));
    //Test for null-data safety
    auto op_facebook = p_dict.get_op_instance_of(facebook);
    assert(!op_facebook.is_instance(facebook));
    assert(!op_facebook.is_instance(company));
}

void ambiguous_entity_equality(){
    std::cerr << "Test: wikidata::test::ambiguous_entity_equality"<<std::endl;
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

void block_binary_search(){
    std::cerr << "Test: wikidata::test::block_binary_search"<<std::endl;
    using wordrep::WikidataUID;
    using wordrep::WordUID;
    std::vector<wordrep::wiki::Entity> items =
            {{1,   {1, 2}},
             {1,   {1, 2}},
             {2,   {1, 3}},
             {2,   {1, 2, 3}},
             {3,   {1, 2, 3}},
             {3,   {1, 2, 3}},
             {4,   {2, 3, 4}},
             {4,   {5}},
             {5,   {5}},
             {5,   {5}},
             {6,   {6, 7}},
             {7,   {2, 3}},
             {8,   {5, 6, 8}},
             {8,   {9}},
             {10,  {9,10}}};
    std::sort(items.begin(), items.end(), [](auto x, auto y){return x.uid<y.uid;});

    auto find = [&items](wordrep::WikidataUID uid){
        auto less = [uid](auto& rhs){return uid <rhs.uid;};
        auto eq   = [uid](auto& rhs){return uid==rhs.uid;};
        auto beg = items.cbegin();
        auto end = items.cend();
        auto mit= util::binary_find_block(beg, end, eq, less);
        if(!mit) assert(0);
        return mit.value();
    };
    auto diff = [](auto pair){return pair.second-pair.first;};
    assert(diff(find(1))==2);
    assert(diff(find(2))==2);
    assert(diff(find(3))==2);
    assert(diff(find(4))==2);
    assert(diff(find(5))==2);
    assert(diff(find(6))==1);
    assert(diff(find(7))==1);
    assert(diff(find(8))==2);
    assert(diff(find(10))==1);
    wordrep::wiki::UIDSortedEntities entities{items};
    wordrep::wiki::EntityReprs entity_reprs{entities};
    auto op=entity_reprs.get_comparison_operator(1);
    assert(op.exact_match({1,2}));
}

void test_all(int argc, char** argv) {
    assert(argc>2);
    auto config_json = util::load_json(argv[1]);
    std::string query = util::string::read_whole(argv[2]);


    integer_list_ordering();
    greedy_matching();
    block_binary_search();
//    uid_lookup_benchmark();
    compare_wordUIDs_and_WikidataUID(config_json, query);
    annotate_sentence(config_json);
    operation_wikiuid_on_sentence(config_json);
    operation_ambiguous_entity_on_sentence(config_json);
    property_p31_instance_of(config_json);
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

    auto op_scorer = scoring.op_similarity();
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
            fmt::print("{} : {}\n",words.repr(wordUIDs), op_scorer.similarity(dep_pair1, dep_pair2));
        }
    }
    fmt::print("\n");

    auto& w=wordUIDs;
    Words words{{w["European"],w["Union"]}};
    fmt::print("{} : {}\n", words.repr(wordUIDs), scoring.phrase(words));

    Scoring::Preprocess scoring_preprocessor{scoring, entity_reprs};

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
                       op_scorer.similarity(x, y));
        }
    }

    for(auto& x : sent_to_scored2.words){
        for(auto& y : sent_to_scored1.words){
            fmt::print("{} vs {} : {}\n", x.repr(wordUIDs), y.repr(wordUIDs),
                       op_scorer.similarity(x, y));
        }
    }
    fmt::print("\nQuery=sent1, Data=sent2. Base matches:\n");
    for(auto& x : sent_to_scored1.entities){
        auto m_best_match = op_scorer.similarity(x,sent_to_scored2);
        if(!m_best_match) continue;
        auto best_match = m_best_match.value();
        auto dep_idx = best_match.data.dep_token_idx(tokens);
        auto word_gov = tokens.head_uid(dep_idx);
        fmt::print("{} vs {}:dep {}:gov : {}\n", x.repr(tokens, wordUIDs),
                   best_match.data.repr(tokens,wordUIDs), wordUIDs[word_gov],
                   best_match.score);
    }
    for(auto& x : sent_to_scored1.words){
        auto m_best_match = op_scorer.similarity(x,sent_to_scored2);
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
    auto m_scored_sent1 = op_query_similarity.score(sent_to_scored1);
    assert(m_scored_sent1);
    auto scored_sent1 = m_scored_sent1.value();
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

void convert_voca_info(int argc, char** argv){
    assert(argc>1);
    namespace fb = util::io::fb;
    auto config_json = util::load_json(argv[1]);
    engine::SubmoduleFactory factory{{config_json}};
    auto conf = [&factory](auto x){return factory.config.value(x);};
    util::Timer timer;

    util::io::H5file file{conf("wordvec_store"), util::io::hdf5::FileMode::read_exist};
    assert(conf("w2v_float_t")=="float32");
    auto wvecs = file.getRawData<float>(conf("w2vmodel_name"));
    timer.here_then_reset("Load word vectors.");
    auto voca_index_wuids =wordrep::load_voca(conf("wordvec_store"), conf("voca_name"));
    timer.here_then_reset("Load voca indexes.");
    std::vector<int64_t> voca_idxs = util::serialize(voca_index_wuids);
    timer.here_then_reset("Serialize WordUIDs.");
    fb::to_file(voca_idxs, {"news.en.uids.bin"});
    fb::to_file(wvecs, {"news.en.vecs.bin"});
    timer.here_then_reset("Write to binary files.");
}

void load_voca_info(int argc, char** argv){
    assert(argc>1);
    namespace fb = util::io::fb;

    util::Timer timer;

    std::vector<wordrep::WordUID> vidx_wuids;
    std::vector<float> wvecs_raw;

    fb::deserialize_f32vector(fb::load_binary_file("news.en.vecs.bin"), wvecs_raw);
    timer.here_then_reset("Load word vector matrix file.");
    wvecs_raw.resize(1000);
    wordrep::WordBlock_base<float,100> wvecs{std::move(wvecs_raw)};
    timer.here_then_reset("Construct word vector matrix.");
    fb::deserialize_i64vector(fb::load_binary_file("news.en.uids.bin"), vidx_wuids);
    timer.here_then_reset("Load voca index map file.");
    wordrep::VocaIndexMap vmap{vidx_wuids};
    timer.here_then_reset("Construct voca index map.");
    wordrep::VocaInfo voca_info{std::move(vidx_wuids), std::move(wvecs)};
    timer.here_then_reset("Construct a voca info object.");
}

template<typename T>
void load_binary_file(std::string filename, T& vec){
    namespace fb = util::io::fb;
    fb::deserialize_i64vector(fb::load_binary_file(filename), vec);
};

//TODO: remove temporal changes on DepParsedTokens and EntityModule.
void load_query_engine(int argc, char** argv) {
    assert(argc>1);
    namespace fb = util::io::fb;
    auto config_json = util::load_json(argv[1]);
    engine::SubmoduleFactory factory{{config_json}};
    auto conf = [&factory](auto x){return factory.config.value(x);};

    using namespace wordrep;
    util::Timer timer;

    std::vector<WordUID> vidx_wuids;
    std::vector<float> wvecs_raw;

    wordrep::DepParsedTokens texts{};

    wordrep::UIDIndexBinary word_uids{conf("word_uid_bin")};
    wordrep::UIDIndexBinary pos_uids{conf("pos_uid_bin")};
    wordrep::wiki::SortedEntities::Binary wikidata_entities{conf("wikidata_entities_by_name")};
    wordrep::wiki::UIDSortedEntities::Binary wikidata_entities_by_uid{conf("wikidata_entities_by_uid")};
    util::io::fb::PairsBinary wikidata_properties{conf("wikidata_properties")};
    util::io::fb::PairsBinary wikidata_instances{conf("wikidata_instances")};
    wordrep::UIDIndexBinary named_entity_wikidata_uids{conf("named_entity_uids")};
    wordrep::UIDIndexBinary wikidata_uids{conf("wikidata_uids")};
    wikidata::EntityModule f{};

    tbb::task_group g;

    g.run([&wvecs_raw](){fb::deserialize_f32vector(fb::load_binary_file("news.en.vecs.bin"), wvecs_raw);});
    g.run([&vidx_wuids](){fb::deserialize_i64vector(fb::load_binary_file("news.en.uids.bin"), vidx_wuids);});

    g.run([&texts](){load_binary_file("nyt.sents_uid.i64v",  texts.sents_uid);});
    g.run([&texts](){load_binary_file("nyt.chunks_idx.i64v", texts.chunks_idx);});
    g.run([&texts](){load_binary_file("nyt.sents_idx.i64v",  texts.sents_idx);});
    g.run([&texts](){load_binary_file("nyt.words.i64v",      texts.words);});
    g.run([&texts](){load_binary_file("nyt.words_uid.i64v",  texts.words_uid);});
    g.run([&texts](){load_binary_file("nyt.words_pidx.i64v", texts.words_pidx);});
    g.run([&texts](){load_binary_file("nyt.head_words.i64v", texts.head_words);});
    g.run([&texts](){load_binary_file("nyt.heads_uid.i64v",  texts.heads_uid);});
    g.run([&texts](){load_binary_file("nyt.heads_pidx.i64v", texts.heads_pidx);});
    g.run([&texts](){load_binary_file("nyt.words_beg.i64v",  texts.words_beg);});
    g.run([&texts](){load_binary_file("nyt.words_end.i64v",  texts.words_end);});
    g.run([&texts](){load_binary_file("nyt.poss.i64v",       texts.poss);});
    g.run([&texts](){load_binary_file("nyt.arclabels.i64v",  texts.arclabels);});

    g.run([&f,&word_uids](){f.wordUIDs = std::make_unique<wordrep::WordUIDindex>(word_uids);});
    g.run([&f,&pos_uids](){f.posUIDs = std::make_unique<wordrep::POSUIDindex>(pos_uids);});
    g.run([&f,&wikidata_uids](){f.wikiUIDs = std::make_unique<wordrep::WikidataUIDindex>(wikidata_uids);});
    g.run([&f,&named_entity_wikidata_uids](){f.wiki_ne_UIDs = std::make_unique<wordrep::WikidataUIDindex>(named_entity_wikidata_uids);});
    g.run([&f,&wikidata_properties,&wikidata_instances](){
        using util::io::fb::deserialize_pairs;
        using util::io::fb::load_binary_file;
        auto properties = deserialize_pairs<wikidata::PropertyOfEntity>(load_binary_file(wikidata_properties));
        auto instances  = deserialize_pairs<wikidata::EntityOfProperty>(load_binary_file(wikidata_instances));
        f.prop_dict = std::make_unique<wikidata::PropertyTable>(std::move(properties),std::move(instances));
    });
    g.run([&f,&wikidata_entities](){f.entities = std::make_unique<wordrep::wiki::SortedEntities>(wikidata_entities);});
    g.run([&f,&wikidata_entities_by_uid](){
        f.entities_by_uid  = std::make_unique<wordrep::wiki::UIDSortedEntities>(wordrep::wiki::read_binary_file(wikidata_entities_by_uid));
    });

    g.wait();
    timer.here_then_reset("Concurrent loading of binary files");
    auto sents = texts.IndexSentences();
    timer.here_then_reset("Post processing of indexed texts.");

    f.greedy_annotator = std::make_unique<wikidata::GreedyAnnotator>(*f.entities);
    f.entity_reprs     = std::make_unique<wordrep::wiki::EntityReprs>(*f.entities_by_uid);
    f.op_named_entity  = std::make_unique<wordrep::wiki::OpNamedEntity>(*f.wiki_ne_UIDs, *f.wordUIDs, *f.entity_reprs);

    wordrep::WordBlock_base<float,100> wvecs{std::move(wvecs_raw)};
    wordrep::VocaInfo voca_info{std::move(vidx_wuids), std::move(wvecs)};
    timer.here_then_reset("Complete to load data structures.");


}

struct SerializedAnnotation{
    struct Binary{
        std::string name;
    };
    std::vector<wordrep::io::EntityCandidate> candidates;
    std::vector<wordrep::io::TaggedToken> tagged_tokens;

    void to_file(Binary file) const {
        namespace fb = wordrep::io;

        flatbuffers::FlatBufferBuilder builder;
        auto candidates_serialized = builder.CreateVectorOfStructs(candidates);
        auto tokens_serialized = builder.CreateVectorOfStructs(tagged_tokens);
        auto entities = fb::CreateTaggedSentences(builder, candidates_serialized, tokens_serialized);
        builder.Finish(entities);

        auto *buf = builder.GetBufferPointer();
        auto size = builder.GetSize();
        std::ofstream outfile(file.name, std::ios::binary);
        outfile.write(reinterpret_cast<const char *>(&size), sizeof(size));
        outfile.write(reinterpret_cast<const char *>(buf), size);
    }
};

std::unique_ptr<SerializedAnnotation> load_binary_file(SerializedAnnotation::Binary file){
    auto data = util::io::fb::load_binary_file(file.name);
    auto block = std::make_unique<SerializedAnnotation>();
    auto rbuf = wordrep::io::GetTaggedSentences(data.get());
    block->candidates.reserve(rbuf->candidates()->size());
    block->tagged_tokens.reserve(rbuf->tagged_tokens()->size());

    for(auto v : *rbuf->candidates())
        block->candidates.push_back(*v);
    for(auto v : *rbuf->tagged_tokens())
        block->tagged_tokens.push_back(*v);
    return block;
}

void annotate_sentences(int argc, char** argv){
    assert(argc>1);
    auto config_json = util::load_json(argv[1]);
    engine::SubmoduleFactory factory{{config_json}};

    util::Timer timer;
    auto wiki = factory.wikientity_module();
    timer.here_then_reset("Load wikidata::EntityModule.");
    auto word_importance = factory.word_importance();
    timer.here_then_reset("Load word_importance.");
    auto wordUIDs = factory.word_uid_index();
    timer.here_then_reset("Load wordUIDs.");
    auto voca = factory.voca_info();
    timer.here_then_reset("Load voca_info.");
    wordrep::Scoring scoring{word_importance, voca.wvecs};
    timer.here_then_reset("Load wordrep::Scoring.");
    wordrep::Scoring::Preprocess scoring_preprocessor{scoring, wiki.entity_repr()};
    timer.here_then_reset("Load wordrep::Scoring::Preprocess.");

    timer.here_then_reset("Load all data.");

    auto tokens = factory.dep_parsed_tokens();
    std::vector<wordrep::Sentence> orig_sents = tokens.IndexSentences();
    timer.here_then_reset("Load texts.");
    tbb::concurrent_vector<wordrep::Scoring::SentenceToScored> sents;
    auto n = orig_sents.size();
//    if(n>100) n=10000;
    tbb::parallel_for(decltype(n){0}, n, [&wiki,&scoring,&scoring_preprocessor,&orig_sents,&sents](auto i) {
        auto& sent = orig_sents[i];
        auto tagged_sent = wiki.annotator().annotate(sent);
        auto sent_to_scored = scoring_preprocessor.sentence(tagged_sent);
        sent_to_scored.filter_false_named_entity(wiki.get_op_named_entity(), wiki.pos_uid());

        for(auto& e : sent_to_scored.entities){
            std::vector<wordrep::WikidataUID> instances;
            for(auto uid : e.uid.candidates)
                util::append(instances, wiki.properties().get_p31_properties(uid));
            for (auto uid : instances) {
                //TODO: don't know why m_synonyms can be empty.
                auto m_synonyms = wiki.entity_repr().find(uid);
                if(!m_synonyms) continue;
                auto synonyms = m_synonyms.value();
                auto repr = scoring.max_score_repr(synonyms);
                e.candidates.push_back({uid, scoring.phrase(repr)});
                e.uid.candidates.push_back(uid);
            }
        }
        sents.push_back(sent_to_scored);
    });

    timer.here_then_reset(fmt::format("Annotated {} sentences.", sents.size()));

    constexpr int n_output = 4;
    SerializedAnnotation blocks[n_output];
    for(auto& sent : sents){
        auto sent_uid = sent.orig.uid.val;
        auto output_hash = sent_uid % n_output;
        auto& candidates = blocks[output_hash].candidates;
        auto& tagged_entities = blocks[output_hash].tagged_tokens;
        for(auto& entity : sent.entities){
            tagged_entities.push_back({sent.orig.uid.val, entity.idxs.idx.val, entity.idxs.len});
            for(auto& candidate : entity.candidates)
                candidates.push_back({entity.idxs.idx.val, candidate.uid.val, candidate.score});
        }
        for(auto& dep_pair : sent.words)
            tagged_entities.push_back({sent.orig.uid.val, dep_pair.idx.val, 0});
    }
    timer.here_then_reset("Serialize annotated sentences.");

    int i=0;
    for(auto const& block : blocks){
        auto filename = fmt::format("nyt.sents.annotated.bin.{}",i++);
        block.to_file({filename});
    }
    timer.here_then_reset("Write to binary files.");
}

void load_annotated_sentences(int argc, char** argv){
    util::Timer timer;
    auto block = load_binary_file(SerializedAnnotation::Binary{"nyt.sents.annotated.bin.0"});
    timer.here_then_reset("Load files.");
    int i=0;
    tbb::concurrent_vector<wordrep::Scoring::AmbiguousEntity> a;
    tbb::concurrent_vector<wordrep::DepPair> b;
    for(auto& entity : block->tagged_tokens){
        auto suid = entity.sent_uid();
        auto idx =entity.token_idx();
        auto len = entity.token_len();
        fmt::print("{} {}\n", idx, len);
        if(++i>50)break;
    }
    for(auto& candidate : block->candidates){
        candidate.token_idx();
        candidate.wiki_uid();
        candidate.score();
    }
}

using wordrep::UIDIndexBinary;
void save_wikidata_entities(int argc, char** argv){
    assert(argc>1);
    auto config_json = util::load_json(argv[1]);
    engine::Config config{config_json};

    util::Timer timer;
    std::string word_uids     = config.value("word_uids_dump");
    std::string pos_uids      = config.value("pid_uids_dump");
    std::string wikidata_uids = config.value("wikidata_uids");
    std::string wikidata_entities          = config.value("wikidata_entities");
    std::string named_entity_wikidata_uids = config.value("named_entity_uids");
    std::string wikidata_properties        = config.value("wikidata_properties");

    wordrep::WordUIDindex wordUIDs{word_uids};
    wordrep::POSUIDindex posUIDs{pos_uids};
    wordrep::WikidataUIDindex wikiUIDs{wikidata_uids};
    wordrep::WikidataUIDindex wiki_ne_UIDs{named_entity_wikidata_uids};
    timer.here_then_reset("Load UID indexes.");
    wikidata::PropertyTable prop_dict{wikidata_properties};
    timer.here_then_reset("Load PropertyTable.");
    auto entities = wikidata::read_wikidata_entities(wordUIDs, wikidata_entities);
    auto entities_by_uid = entities.to_uid_sorted();
    timer.here_then_reset("Load wikidata entities.");
    entities.to_file({"wikidata.entities.by_name.bin"});
    entities_by_uid.to_file({"wikidata.entities.by_uid.bin"});
    timer.here_then_reset("Save to binary file : wikidata.entities.bin");
    wordUIDs.to_file(UIDIndexBinary{"words.uid.bin"});
    timer.here_then_reset("Save to binary file : words.uid.bin");
    wikiUIDs.to_file(UIDIndexBinary{"wikidata.uid.bin"});
    timer.here_then_reset("Save to binary file : wikidata.uid.bin");
}

void test_property_table(){
    using util::io::fb::PairsBinary;
    using util::io::fb::deserialize_pairs;
    using util::io::fb::load_binary_file;

    PairsBinary properties_file{"wikidata.P31.e2p.bin"};
    PairsBinary instances_file{"wikidata.P31.p2e.bin"};
    util::Timer timer;
    auto wikiUIDs = std::make_unique<wordrep::WikidataUIDindex>(UIDIndexBinary{"wikidata.uid.bin"});
    timer.here_then_reset("Load WikiUIDs.");


    auto data = util::io::fb::load_binary_file(properties_file);
    timer.here_then_reset(fmt::format("Read file. {}", properties_file.name));
    auto properties = deserialize_pairs<wikidata::PropertyOfEntity>(std::move(data));
    timer.here_then_reset(fmt::format("Construct {} property values.", properties->size()));
    auto instances  = deserialize_pairs<wikidata::EntityOfProperty>(load_binary_file(instances_file));
    timer.here_then_reset(fmt::format("Construct {} instance values.", instances->size()));
    wikidata::PropertyTable table{std::move(properties), std::move(instances)};
    timer.here_then_reset("Construct PropertyTable.");

    auto entity = wikiUIDs->uid("Q419");
    for(auto uid : table.get_p31_properties(entity))
        fmt::print("{} : {}\n", wikiUIDs->str(entity), wikiUIDs->str(uid));

    entity = wikiUIDs->uid("Q95");
    for(auto uid : table.get_p31_properties(entity))
        fmt::print("{} : {}\n", wikiUIDs->str(entity), wikiUIDs->str(uid));

    assert(table.get_p31_properties(wikiUIDs->uid("Q09832broken333333333")).empty());
    assert(!table.get_p31_properties(wikiUIDs->uid("Q419")).empty());
    {
        util::Timer timer;
        wikidata::PropertyTable table{"/home/jihuni/word2vec/rss/wikidata.properties"};
        timer.here_then_reset("Load from text file.");

        auto entity = wikiUIDs->uid("Q419");
        for(auto uid : table.get_p31_properties(entity))
            fmt::print("{} : {}\n", wikiUIDs->str(entity), wikiUIDs->str(uid));

        entity = wikiUIDs->uid("Q95");
        for(auto uid : table.get_p31_properties(entity))
            fmt::print("{} : {}\n", wikiUIDs->str(entity), wikiUIDs->str(uid));
    }
}

namespace util{
namespace io{
namespace fb{
namespace test{

void ordered_pair(){
    std::cerr<< "util::io::fb::test::ordered_pair" <<std::endl;
    std::vector<Pair> vals = {{1,1},{0,2},{1,3},{4,2},{5,2},{3,4},{2,4},{2,1},{4,5},{1,3}};
    std::sort(vals.begin(), vals.end());
    auto beg = vals.begin();

    auto b = util::binary_find_block(vals, Pair{1,-1}).value();
    assert(b.first==beg+1);
    assert(b.second==beg+4);

    auto b2 = util::binary_find_block(vals, Pair{2,-1}).value();
    assert(b2.first==beg+4);
    assert(b2.second==beg+6);
}

void test_all(){
    ordered_pair();
}

}//namespace util::io::fb::test
}//namespace util::io::fb
}//namespace util::io
}//namespace util


void proptext_to_binary_file(){
    namespace fb = util::io::fb;

    util::Timer timer;
    std::string wikidata_properties        = "/home/jihuni/word2vec/rss/wikidata.properties";
    std::ifstream is{wikidata_properties};
    tbb::task_group g;
    tbb::concurrent_vector<wikidata::PropertyOfEntity> items;
    while (auto buffer=util::string::read_chunk(is, 2000000)) {
        auto& chars =  buffer.value();
        g.run([&items,chars{std::move(chars)}](){
            std::stringstream ss;
            ss.str(chars.data());
            auto lines = util::string::readlines(std::move(ss));
            for(auto& line : lines){
                wikidata::PropertiesTriple ps{line};
                auto p31 = wordrep::WikidataUIDindex::get_uid("P31");
                if(ps.property_type != p31) continue;
                for(auto p : ps.properties)
                    items.push_back({ps.entity, p});
            }
        });
    }
    g.wait();
    timer.here_then_reset(fmt::format("Load file : {} items.", items.size()));

    std::vector<fb::Pair> entity2property;
    std::vector<fb::Pair> property2entity;
    entity2property.reserve(items.size());
    property2entity.reserve(items.size());
    for(auto item : items) entity2property.push_back({item.entity.val, item.property.val});
    for(auto item : items) property2entity.push_back({item.property.val, item.entity.val});
    timer.here_then_reset("Copy data.");
    tbb::parallel_sort(entity2property.begin(),entity2property.end());
    tbb::parallel_sort(property2entity.begin(),property2entity.end());
    timer.here_then_reset("Sort data.");
    util::io::fb::to_file(entity2property, {"wikidata.P31.e2p.bin"});
    util::io::fb::to_file(property2entity, {"wikidata.P31.p2e.bin"});
    timer.here_then_reset("Write files.");
}

int main(int argc, char** argv){
    util::Timer timer;
//    save_wikidata_entities(argc,argv);
//    proptext_to_binary_file();
    //convert_voca_info(argc,argv);
//    load_voca_info(argc,argv);
//    load_query_engine(argc,argv);
//    annotate_sentences(argc,argv);
    load_annotated_sentences(argc,argv);
    return 0;

//    test_property_table();
//    util::io::fb::test::test_all();
//    wikidata::test::test_all(argc, argv);
//    wordrep::test::test_all(argc,argv);
    return 0;

    assert(argc>2);
    auto config_json = util::load_json(argv[1]);
    std::string query = util::string::strip(util::string::read_whole(argv[2]));

    engine::Config config{config_json};
    engine::SubmoduleFactory factory{config};
    auto wiki = factory.wikientity_module();
    timer.here_then_reset("Load wikidata module.");
    auto words = util::string::split(query, " ");
    std::vector<wordrep::WordUID> text = util::map(words, [&wiki](auto x){return wiki.word_uid()[x];});

    auto tags = wiki.annotator().annotate(text);
    timer.here_then_reset(fmt::format("Annotate a query of {} words.", words.size()));
    for(auto tag : tags)
        fmt::print("{} {} : {}\n", tag.offset, tag.len, wiki.entity_repr()[tag.uid].repr(wiki.wiki_uid(), wiki.word_uid()));
    return 0;
}
