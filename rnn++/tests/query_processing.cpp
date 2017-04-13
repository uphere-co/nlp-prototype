#include <fmt/printf.h>

#include "similarity/config.h"

#include "wordrep/preprocessed_sentences.h"

#include "tests/wiki/test_dataset.h"

#include "utils/flatbuffers/io.h"
#include "utils/profiling.h"
#include "utils/parallel_algorithm.h"

namespace engine {
namespace test{

struct LookupEntityCandidateIndexDummy{};

struct LookupEntityCandidate{
    using Index = util::IntegerLike<LookupEntityCandidateIndexDummy>;
    struct Range{
        struct Iterator{
            Iterator(Index idx) : idx{idx} {}
            Index operator*( void ) const {return idx;}
            void operator++(void) {++idx;}
            bool operator==(Iterator rhs) const {return idx == rhs.idx;}
            bool operator!=(Iterator rhs) const {return idx != rhs.idx;}
        private:
            Index idx;
        };
        Range(Index beg, Index end) : beg_{beg},end_{end} {}
        auto begin() const { return Iterator{beg_};}
        auto end() const { return Iterator{end_};}
        size_t size() const {return end_.val - beg_.val;}
    private:
        Index beg_;
        Index end_;
    };

    static LookupEntityCandidate factory(wordrep::AnnotationData const& tokens){
        util::Timer timer;
        LookupEntityCandidate candidates;

        for(auto const& block : tokens.blocks){
            util::append(*candidates.tokens, block->candidates);
        }
        timer.here_then_reset("Aggregate tokens.");

        tbb::parallel_sort(candidates.tokens->begin(),
                           candidates.tokens->end(),
                           [](auto const& x, auto const& y) {return x.wiki_uid() < y.wiki_uid();});
        timer.here_then_reset("Sort tokens by WikiUID.");

        return candidates;
    }

    Range find(wordrep::WikidataUID uid) const{
        auto eq   = [uid](auto x){return uid.val==x.wiki_uid();};
        auto less = [uid](auto x){return uid.val<x.wiki_uid();};

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
            : tokens{std::make_unique<tbb::concurrent_vector<wordrep::io::EntityCandidate>>()}
    {}
    std::unique_ptr<tbb::concurrent_vector<wordrep::io::EntityCandidate>> tokens;
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
    wordrep::AnnotationData annotated_tokens;

    auto load_word_uids =[&wordUIDs,&factory](){
        wordUIDs = std::make_unique<wordrep::WordUIDindex>(factory.conf.word_uid);
    };
    auto load_word_scores =[&word_importance,&factory](){
        word_importance = std::make_unique<wordrep::WordImportance>(factory.word_importance());
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
                          load_word_scores,
//                          load_wiki_module,
                          load_word_embedding,
                          load_indexed_text);

    timer.here_then_reset("Concurrent loading of binary files");
    auto sents = texts->IndexSentences();
//    auto data_sent = wordrep::PreprocessedSentences::factory(sents, annotated_tokens);
    wordrep::Scoring scoring{*word_importance, voca->wvecs};
    wordrep::Scoring::Preprocess scoring_preprocessor{scoring, testset.entity_reprs};
    timer.here_then_reset("Post processing of indexed texts.");

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

    auto keys_per_ambiguous_entity = util::map(named_entities, [&](auto& e){
        auto ranges = candidates.find(e);
        return util::concat_map(ranges, [&](auto i){return texts->sent_uid(candidates.token_index(i));});
    });
    timer.here_then_reset("Map phase for Wiki entities.");
    auto matched_sents  = util::intersection(keys_per_ambiguous_entity);
    timer.here_then_reset("Reduce phase for Wiki entities.");

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
}

}//namespace engine::test
}//namespace engine
