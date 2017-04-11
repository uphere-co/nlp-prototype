#pragma once
#include "wordrep/word_uid.h"
#include "wordrep/wikientity.h"
#include "wordrep/simiarity_score.h"
#include "wordrep/serialized_annotation.h"
#include "wordrep/preprocessed_sentences.h"

#include "wiki/wikidata.h"
#include "wiki/property_triple.h"

namespace wikidata {
namespace test {

struct UnittestDataset {
    UnittestDataset(util::json_t const &config)
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
        std::vector <std::string> jsons = {"../rnn++/tests/data/sentence.0.corenlp",
                                           "../rnn++/tests/data/sentence.1.corenlp",
                                           "../rnn++/tests/data/sentence.2.corenlp",
                                           "../rnn++/tests/data/sentence.3.corenlp",
                                           "../rnn++/tests/data/sentence.4.corenlp"};
        for (auto &json : jsons)
            tokens.append_corenlp_output(data::CoreNLPjson{json});
        tokens.build_sent_uid(0);
        sents = tokens.IndexSentences();
    }

    engine::SubmoduleFactory         factory;
    wordrep::wiki::SortedEntities    entities;
    wordrep::wiki::UIDSortedEntities entities_by_uid;
    GreedyAnnotator annotator;
    PropertyTable   p_dict;
    wordrep::WordUIDindex        wordUIDs;
    wordrep::wiki::EntityReprs   entity_reprs;
    wordrep::wiki::OpAcronym     op_acronym;
    wordrep::WikidataUIDindex    wikidataUIDs;
    wordrep::WikidataUIDindex    wiki_ne_UIDs;
    wordrep::wiki::OpNamedEntity op_named_entity;
    wordrep::DepParsedTokens     tokens{};
    std::vector<wordrep::Sentence> sents{};

};

}//namespace wikidata::test
}//namespace wikidata