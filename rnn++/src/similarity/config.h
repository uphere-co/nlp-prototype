#pragma once

#include <map>
#include <string>

#include "data_source/corenlp_helper.h"
#include "data_source/db.h"

#include "similarity/dataset.h"

#include "wordrep/config.h"
#include "wordrep/dep_parsed.h"
#include "wordrep/word_uid.h"
#include "wordrep/word_prob.h"
#include "wordrep/voca_info.h"
#include "wordrep/word_case_corrector.h"
#include "wordrep/serialized_annotation.h"
#include "wordrep/similar_words.h"

#include "wordrep/simiarity_score.h"
#include "wiki/wikidata.h"

#include "utils/json.h"
#include "utils/optional.h"

namespace engine{

wikidata::EntityModule::InputParam get_wikimoudle_param(wordrep::ConfigParams const &config);

struct SubmoduleFactory{
    SubmoduleFactory(util::json_t const& config_, std::optional<int> data_minor_version={});

    data::CoreNLPwebclient corenlp_webclient() const;
    wordrep::WordUIDindex word_uid_index() const;
    wordrep::POSUIDindex pos_uid_index() const;
    wordrep::ArcLabelUIDindex arclabel_uid_index() const;
    wordrep::DepParsedTokens dep_parsed_tokens() const;
    wordrep::WordImportance word_importance() const;
    wordrep::VocaInfo voca_info() const;
    wordrep::SimilarWords similar_words() const;
    wordrep::WordCaseCorrector word_case_corrector(wordrep::WordImportance const& importance) const;
    wordrep::AnnotationData load_annotation() const;

    wordrep::WikidataUIDindex wikientity_uid_index() const;
    wikidata::EntityModule wikientity_module() const;


    Dataset empty_dataset() const;
    Dataset load_dataset() const;
    data::DBIndexer db_indexer() const;

    wordrep::ConfigParams conf;
    std::optional<int> data_minor_version;
};




}//namespace engine
