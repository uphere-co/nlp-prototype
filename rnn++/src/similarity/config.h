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

#include "wordrep/simiarity_score.h"
#include "wiki/wikidata.h"

#include "utils/json.h"
#include "utils/optional.h"

namespace engine{

template<typename T>
struct ConfigKeys{
    std::vector<T> keys={{"engine_type"},
                         {"corenlp_client_script"},
                         {"words_list"},
                         {"word_uid_bin"},
//                         {"pos_uids_dump"},
                         {"pos_uid_bin"},
                         {"arclabel_uids_dump"},
                         {"column_uids_dump"},
                         {"word_prob_dump"},
                         {"corenlp_dumps"},
                         {"dep_parsed_bins"},
                         {"dep_parsed_store"}, //TODO : DBIndexer use this. Binarize this
                         {"dep_parsed_prefix"},//TODO : DBIndexer use this. Binarize this
                         {"annotated_tokens"},
                         {"annotated_tokens_n_block"},
                         {"voca_bin"},
                         {"w2vmodel_bin"},
                         {"wikidata_entities_by_name"},
                         {"wikidata_entities_by_uid"},
                         {"wikidata_entities"},
                         {"wikidata_uids"},
                         {"wikidata_properties"},
                         {"wikidata_instances"},
                         {"named_entity_uids"}};
};

using Config = util::ConfigT<ConfigKeys>;

wikidata::EntityModule::InputParam get_wikimoudle_param(Config const &config);

struct SubmoduleFactory{
    SubmoduleFactory(Config const& config, std::optional<int> data_minor_version={});

    data::CoreNLPwebclient corenlp_webclient() const;
    wordrep::WordUIDindex word_uid_index() const;
    wordrep::POSUIDindex pos_uid_index() const;
    wordrep::ArcLabelUIDindex arclabel_uid_index() const;
    wordrep::DepParsedTokens dep_parsed_tokens() const;
    wordrep::WordImportance word_importance() const;
    wordrep::VocaInfo voca_info() const;
    wordrep::WordCaseCorrector word_case_corrector(wordrep::WordImportance const& importance) const;
    wordrep::AnnotationFile load_annotation() const;

    wordrep::WikidataUIDindex wikientity_uid_index() const;
    wikidata::EntityModule wikientity_module() const;

    Dataset empty_dataset() const;
    Dataset load_dataset() const;
    data::DBIndexer db_indexer() const;

    Config config;
    wordrep::ConfigParams conf;
    std::optional<int> data_minor_version;
};




}//namespace engine
