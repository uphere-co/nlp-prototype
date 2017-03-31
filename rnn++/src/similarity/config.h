#pragma once

#include <map>
#include <string>

#include "data_source/corenlp_helper.h"
#include "data_source/db.h"

#include "similarity/dataset.h"

#include "wordrep/dep_parsed.h"
#include "wordrep/word_uid.h"
#include "wordrep/word_prob.h"
#include "wordrep/voca_info.h"
#include "wordrep/word_case_corrector.h"

#include "wordrep/simiarity_score.h"
#include "wiki/wikidata.h"

#include "utils/json.h"

namespace engine{

template<typename T>
struct ConfigKeys{
    std::vector<T> keys={{"engine_type"},
                         {"corenlp_client_script"},
                         {"word_uids_dump"},
                         {"word_uid_bin"},
                         {"pos_uids_dump"},
                         {"pos_uid_bin"},
                         {"arclabel_uids_dump"},
                         {"column_uids_dump"},
                         {"word_prob_dump"},
                         {"corenlp_dumps"},
                         {"dep_parsed_store"},
                         {"dep_parsed_prefix"},
                         {"wordvec_store"},
                         {"voca_name"},
                         {"w2vmodel_name"},
                         {"w2v_float_t"},
                         {"wikidata_entities_by_name"},
                         {"wikidata_entities_by_uid"},
                         {"wikidata_entities"},
                         {"wikidata_uids"},
                         {"wikidata_properties"},
                         {"wikidata_instances"},
                         {"named_entity_uids"}};
};

using Config = util::ConfigT<ConfigKeys>;

struct SubmoduleFactory{
    SubmoduleFactory(Config const& config);

    data::CoreNLPwebclient corenlp_webclient() const;
    wordrep::WordUIDindex word_uid_index() const;
    wordrep::POSUIDindex pos_uid_index() const;
    wordrep::ArcLabelUIDindex arclabel_uid_index() const;
    wordrep::DepParsedTokens dep_parsed_tokens() const;
    wordrep::WordImportance word_importance() const;
    wordrep::VocaInfo voca_info() const;
    wordrep::WordCaseCorrector word_case_corrector(wordrep::WordImportance const& importance) const;

    wordrep::WikidataUIDindex wikientity_uid_index() const;
    wikidata::EntityModule wikientity_module() const;

    Dataset empty_dataset() const;
    Dataset load_dataset() const;
    data::DBIndexer db_indexer() const;
    Config config;
};




}//namespace engine
