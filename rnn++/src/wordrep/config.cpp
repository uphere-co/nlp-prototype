#include "wordrep/config.h"

#include "utils/json.h"

using util::get_str;

namespace wordrep {

ConfigParams::ConfigParams(util::json_t const& config)
        : corenlp_client{{get_str(config, "corenlp_client_script")}},
          word_list{{get_str(config, "words_list")}},
          word_uid{{get_str( config, "word_uid_bin")}},
          pos_uid{{get_str(config, "pos_uid_bin")}},
          arclabel_uid{{get_str(config, "arclabel_uids_dump")}},
          dataset_columns{{get_str(config, "column_uids_dump")}},
          parsed_text{{get_str(config, "dep_parsed_bins")}},
          annotated_tokens{get_str(config, "annotated_tokens"),
                           std::stoi(get_str(config, "annotated_tokens_n_block"))},
          voca_idx{{get_str(config, "voca_bin")}},
          word_vecs{{get_str(config, "w2vmodel_bin")}},
          word_imporance{{get_str(config, "word_prob_bin")}},
          sim_words{{get_str(config, "simword_bin")}},
          entity_names{{get_str(config, "wikidata_entities_by_name")}},
          entity_uids{{get_str(config, "wikidata_entities_by_uid")}},
          entity_properties{{get_str(config, "wikidata_properties")}},
          entity_instances{{get_str(config, "wikidata_instances")}},
          wiki_uid{{get_str(config, "wikidata_uids")}},
          named_entity_uid{{get_str(config, "named_entity_uids")}}
{}

}//namespace wordrep
