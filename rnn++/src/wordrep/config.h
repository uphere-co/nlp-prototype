#pragma once

#include <string>
#include "wordrep/file_formats.h"
#include "utils/json.h"

namespace wordrep {

struct ConfigParams {
    ConfigParams(util::json_t const& config);

    PythonScript corenlp_client;
    TextFile     word_list;

    UIDIndexFile word_uid;
    UIDIndexFile pos_uid;
    TextFile     arclabel_uid;

    TextFile           dataset_columns;

    DepParsedFile      parsed_text;
    AnnotatedTokenFile annotated_tokens;

    VocaIndexMapFile   voca_idx;
    WordEmbeggingFile  word_vecs;
    WordImportanceFile word_imporance;
    SimilarWordsFile   sim_words;

    WikiEntityByNameFile entity_names;
    WikiEntityByUIDFile  entity_uids;
    WikiPropertyFile     entity_properties;
    WikiInstanceFile     entity_instances;
    UIDIndexFile         wiki_uid;
    UIDIndexFile         named_entity_uid;
};

}//namespace wordrep
