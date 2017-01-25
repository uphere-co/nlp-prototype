#pragma once

#include <map>
#include <string>

#include "data_source/corenlp_helper.h"

#include "wordrep/dep_parsed.h"
#include "wordrep/word_uid.h"
#include "wordrep/word_prob.h"
#include "wordrep/voca_info.h"
#include "wordrep/word_case_corrector.h"

namespace engine{

struct Config{
    struct Key{
        std::string val;
        bool operator< (Key const& rhs) const {return val < rhs.val;}
    };
    Config(util::json_t const& config) {
        for(auto key : keys) values[key] = util::get_str(config, key.val);
    }
    std::string value(std::string key) const {return values.find(Key{key})->second;}

    std::vector<Key> keys={{"engine_type"},
                           {"corenlp_client_script"},
                           {"word_uids_dump"},
                           {"pos_uids_dump"},
                           {"arclabel_uids_dump"},
                           {"column_uids_dump"},
                           {"word_prob_dump"},
                           {"row_hashes"},
                           {"corenlp_dumps"},
                           {"dep_parsed_store"},
                           {"dep_parsed_prefix"},
                           {"wordvec_store"},
                           {"voca_name"},
                           {"w2vmodel_name"},
                           {"w2v_float_t"}};
    std::map<Key,std::string> values;
};

struct SubmoduleFactory{
    SubmoduleFactory(Config const& config) : config{config} {}

    data::CoreNLPwebclient corenlp_webclient() const;
    wordrep::WordUIDindex word_uid_index() const;
    wordrep::POSUIDindex pos_uid_index() const;
    wordrep::ArcLabelUIDindex arclabel_uid_index() const;
    wordrep::DepParsedTokens dep_parsed_tokens() const;
    wordrep::WordImportance word_importance() const;
    wordrep::VocaInfo voca_info() const;
    wordrep::WordCaseCorrector word_case_corrector(wordrep::WordImportance const& importance) const;
    Config config;
};


std::ostream& operator<< (std::ostream& os, Config::Key const& key);
std::ostream& operator<< (std::ostream& os, Config const& config);

}//namespace engine
