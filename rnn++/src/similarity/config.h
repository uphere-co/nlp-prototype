#pragma once

#include <map>
#include <string>

#include "data_source/corenlp_helper.h"

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

    data::CoreNLPwebclient corenlp_client() const {
        return {config.value("corenlp_client_script")};
    }
    Config config;
};


std::ostream& operator<< (std::ostream& os, Config::Key const& key);
std::ostream& operator<< (std::ostream& os, Config const& config);

}//namespace engine
