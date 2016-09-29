#pragma once

#include "parser/parser.h"
#include "parser/wordvec.h"
#include "utils/json.h"

struct SimilaritySearch{
    using json_t = nlohmann::json;
    using voca_info_t = rnn::simple_model::VocaInfo;
    using param_t     = rnn::simple_model::Param;
    SimilaritySearch(json_t const &config)
    :   sent_vecs{rnn::wordrep::load_voca_vecs<param_t::dim>(config["phrase_store"], config["phrase_vec"], util::datatype_from_string(config["float_t"]))},
        phrase_voca{rnn::wordrep::load_voca(config["phrase_store"], config["phrase_word"])},
        param{rnn::simple_model::load_param(config["rnn_param_store"], config["rnn_param_uid"], util::datatype_from_string(config["float_t"]))},
        rnn{config["wordvec_store"], config["voca_name"], config["w2vmodel_name"], util::datatype_from_string(config["float_t"])}
    {}

    json_t static parse(const char *query) {return json_t::parse(query);}

    json_t process_queries(json_t ask) const;

    voca_info_t::voca_vecs_t sent_vecs;
    voca_info_t::voca_t  phrase_voca;
    param_t param;
    voca_info_t rnn;
};


struct BoWVSimilaritySearch{
    using json_t = nlohmann::json;
    using voca_info_t = rnn::simple_model::VocaInfo;
    BoWVSimilaritySearch(json_t const &config)
    : rnn{config["wordvec_store"], config["voca_name"], config["w2vmodel_name"], util::datatype_from_string(config["float_t"])},
      rows{util::string::readlines(config["phrase_rawdata"])}
    {}
    json_t process_queries(json_t ask) const;

    voca_info_t rnn;
    std::vector<std::string> rows;
};

