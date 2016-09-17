#pragma once

#include "tbb/task_group.h"

//#include "wordrep/sentence2vec.h"

#include "parser/parser.h"
#include "parser/wordvec.h"
#include "utils/profiling.h"
#include "utils/parallel.h"
#include "utils/linear_algebra.h"
#include "utils/print.h"
#include "utils/json.h"
#include "utils/loop_gen.h"

#include  "parser/parser.h"

using namespace rnn::wordrep;
using namespace rnn::simple_model;
using namespace util;
using namespace util::math;
using namespace util::io;

using json = nlohmann::json;


using char_t = char;
using wcount_t = int32_t;
using val_t = double;
using idx_t = std::size_t;
//constexpr int word_dim=100; //already declared in sentence2vec.h
constexpr util::DataType w2vmodel_f_type = util::DataType::dp;

constexpr int word_dim=100;

struct Query{
    using vec_view_t = WordBlock::span_t;
    using vec_t = Vector<WordBlock::float_t, 100>;
    Query(std::string word, vec_view_t vec, Voca const &voca)
    :query_word{word}, query_vec{vec}, distances(voca.size())
    {}
    std::string query_word;
    vec_t query_vec;
    std::vector<val_t> distances;
};

void collect_query_result(Query const &query, Voca const &voca, json &output);


json collect_queries_results(std::vector<Query> const &queries, Voca const &voca);

void KLdistance();

struct SimilaritySearch{
    SimilaritySearch(json const &config)
    :   sent_vecs{load_voca_vecs<word_dim>(config["phrase_store"], config["phrase_vec"], util::DataType::dp)},
        phrase_voca{load_voca(config["phrase_store"], config["phrase_word"])},
        param{load_param(config["rnn_param_store"], config["rnn_param_uid"], util::DataType::dp)},
        rnn{config["wordvec_store"], config["voca_name"], config["w2vmodel_name"], util::DataType::dp}
    {}

    json process_queries(json ask) const;

    WordBlock sent_vecs;
    Voca phrase_voca;
    Param param; 
    VocaInfo rnn;
};

extern "C" {
  void querytest( void );
}
