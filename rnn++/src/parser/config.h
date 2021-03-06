#pragma once

#include <string>
#include "parser/basic_type.h"
#include "utils/type_param.h"
#include "utils/math_funname.h"

namespace rnn{
namespace config{

//TODO: remove the include and replace util::io::H5name with std::string??
extern std::string file_name;
extern std::string voca_name;
extern std::string w2vmodel_name;
// util::io::H5name file_name{"word2vec.h5"};
// util::io::H5name voca_name{"foo.word"};
// util::io::H5name w2vmodel_name{"foo.vec"};
// constexpr util::DataType w2vmodel_f_type = util::DataType::sp;
constexpr util::DataType w2vmodel_f_type = util::DataType::dp;

extern std::string rnn_param_store_name;
//HDF5 dataset name to use as inital RNN parameter.
extern std::string rnn_param_name;
constexpr util::DataType param_f_type = util::DataType::dp;

extern std::string trainset_name;
extern std::string testset_name;

constexpr int word_dim=100;

// constexpr auto activation = util::math::FunName::ax;
// constexpr auto activation_df = util::math::FunName::d_ax;
constexpr auto activation    = util::math::FunName::tanh;
constexpr auto activation_df = util::math::FunName::d_tanh;
// constexpr auto activation    = util::math::FunName::test;
// constexpr auto activation_df = util::math::FunName::d_test;

constexpr rnn::type::idx_t n_minibatch = 1000;
constexpr rnn::type::idx_t n_epoch = 10;
}//namescpae config
}//namespace rnn

