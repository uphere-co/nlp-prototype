#pragma once

//TODO: remove the include and replace util::io::H5name with std::string??
#include "utils/hdf5.h"
#include "utils/math.h" //TODO: it is for util::math::FunName only. Separate it from other part.

namespace rnn{
namespace config{

util::io::H5name file_name{"data.h5"};
util::io::H5name voca_name{"1b.model.voca"};
util::io::H5name w2vmodel_name{"1b.model"};

util::io::H5name rnn_param_store_name{"rnnparser.h5"};
util::io::H5name rnn_param_name{"rnn.Parser#447cc3c8.18800"};

//voca_max_word_len can be read using `h5dump -H` command.
//It can be directly read from a H5File,
//but it needs knowledge of low level details of HDF5.
constexpr int voca_max_word_len = 74;
//voca_size, word_dim are easy to get programmatically.
//For consistencies, however, they are set by runtime configuration.
constexpr size_t voca_size=552402;
constexpr int word_dim=100;

constexpr auto activation = util::math::FunName::tanh;
constexpr auto activation_df = util::math::FunName::d_tanh;
}//namescpae config
}//namespace rnn
