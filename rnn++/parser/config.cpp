//TODO: remove the include and replace util::io::H5name with std::string??
#include "parser/config.h"

namespace rnn{
namespace config{

util::io::H5name file_name{"data.h5"};
util::io::H5name voca_name{"1b.model.voca"};
util::io::H5name w2vmodel_name{"1b.model"};
// util::io::H5name file_name{"word2vec.h5"};
// util::io::H5name voca_name{"foo.word"};
// util::io::H5name w2vmodel_name{"foo.vec"};

util::io::H5name rnn_param_store_name{"rnnparser.h5"};
//HDF5 dataset name to use as inital RNN parameter.
util::io::H5name rnn_param_name{"rnn.Parser#447cc3c8.18800"};

std::string trainset_name{"1b.training.sample"};
}//namescpae config
}//namespace rnn
