//TODO: remove the include and replace util::io::H5name with std::string??
#include "parser/config.h"

namespace rnn{
namespace config{

std::string file_name{"data.h5"};
std::string voca_name{"1b.model.voca"}; 
std::string w2vmodel_name{"1b.model"};
// std::string file_name{"glove.h5"};
// std::string voca_name{"glove.6B.100.voca"}; 
// std::string w2vmodel_name{"glove.6B.100"};
// std::string file_name{"word2vec.h5"};
// std::string voca_name{"foo.word"};
// std::string w2vmodel_name{"foo.vec"};

std::string rnn_param_store_name{"rnn_params.h5"};
//HDF5 dataset name to use as inital RNN parameter.
std::string rnn_param_name{"model1.98b94481.200"};
// std::string rnn_param_store_name{"rnnparser.h5"};
// //HDF5 dataset name to use as inital RNN parameter.
// std::string rnn_param_name{"rnn.Parser#447cc3c8.18800"};

std::string trainset_name{"1b.trainset"};
std::string testset_name{"1b.testset"};
// std::string testset_name{"1b.testset.glove.sample"};
// std::string testset_name{"1b.training.sample"};
}//namescpae config
}//namespace rnn
