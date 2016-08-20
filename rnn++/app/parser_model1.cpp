#include "parser/parser.h"

using namespace rnn::config;
using namespace rnn::simple_model;
using namespace rnn::simple_model::detail;

int main(int argc, char** argv){
    auto param = load_param("rnn_params.h5", argv[1], util::DataType::dp);    
    // auto param = randomParam(0.05);
    VocaInfo rnn{file_name, voca_name, w2vmodel_name, word_dim};
    auto testset=TokenizedSentences{argv[2]};
    for(auto line : testset.val){
        auto initial_nodes = rnn.initialize_tree(line);
        auto &nodes = initial_nodes.val;
        auto top_nodes = merge_leaf_nodes(param, nodes);
        if(!top_nodes.size()) {
            std::cout << line << std::endl;
            continue;
        }
        auto merge_history = foward_path(param, top_nodes);
        auto top_node = *std::find_if(top_nodes.cbegin(),top_nodes.cend(),
                                     [](auto x){return x->parent==nullptr;});
        std::cout << top_node->name.val << std::endl;
    }
    return 0;
}
