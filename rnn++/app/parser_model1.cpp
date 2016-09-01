#include "parser/parser.h"
#include "utils/binary_tree.h"
#include "utils/parallel.h"

using namespace rnn::config;
using namespace rnn::simple_model;
using namespace rnn::simple_model::detail;

int main(int /*argc*/, char** argv){
    auto param = load_param("rnn_params.h5", argv[1], util::DataType::dp);    
    // auto param = randomParam(0.05);
    VocaInfo rnn{file_name, voca_name, w2vmodel_name, w2vmodel_f_type};
    auto testset=TokenizedSentences{argv[2]};
    std::vector<std::string> parsed_sents(testset.val.size());
    //for(auto line : testset.val){
    auto n=testset.val.size();
    tbb::parallel_for(decltype(n){0}, n, [&](int i) {
        auto line=testset.val[i];
        auto initial_nodes = rnn.initialize_tree(line);
        auto &nodes = initial_nodes.val;
        auto top_nodes = merge_leaf_nodes(param, nodes);
        if(top_nodes.size()) {
            auto merge_history = foward_path(param, top_nodes);
            auto top_node = util::find_top_node_ptr(top_nodes);
            // std::cout << top_node->name.val << std::endl;
            parsed_sents[i]=std::string{top_node->name.val};
        } else {
            //std::cout << line << std::endl;
            parsed_sents[i]=line;
        }
    });
    for(auto sent:parsed_sents)
        std::cout<<sent<<std::endl;
    return 0;
}
