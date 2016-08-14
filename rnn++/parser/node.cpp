#include "parser/node.h"

namespace rnn{
namespace simple_model{
namespace tree{

void print_all_descents(Node const & node) {
    std::cerr<< node.score << " : "<< node.name.val << std::endl;
    if(node.left != nullptr) print_all_descents(*node.left);
    if(node.right!= nullptr) print_all_descents(*node.right);
}
}//namespace rnn::simple_model::tree
}//namespace rnn::simple_model
}//namespace rnn

