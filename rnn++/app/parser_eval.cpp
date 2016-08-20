#include <vector>
#include <string>
#include <algorithm>
#include <cassert>

struct Node{
    std::string name;
    Node* left=nullptr;
    Node* right=nullptr;
    Node* parent=nullptr;
};

auto node_begin = [](char x){return x=='(';};
auto node_end   = [](char x){return x==')';};
auto node_sep   = [](char x){return x==' ';};
//auto node_name_field   = [](char x){return x!='(' && x!=')' && x!=' ';};

auto fill_child_node=[](auto parent, auto child){
    if(parent->left ==nullptr) parent->left=child;
    else if(parent->right ==nullptr) parent->right=child;
    else {assert(0);}
};
std::vector<Node> deserialize_binary_tree(std::string tree_str){
    auto n_nodes = std::count_if(tree_str.cbegin(), tree_str.cend(), node_begin);
    std::vector<Node> nodes(n_nodes);
    auto it=tree_str.cbegin();
    if(!node_begin(*it)){assert(0);}

    Node *current_node = &nodes.front();
    Node *new_node = current_node;
    while(++it!=tree_str.cend()){
        if(node_begin(*it)){
            ++new_node;
            new_node->parent =current_node;
            fill_child_node(current_node, new_node);
            current_node = new_node;
        } else if (node_end(*it)){
            current_node = current_node->parent;
        } else if (node_sep(*it)){
        } else{
        }
    }
    assert(current_node == nullptr);
    return nodes;
}
void test_simple(){
    {
        auto nodes = deserialize_binary_tree("(a b)");
        assert(nodes.size()==1);
    }
    {
        auto nodes = deserialize_binary_tree("((aaaa bb) (ccc dd))");
        assert(nodes.size()==3);
        assert(&nodes[0] == nodes[1].parent);
        assert(&nodes[0] == nodes[2].parent);
        assert(nodes[0].left == &nodes[1]);
        assert(nodes[0].right == &nodes[2]);
    }
    {
        auto nodes = deserialize_binary_tree("(((c d) b) ((c (c d)) d))");
        assert(nodes.size()==6);
        assert(nodes[0].left == &nodes[1]);
        assert(nodes[0].right == &nodes[3]);
        assert(nodes[1].left == &nodes[2]);
        assert(nodes[1].right == nullptr);
        assert(nodes[3].left == &nodes[4]);
        assert(nodes[3].right == nullptr);
        // assert(nodes[4].left ==  nullptr);
        // assert(nodes[4].right == &nodes[5]);
    }
    
}
int main(){
    test_simple();
    return 0;
}