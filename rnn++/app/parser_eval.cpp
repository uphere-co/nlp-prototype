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
auto node_name_field   = [](char x){return x!='(' && x!=')' && x!=' ';};

auto fill_child_node=[](auto parent, auto child){
    child->parent=parent;
    if(parent->left ==nullptr) parent->left=child;
    else if(parent->right ==nullptr) parent->right=child;
    else {assert(0);}
};
std::vector<Node> deserialize_binary_tree(std::string tree_str){
    auto n_composites = std::count_if(tree_str.cbegin(), tree_str.cend(), node_begin);
    std::vector<Node> nodes(n_composites*2+1);
    auto it=tree_str.cbegin();
    if(!node_begin(*it)){assert(0);}

    Node *current_leaf = &nodes[0];
    Node *current_node = &nodes[n_composites+1];
    Node *new_node = current_node;
    while(++it!=tree_str.cend()){
        if(node_begin(*it)){
            ++new_node;
            fill_child_node(current_node, new_node);
            current_node = new_node;
        } else if (node_end(*it)){
            current_node = current_node->parent;
        } else if (node_sep(*it)){
        } else{
            it = std::find_if_not(it, tree_str.cend(), node_name_field)-1;
            fill_child_node(current_node, current_leaf++);
        }
    }
    assert(current_node == nullptr);
    return nodes;
}
auto assert_node=[](auto const &parent, auto const &left, auto const &right){
    assert(parent.left == &left);
    assert(parent.right == &right);
    assert(&parent == left.parent);
    assert(&parent == right.parent);
};

void test_simple(){
    {
        auto nodes = deserialize_binary_tree("(a b)");
        assert(nodes.size()==3);
        assert_node(nodes[2], nodes[0], nodes[1]);
    }
    {
        auto nodes = deserialize_binary_tree("((aaaa bb) (ccc dd))");
        assert(nodes.size()==7);
        assert_node(nodes[4], nodes[5], nodes[6]);
        assert_node(nodes[5], nodes[0], nodes[1]);
        assert_node(nodes[6], nodes[2], nodes[3]);
    }
    {
        auto nodes = deserialize_binary_tree("(((c d) b) ((c (c d)) d))");
        assert(nodes.size()==13);
        assert_node(nodes[7], nodes[8], nodes[10]);
        assert_node(nodes[8], nodes[9], nodes[2]);
        assert_node(nodes[9], nodes[0], nodes[1]);
        assert_node(nodes[10], nodes[11], nodes[6]);
        assert_node(nodes[11], nodes[3], nodes[12]);
        assert_node(nodes[12], nodes[4], nodes[5]);
    }
    
}
int main(){
    test_simple();
    return 0;
}