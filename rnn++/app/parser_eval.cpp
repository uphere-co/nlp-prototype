#include "tests/test_tree_spans.h"

#include <iostream>

#include "utils/binary_tree.h"
#include "utils/string.h"

int main(int /*argc*/, char** argv){
    test::test_simple();
    test::test_reconstruct_merge_history();
    return 0;
    using util::Node;
    using namespace util;
    auto lines1 = util::string::readlines(argv[1]);
    auto lines2 = util::string::readlines(argv[2]);
    auto score_sum{0.0};
    for(auto i=0ull; i<lines1.size(); ++i){
        auto tree1 = deserialize_binary_tree<Node>(lines1[i]);
        auto tree2 = deserialize_binary_tree<Node>(lines2[i]);        
        auto spans1=get_span_hashes(tree1);
        auto spans2=get_span_hashes(tree2);
        if(!spans1.size() || !spans2.size()) {
            std::cout << lines1[i] << std::endl;
            std::cout << lines2[i] << std::endl;
            continue;
        }
        auto diff=span_diffs(spans1, spans2);
        auto score=1.0*(spans1.size()-diff)/spans1.size();
        score_sum += score;
    }
    std::cout << score_sum / lines1.size() << std::endl;
    return 0;
}