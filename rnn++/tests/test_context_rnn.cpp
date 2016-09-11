#include "tests/test_context_rnn.h"

#include "fmt/printf.h"

#include "utils/hdf5.h"
#include "utils/math.h"
#include "utils/string.h"
#include "utils/linear_algebra.h"
#include "utils/print.h"
#include "utils/profiling.h"
#include "utils/parallel.h"
#include "utils/logger.h"
#include "utils/span.h"

#include "parser/optimizers.h"
#include "parser/parser.h"

using namespace util;
using namespace util::io;
using namespace util::math;
using namespace rnn::wordrep;
using namespace rnn::config;
using namespace rnn::simple_model::optimizer;
using namespace rnn::simple_model;
using namespace rnn::simple_model::test;


namespace {

constexpr int len_context = 2;
struct NodeContext{
    using idx_t = std::ptrdiff_t;
    using node_t = tree::Node;
    using span_t = util::span_dyn<node_t>;
    NodeContext(span_t nodes, idx_t idx_self)
    : self(nodes[idx_self]) {}
    node_t& self;
    span_t lefts;
    span_t rights;
};

struct InializedNodesContext{
    InializedNodesContext(InializedLeafNodes &init_nodes)
    : nodes{init_nodes.val} {
        auto n=nodes.length();
        for(decltype(n)i=0; i!=n; ++i){
            NodeContext cnode{nodes, i};
            auto left_beg=i>len_context?i-len_context:0;
            auto right_end=i+len_context+1<n?i+len_context+1:n;
            cnode.lefts=nodes.subspan(left_beg,i-left_beg);
            cnode.rights=nodes.subspan(i+1,right_end-(i+1));
            cnodes.push_back(cnode);
        }
    }

    util::span_dyn<tree::Node> nodes;
    std::vector<NodeContext> cnodes;
};

}//nameless namespace
namespace rnn{
namespace simple_model{
namespace test{

void test_context_node(){
    Voca voca =load_voca("data.h5", "1b.model.voca");
    auto voca_vecs = load_voca_vecs<100>("data.h5", "1b.model", util::DataType::sp);
    std::cerr << voca_vecs.size() << " " << voca.size() <<std::endl;
    VocaIndexMap word2idx = voca.indexing();

    auto sentence = u8"A symbol of British pound is £ .";
    auto idxs = word2idx.getIndex(sentence);
    auto word_block = voca_vecs.getWordVec(idxs);

    auto words = util::string::split(sentence);
    auto leaf_nodes = construct_nodes_with_reserve(words, idxs);
    InializedLeafNodes nodes{std::move(leaf_nodes), word_block};
    InializedNodesContext cnodes{nodes};

    for(auto const &cnode: cnodes.cnodes){
        for(auto const &node : cnode.lefts) fmt::print("{} ", node.name.val);
        fmt::print(", {} , ", cnode.self.name.val);
        for(auto const &node : cnode.rights) fmt::print("{} ", node.name.val);
        fmt::print("\n");
    }
}

}//namespace rnn::simple_model::test
}//namespace rnn::simple_model
}//namespace rnn
