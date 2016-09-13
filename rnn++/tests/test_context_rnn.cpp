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


namespace rnn{
namespace context_model{

struct Param{
    static constexpr auto dim = rnn::config::word_dim;
    static constexpr auto d_ext =util::dim<dim>();
    static constexpr auto len_context = 0;
    static constexpr auto lc_ext = util::dim<len_context>();
    using val_t = rnn::type::float_t;
    using raw_t = std::vector<val_t>;
    using raw_span_t = util::span_dyn<val_t>;
    using ws_t  = util::span_3d<val_t, len_context*2+2, dim,dim>;
    using mats_t= util::span_3d<val_t, len_context, dim,dim>;
    using mat_t = util::span_2d<val_t, dim,dim>;
    using vec_t = util::span_1d<val_t, dim>;
    Param()
    : _val(dim*dim*(2+2*len_context)+dim*2), span{_val},
      w_context_left{gsl::as_span(span.subspan(0, len_context*dim*dim), lc_ext,d_ext,d_ext)},
      w_left{ gsl::as_span(span.subspan(len_context*dim*dim, dim*dim),     d_ext,d_ext)},
      w_right{gsl::as_span(span.subspan((1+len_context)*dim*dim, dim*dim), d_ext, d_ext)},
      w_context_right{gsl::as_span(span.subspan((2+len_context)*dim*dim, len_context*dim*dim),lc_ext,d_ext,d_ext)},
      bias{gsl::as_span(span.subspan((2+2*len_context)*dim*dim, dim), d_ext)},
      u_score{gsl::as_span(span.subspan((2+2*len_context)*dim*dim+dim, dim), d_ext)}
    {}

    raw_t serialize() const {return _val;};

    raw_t _val;
    raw_span_t span;
    mats_t w_context_left;
    mat_t w_left;
    mat_t w_right;
    mats_t w_context_right;
    vec_t bias;
    vec_t u_score;
};

}//namespace rnn::context_model
}//namespace rnn
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

void copy(rnn::simple_model::Param const &ori, rnn::context_model::Param &dest){
    std::copy(ori.w_left.span.cbegin(), ori.w_left.span.cend(), dest.w_left.begin());
    std::copy(ori.w_right.span.cbegin(),ori.w_right.span.cend(),dest.w_right.begin());
    std::copy(ori.bias.span.cbegin(),   ori.bias.span.cend(),   dest.bias.begin());
    std::copy(ori.u_score.span.cbegin(),ori.u_score.span.cend(),dest.u_score.begin());
}
}//nameless namespace

namespace rnn{
namespace simple_model{
namespace test{

void test_context_node(){
    constexpr auto dim=util::dim<2>();
    std::vector<float> vec(1000000);
    util::span_dyn<float> aa{vec};
    auto bb = gsl::as_span(aa.subspan(0, 2*2*2),util::dim<2>(),dim,dim);
    util::span_3d<float,1,2,2> cc = gsl::as_span(bb.subspan(0, 1*2*2),util::dim<1>(),dim,dim);
    rnn::context_model::Param cparam;
    auto param = randomParam(0.05);
    param.bias.span *= rnn::type::float_t{0.0};
    copy(param, cparam);

    Voca voca =load_voca("data.h5", "1b.model.voca");
    auto voca_vecs = load_voca_vecs<100>("data.h5", "1b.model", util::DataType::sp);
    std::cerr << voca_vecs.size() << " " << voca.size() <<std::endl;
    VocaIndexMap word2idx = voca.indexing();

    auto sentence = u8"A symbol of British pound is £ .";
    auto idxs = word2idx.getIndex(sentence);
    auto word_block = voca_vecs.getWordVec(idxs);

    auto words = util::string::split(sentence);
    auto leaf_nodes = construct_nodes_with_reserve(words, idxs);
    //Nodes are initialized if word vectors are set.
    InializedLeafNodes nodes{std::move(leaf_nodes), word_block};
    //NodeCcontexts are initailized if context words of initialized nodes are set
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
