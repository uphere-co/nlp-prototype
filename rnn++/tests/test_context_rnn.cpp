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

using namespace util::io;
using namespace util::math;
using namespace rnn::wordrep;
using namespace rnn::config;
using namespace rnn::simple_model::optimizer;
using namespace rnn::simple_model;

namespace{

template<typename T>
struct Node{
    using prop_t = T;
    Node(T &&property) : prop{std::move(property)} {}
    Node(T const &property) : prop{property} {}

    static auto blank_node(){return Node{T{}};}
    bool is_combined() const {return (left!=nullptr)&(right!=nullptr);}
    bool is_leaf() const {return (left==nullptr)&(right==nullptr);}

    Node const *left=nullptr;
    Node const *right=nullptr;

    T prop;
};


}//nameless namespace

namespace rnn{
namespace context_model{

template<typename FLOAT, int WORD_DIM, int LEN_CTX>
struct Context{
    using val_t = FLOAT;
    using node_t = Node<Context>;
    using vec_t = util::span_1d<val_t,WORD_DIM>;
    static constexpr auto len_context = LEN_CTX;
    static constexpr auto word_dim = WORD_DIM;

    Context(std::vector<val_t> &&raw, util::cstring_span<> name)
            : vecs{std::move(raw)}, vspan{vecs}, name{name},
              vec{       vspan.subspan(0,          WORD_DIM)},
              vec_wsum{  vspan.subspan(WORD_DIM,   WORD_DIM)},
              vec_update{vspan.subspan(2*WORD_DIM, WORD_DIM)} {
        for(auto &x:left_ctxs) x=nullptr;
        for(auto &x:right_ctxs) x=nullptr;
        assert(vecs.size()==3*WORD_DIM);
    }
    Context(std::vector<val_t> const &raw, util::cstring_span<> name)
    : Context(std::vector<val_t>{raw}, name) {}
    Context()
    : Context(std::move(std::vector<val_t>(3*WORD_DIM)), {}) {}
    Context(util::cstring_span<> name, vec_t word_vec)
    : Context(std::move(std::vector<val_t>(3*WORD_DIM)), name)
    {
        std::copy(word_vec.cbegin(), word_vec.cend(), vec.begin());
    }
    Context(Context const &orig)
    : Context(orig.vecs, orig.name) {
        std::copy(orig.left_ctxs.cbegin(), orig.left_ctxs.cend(), left_ctxs.begin());
        std::copy(orig.right_ctxs.cbegin(), orig.right_ctxs.cend(), right_ctxs.begin());
    }

    void set_context(util::span_dyn<node_t> lefts,
                     util::span_dyn<node_t> rights){
        //TODO:Simplify this
        auto m = lefts.length();
        for(decltype(m)i=0; i<m; ++i) left_ctxs[i]  = &lefts[i];
        auto n = rights.length();
        for(decltype(n)i=0; i<n; ++i) right_ctxs[i] = &rights[i];
    }

    std::vector<val_t> vecs;
    util::span_1d <val_t,  3*WORD_DIM> vspan;
    std::array<node_t const*, LEN_CTX> left_ctxs;
    std::array<node_t const*, LEN_CTX> right_ctxs;
    util::cstring_span<> name;
    vec_t vec;
    vec_t vec_wsum;
    vec_t vec_update;
    val_t score{0.0};
};

using Node = ::Node<Context<rnn::type::float_t, rnn::config::word_dim, 0>>;


struct UninializedLeafNodes{
    UninializedLeafNodes(std::vector<Node> &&nodes) : val(std::move(nodes)) {}
    std::vector<Node> val;
};

auto construct_nodes_with_reserve=[](auto const &voca, auto const &word_block,
                                     auto const &idxs){
    std::vector<Node> nodes;
    nodes.reserve(idxs.size()*2-1);
    for(auto idx : idxs){
        Node::prop_t prop{voca.getWordSpan(idx), word_block[idx]};
        Node node{std::move(prop)};
        nodes.push_back(node);
    }
    return UninializedLeafNodes{std::move(nodes)};
};

struct InitializedNodes{
    InitializedNodes(UninializedLeafNodes &&leafs)
            : val{std::move(leafs.val)}, nodes{val} {
        auto len_context = Node::prop_t::len_context;
//        auto nodes=util::span_dyn<Node>{val};
        auto n=nodes.length();
        assert(&nodes[0]==&val[0]);
        for(decltype(n)i=0; i!=n; ++i){
            auto left_beg=i>len_context?i-len_context:0;
            auto right_end=i+len_context+1<n?i+len_context+1:n;
            auto lefts = nodes.subspan(left_beg,i-left_beg);
            auto rights= nodes.subspan(i+1,right_end-(i+1));
            nodes[i].prop.set_context(lefts, rights);
        }
    }
    std::vector<Node> val;
    util::span_dyn<Node> nodes;
};


void print(util::cstring_span<> word){
    for (auto e:word)
        fmt::print("{}", e);
    fmt::print(" ");
}
void print_cnode(Node const &cnode) {
    for(auto left : cnode.prop.left_ctxs) if(left!= nullptr) print(left->prop.name);
    fmt::print(" __ ");
    print(cnode.prop.name);
    fmt::print(" __ ");
    for(auto right : cnode.prop.right_ctxs) if(right!= nullptr) print(right->prop.name);
    fmt::print("\n");
//    for (auto const &node : cnode.lefts) fmt::print("{} ", node.name.val);
//    fmt::print("__ {} __ ", cnode.self.name.val);
//    for (auto const &node : cnode.rights) fmt::print("{} ", node.name.val);
//    fmt::print("\n");
}
struct Param{
    static constexpr auto dim = rnn::config::word_dim;
    static constexpr auto len_context = 2;
    static constexpr auto d_ext =util::dim<dim>();
    static constexpr auto lc_ext = util::dim<len_context>();

    using val_t = rnn::type::float_t;
    using mats_t= util::span_3d<val_t, len_context, dim,dim>;
    using mat_t = util::span_2d<val_t, dim,dim>;
    using vec_t = util::span_1d<val_t, dim>;
    Param()
    : _val(dim*dim*(2+2*len_context)+dim*2, 0), span{_val},
      w_context_left{util::as_span(span.subspan(0, len_context*dim*dim), lc_ext,d_ext,d_ext)},
      w_left{ util::as_span(span.subspan(len_context*dim*dim, dim*dim),     d_ext,d_ext)},
      w_right{util::as_span(span.subspan((1+len_context)*dim*dim, dim*dim), d_ext, d_ext)},
      w_context_right{util::as_span(span.subspan((2+len_context)*dim*dim, len_context*dim*dim),lc_ext,d_ext,d_ext)},
      bias{util::as_span(span.subspan((2+2*len_context)*dim*dim, dim), d_ext)},
      u_score{util::as_span(span.subspan((2+2*len_context)*dim*dim+dim, dim), d_ext)}
    {}

    std::vector<val_t> serialize() const {return _val;};

    std::vector<val_t> _val;
    util::span_1d<val_t, dim*dim*(2+2*len_context)+dim*2> span;
    mats_t w_context_left;
    mat_t w_left;
    mat_t w_right;
    mats_t w_context_right;
    vec_t bias;
    vec_t u_score;
};

auto weighted_sum=[](int64_t i, auto &word_vec,
                     auto const &w_left, auto const &w_right,
                     auto const &bias,
                     auto const &word_left, auto const &word_right) {
    using util::math::dot;
    word_vec[i] = dot(w_left[i], word_left)+dot(w_right[i], word_right) + bias[i];
};
auto accumulate_context_weights=[](int64_t i, auto &word_vec,
                                   auto const &w_context_left, auto const &w_context_right,
                                   auto const &left_ctxs, auto const &right_ctxs) {
    using util::math::dot;
    assert(left_ctxs.size()==right_ctxs.size());
    auto n_left=left_ctxs.size();
    for(decltype(n_left)k=0; k!=n_left; ++k) {
        if (left_ctxs[k])
            word_vec[i] += dot(w_context_left[k][i], left_ctxs[k]->prop.vec);
        if (right_ctxs[k])
            word_vec[i] += dot(w_context_right[k][i], right_ctxs[k]->prop.vec);
    }
};

auto activation_fun=[](int64_t i, auto &out, auto const &x) {
    out[i] = util::math::Fun<rnn::config::activation>(x[i]);
};
auto activation_dfun=[](int64_t i, auto &out, auto const &x) {
    out[i] =  util::math::Fun<rnn::config::activation_df>(x[i]);
};

void weighted_sum_word_pair(Param const &param, Node::prop_t &self,
                            Node::prop_t const &left, Node::prop_t const &right) {
    auto vecloop_void = util::math::VecLoop_void<Param::val_t, Param::dim>{};
    vecloop_void(weighted_sum, self.vec_wsum,
                 param.w_left, param.w_right, param.bias,
                 left.vec, right.vec);
    vecloop_void(accumulate_context_weights, self.vec_wsum,
                 param.w_context_left, param.w_context_right,
                 self.left_ctxs, self.right_ctxs);
}

Param::val_t scoring_node(Param const &param, Node::prop_t const &node) {
    using namespace util::math;
    return dot(param.u_score, node.vec) / norm_L2(param.u_score);
}

Node::prop_t compose_node_prop(Param const &param, Node::prop_t const &left, Node::prop_t const &right){
    Node::prop_t self{};
    self.left_ctxs = left.left_ctxs;
    self.right_ctxs = right.right_ctxs;
    weighted_sum_word_pair(param, self, left, right);
    auto vecloop_void = util::math::VecLoop_void<Param::val_t, Param::dim>{};
    vecloop_void(activation_fun, self.vec, self.vec_wsum);
    self.score= scoring_node(param, self);
    return self;
}
Node compose_node(Param const &param, Node const &left, Node const &right) {
    auto prop = compose_node_prop(param, left.prop, right.prop);
    Node new_node{prop};
    new_node.left=&left;
    new_node.right=&right;
    return new_node;
}

}//namespace rnn::context_model
}//namespace rnn
namespace {

void copy(rnn::simple_model::Param const &ori, rnn::context_model::Param &dest){
    std::copy(ori.w_left.span.cbegin(), ori.w_left.span.cend(), dest.w_left.begin());
    std::copy(ori.w_right.span.cbegin(),ori.w_right.span.cend(),dest.w_right.begin());
    std::copy(ori.bias.span.cbegin(),   ori.bias.span.cend(),   dest.bias.begin());
    std::copy(ori.u_score.span.cbegin(),ori.u_score.span.cend(),dest.u_score.begin());
}

}//nameless namespace

namespace rnn{
namespace context_model{
namespace test{
Word operator"" _w (const char* word, size_t /*length*/)
{
    return Word{word};
}
void test_context_node(){
    using namespace rnn::simple_model;
    rnn::context_model::Param param{};
    auto param_rnn1 = randomParam(0.05);
    param_rnn1.bias.span *= rnn::type::float_t{0.0};
    copy(param_rnn1, param);

    Voca voca =load_voca("data.h5", "1b.model.voca");
    auto voca_vecs = load_voca_vecs<100>("data.h5", "1b.model", util::DataType::sp);
    std::cerr << voca_vecs.size() << " " << voca.size() <<std::endl;
    VocaIndexMap word2idx = voca.indexing();

    std::string sentence = u8"A symbol of British pound is £ .";
    auto idxs = word2idx.getIndex(sentence);
//    auto word_block = voca_vecs.getWordVec(idxs);

    auto leaf_nodes = construct_nodes_with_reserve(voca, voca_vecs, idxs);
    for(auto idx : idxs) fmt::print("{} ", idx);
    fmt::print(": idxs\n");
    auto nodes = InitializedNodes(std::move(leaf_nodes));
    assert(nodes.val.size()==8);
    auto vec_sum{0.0};
//    for(auto const &node:nodes.val)
//        for(auto x : node.prop.vec) vec_sum += x;
    for(auto x : nodes.val[0].prop.vec) vec_sum += x;
    fmt::print("vec sum = {}\n", vec_sum);
//    assert(nodes.val[0].prop.right_ctxs[0]==&nodes.val[1]);
//    assert(nodes.val[0].prop.left_ctxs[0]== nullptr);
    for(auto& node: nodes.val) print_cnode(node);
    fmt::print("\n");

//    auto word_views = util::string::unpack_tokenized_sentence(sentence);
//    auto new_node = detail::merge_node(param, nodes.val[1], nodes.val[2]);
//    auto new_cnode = merge_cnode(cparam, cnodes.cnodes[1], cnodes.cnodes[2]);
//    print_cnode(new_cnode);
}

}//namespace rnn::context_model::test
}//namespace rnn::context_model
}//namespace rnn
