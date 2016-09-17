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

namespace rnn{
template<typename T>
struct Node{
    using prop_t = T;
    Node(T &&property) : prop{std::move(property)} {}
    Node(T const &property) : prop{property} {}
    Node(Node const &orig)
    : left{orig.left}, right{orig.right},
      prop{orig.prop} {}
    Node(Node &&orig)
    : left{orig.left}, right{orig.right},
      prop{std::move(orig.prop)} {}
    Node& operator=(Node const& orig) {
        prop=orig.prop;
        left=orig.left;
        right=orig.right;
        return *this;
    }

    static auto blank_node(){return Node{T{}};}
    bool is_combined() const {return (left!=nullptr)&(right!=nullptr);}
    bool is_leaf() const {return (left==nullptr)&(right==nullptr);}

    Node const *left=nullptr;
    Node const *right=nullptr;
    T prop;
};
}

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
        score=orig.score;
    }
    Context& operator=(Context const& orig){
        assert(vecs.size()==3*WORD_DIM);
        std::copy(orig.vecs.cbegin(), orig.vecs.cend(), vecs.begin());
        std::copy(orig.left_ctxs.cbegin(), orig.left_ctxs.cend(), left_ctxs.begin());
        std::copy(orig.right_ctxs.cbegin(), orig.right_ctxs.cend(), right_ctxs.begin());
        name=orig.name;
        score=orig.score;
        return *this;
    }
    Context(Context &&orig)
    : Context(std::move(orig.vecs), orig.name) {
        std::copy(orig.left_ctxs.cbegin(), orig.left_ctxs.cend(), left_ctxs.begin());
        std::copy(orig.right_ctxs.cbegin(), orig.right_ctxs.cend(), right_ctxs.begin());
        score=orig.score;
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

using Node = rnn::Node<Context<rnn::type::float_t, rnn::config::word_dim, 0>>;


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
    fmt::print(" , {}\n", cnode.prop.score);
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
    //TODO: Remove this temporal restriction
    Param(Param const &orig) = delete;

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


void update_node_prop(Node::prop_t &self, Param const &param,
                      Node::prop_t const &left, Node::prop_t const &right){
    self.left_ctxs = left.left_ctxs;
    self.right_ctxs = right.right_ctxs;
    weighted_sum_word_pair(param, self, left, right);
    auto vecloop_void = util::math::VecLoop_void<Param::val_t, Param::dim>{};
    vecloop_void(activation_fun, self.vec, self.vec_wsum);
    self.score= scoring_node(param, self);
}
Node::prop_t compose_node_prop(Param const &param, Node::prop_t const &left, Node::prop_t const &right){
    Node::prop_t self{};
    update_node_prop(self, param, left, right);
    return self;
}

Node compose_node(Param const &param, Node const &left, Node const &right) {
    auto prop = compose_node_prop(param, left.prop, right.prop);
    Node new_node{prop};
    new_node.left=&left;
    new_node.right=&right;
    return new_node;
}

void update_node(Param const &param, Node &node) {
    auto& self = node.prop;
    auto& left = node.left->prop;
    auto& right= node.right->prop;
    update_node_prop(self, param, left, right);
}

auto foward_path(Param const &param, std::vector<Node*> top_nodes) ->
std::vector<decltype(top_nodes.size())> {
    std::vector<decltype(top_nodes.size())> merge_history;
    while(top_nodes.size()){
        auto it_max=std::max_element(top_nodes.cbegin(), top_nodes.cend(),
                                     [](auto const x, auto const y){
                                         return x->prop.score < y->prop.score;
                                     });
        auto i_max = it_max - top_nodes.cbegin();
        merge_history.push_back(i_max);
        if(it_max!=top_nodes.cbegin()){
            auto it_left = *(it_max-1);
            it_left->right=*it_max;
            update_node(param, *it_left);
        }
        if(it_max!=top_nodes.cend()-1){
            auto it_right= *(it_max+1);
            it_right->left=*it_max;
            update_node(param, *it_right);
        }
        std::copy(it_max+1,top_nodes.cend(),
                  top_nodes.begin()+i_max);
        top_nodes.pop_back();
    }
    return merge_history;
}

std::vector<Node*> compose_leaf_nodes(Param const &param, std::vector<Node> &leaves)  {
    std::vector<Node*> top_node;
    auto n_leaf = leaves.size();
    if(n_leaf==1) return top_node;
    auto last_leaf = leaves.cend()-1;
    for(auto it=leaves.cbegin(); it!=last_leaf;)
        leaves.push_back( compose_node(param, *it,*++it ));

    for(auto it=leaves.data()+n_leaf; it!=leaves.data()+leaves.size(); ++it)
        top_node.push_back(it);
    return top_node;
}
Param::val_t get_full_greedy_score(Param const &param, InitializedNodes &nodes ) {
    auto& all_nodes = nodes.val;
    auto top_nodes = compose_leaf_nodes(param, all_nodes);
    auto merge_history = foward_path(param, top_nodes);
    Param::val_t score{};
    for(auto node: top_nodes) score+= node->prop.score;
    return score;
}

class DPtable{
public:
    using idx_t = std::size_t;
    using node_t = Node;
    using val_t= node_t::prop_t::val_t;

    DPtable(std::vector<node_t> const &nodes)
    : n_words{nodes.size()}, raw(n_words*n_words, node_t::blank_node()),
      score_sums(raw.size(), std::numeric_limits<val_t>::lowest()),
      penalties(0) {
        for(decltype(n_words)i=0; i<n_words; ++i){
            get(i,i)=nodes[i];
            score_sum(i,i)=0.0;
        }
    }
    node_t& get(idx_t i, idx_t j)  {return raw[i*n_words+j];}
    node_t& root_node() {return get(0,n_words-1);}
    val_t&  score_sum(idx_t i, idx_t j) {return score_sums[i*n_words+j];}
    val_t&  penalty(idx_t i, idx_t j) {return penalties[i*n_words+j];}
    void search_best(Param const &param, idx_t i, idx_t j){
        auto& node=get(i,j);
        for(idx_t k=i; k<j; ++k){
            auto& left =get(i,k);
            auto& right=get(k+1,j);
            auto phrase=compose_node(param, left,right);
            auto score_total=phrase.prop.score+score_sum(i,k)+score_sum(k+1,j);
            auto& current_best_score=score_sum(i,j);
            if(score_total>current_best_score){
                node=phrase;
                current_best_score=score_total;
            }
        }
    }
    void search_best_with_penalty(Param const &param, idx_t i, idx_t j){
        auto& node=get(i,j);
        for(idx_t k=i; k<j; ++k){
            auto& left =get(i,k);
            auto& right=get(k+1,j);
            auto phrase=compose_node(param, left,right);
            auto score_total=phrase.prop.score+penalty(i,j)+score_sum(i,k)+score_sum(k+1,j);
            auto& current_best_score=score_sum(i,j);
            if(score_total>current_best_score){
                node=phrase;
                current_best_score=score_total;
            }
        }
    }
    void compute(Param const &param){
        for(idx_t len=1; len<n_words;++len){
            for(idx_t left=0; left<n_words-len;++left){
                search_best(param,left,left+len);
            }
        }
    }
    void compute(Param const &param, val_t lambda, std::string parsed_sentence){
        set_penalty(lambda, parsed_sentence);
        for(idx_t len=1; len<n_words;++len){
            for(idx_t left=0; left<n_words-len;++left){
                search_best_with_penalty(param,left,left+len);
            }
        }
    }
    void set_penalty(val_t lambda, std::string parsed_sentence){
        using namespace util;
        penalties=std::vector<val_t>(score_sums.size(), -lambda);
        auto label_nodes = deserialize_binary_tree<util::Node>(parsed_sentence);
        auto spans=get_span_hashes(label_nodes);
        for(auto hash:spans) {
            auto left=hash/label_nodes.size();
            auto right=hash%label_nodes.size();
            penalty(left,right)=0.0;
        }
    }
    std::vector<const node_t*> get_phrases(){
        std::vector<const node_t*> phrases;
        collect_phrases(&get(0,n_words-1), phrases);
        return phrases;
    }
    std::vector<const node_t*> get_leafs(){
        std::vector<const node_t*> nodes;
        for(decltype(n_words)i=0;i<n_words; ++i) nodes.push_back(&get(i,i));
        return nodes;
    }

private:
    void collect_phrases(const node_t* node, std::vector<const node_t*> &phrases){
        if(node->is_leaf()) return;
        phrases.push_back(node);
        if(node->left != nullptr) collect_phrases(node->left, phrases);
        if(node->right!= nullptr) collect_phrases(node->right, phrases);
    }
    idx_t n_words;
    std::vector<node_t> raw;
    std::vector<val_t> score_sums;
    std::vector<val_t> penalties;
};

std::string parsed_binary_tree_string(Node const &node){
    if(node.is_leaf()) return node.prop.name.data();
    return "("+full_name(*node.left) +" " + full_name(*node.right)+")";
}

void print_all_descents(Node const &node) {
    std::cerr<< parsed_binary_tree_string(node) << " " <<node.prop.score << std::endl;
    if(node.left != nullptr) print_all_descents(*node.left);
    if(node.right!= nullptr) print_all_descents(*node.right);
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

auto test_rnn_greedy_score(rnn::simple_model::Param &param,
                     rnn::simple_model::VocaInfo &rnn){
    using namespace rnn::simple_model;
    using namespace rnn::simple_model::detail;

    auto sentence_test = u8"A symbol of British pound is £ .";
    auto idxs = rnn.word2idx.getIndex(sentence_test);
    for(auto idx : idxs) fmt::print("{} ", idx);
    fmt::print(": idxs.\n");
    auto initial_nodes = rnn.initialize_tree(sentence_test);
    auto &nodes = initial_nodes.val;
    auto n_words=nodes.size();
    assert(n_words==8);
    auto vec_sum{0.0};
//    for(auto const &node:nodes)
//        for(auto x : node.vec.span) vec_sum += x;
    for(auto x : nodes[0].vec.span) vec_sum += x;
    fmt::print("vec sum = {}\n", vec_sum);

    util::Timer timer{};
    auto top_nodes = merge_leaf_nodes(param, nodes);
    auto merge_history = foward_path(param, top_nodes);
    timer.here_then_reset("RNN greedy forward path.");

    auto score{0.0};
    for(auto const & node:nodes) score+= node.score;
    fmt::print("Model1 : {}\n", score);
    for(auto x : merge_history) fmt::print("{} ", x);
    fmt::print(": merge history\n");
    return score;
}
auto test_rnn_dp_score(rnn::simple_model::Param &param,
                     rnn::simple_model::VocaInfo &rnn){
    using namespace rnn::simple_model;
    using namespace rnn::simple_model::detail;
    util::Timer timer{};

    auto sentence_test = u8"A symbol of British pound is £ .";
    auto idxs = rnn.word2idx.getIndex(sentence_test);
    auto initial_nodes = rnn.initialize_tree(sentence_test);
    auto &nodes = initial_nodes.val;
    auto n_words=nodes.size();
    assert(n_words==8);
    assert(nodes.capacity()==15);
    timer.here_then_reset("Setup word2vecs & nodes");
    DPtable table{nodes};
    table.compute(param);
    auto phrases = table.get_phrases();
    timer.here_then_reset("RNN DP Forward path.");
    auto score_dp{0.0};
    for(auto phrase : phrases) score_dp += phrase->score;
    fmt::print("{}:RNN DP score. {} : score_sum.\n", score_dp, table.score_sum(0,7));
    auto root_node=table.get(0,n_words-1);
    print_all_descents(root_node);

    auto top_nodes = merge_leaf_nodes(param, nodes);
    auto merge_history = foward_path(param, top_nodes);
    auto score{0.0};
    for(auto const &node:nodes){
        if(node.is_leaf()) continue;
        score+=node.score;
    }
    fmt::print("{}:RNN greedy score.\n", score);
    return score_dp;
}

}//nameless namespace


namespace rnn{
namespace context_model{
namespace test{

using namespace rnn::wordrep;
using namespace rnn::config;

Word operator"" _w (const char* word, size_t /*length*/)
{
    return Word{word};
}
void test_context_node(){
    using namespace rnn::simple_model;
    rnn::context_model::Param param{};
    auto param_rnn1 = randomParam(0.05);
    param_rnn1.bias.span *= rnn::type::float_t{0.0};
    VocaInfo rnn{"data.h5", "1b.model.voca", "1b.model", util::DataType::sp};
    copy(param_rnn1, param);
    auto rnn_greedy_score = test_rnn_greedy_score(param_rnn1, rnn);
    auto rnn_dp_score = test_rnn_dp_score(param_rnn1, rnn);

    Voca voca =load_voca("data.h5", "1b.model.voca");
    auto voca_vecs = load_voca_vecs<100>("data.h5", "1b.model", util::DataType::sp);
    std::cerr << voca_vecs.size() << " " << voca.size() <<std::endl;
    VocaIndexMap word2idx = voca.indexing();

    std::string sentence = u8"A symbol of British pound is £ .";
    auto idxs = word2idx.getIndex(sentence);
//    auto word_block = voca_vecs.getWordVec(idxs);

    {
        auto leaf_nodes = construct_nodes_with_reserve(voca, voca_vecs, idxs);
        auto nodes = InitializedNodes(std::move(leaf_nodes));
        assert(nodes.val.size() == 8);
        //    assert(nodes.val[0].prop.right_ctxs[0]==&nodes.val[1]);
        //    assert(nodes.val[0].prop.left_ctxs[0]== nullptr);

        util::Timer timer{};
        auto score = get_full_greedy_score(param, nodes);
        timer.here_then_reset("CRNN greedy forward path.");
        fmt::print("CRNN score : {}\n", score);
        for (auto &node: nodes.val) print_cnode(node);
        fmt::print("\n");
    }
    {
        auto leaf_nodes = construct_nodes_with_reserve(voca, voca_vecs, idxs);
        auto nodes = InitializedNodes(std::move(leaf_nodes));
        util::Timer timer{};

        DPtable table{nodes.val};
        table.compute(param);
        auto phrases = table.get_phrases();
        timer.here_then_reset("CRNN DP Forward path.");
        auto score_dp{0.0};
        for(auto phrase : phrases) score_dp += phrase->prop.score;
        fmt::print("{}:CRNN DP score. {} : score_sum.\n", score_dp, table.score_sum(0,7));
        auto root_node = table.root_node();
        print_all_descents(root_node);
    }
}

}//namespace rnn::context_model::test
}//namespace rnn::context_model
}//namespace rnn
