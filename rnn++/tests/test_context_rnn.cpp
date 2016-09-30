#include "tests/test_context_rnn.h"

#include <random>
#include <sstream>

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
#include "models/crnn.h"


namespace rnn{
constexpr int len_context=2;
using Node = rnn::detail::Node<rnn::model::crnn::Context<rnn::type::float_t, rnn::config::word_dim,len_context>>;
using Param= rnn::model::crnn::Param<rnn::type::float_t,rnn::config::word_dim,len_context>;
using InitializedNodes = rnn::detail::InitializedNodes<Node>;

struct VocaInfo{
    using node_t = Node;
    using voca_t = rnn::wordrep::Voca;
    using voca_idx_map_t = rnn::wordrep::VocaIndexMap;
    using voca_vecs_t = rnn::wordrep::WordBlock_base<node_t::prop_t::word_dim>;

    VocaInfo(std::string vocafile, std::string voca_dataset,
             std::string w2vmodel_dataset, util::DataType float_type)
    : voca{rnn::wordrep::load_voca(vocafile, voca_dataset)}, word2idx{voca.indexing()},
      voca_vecs{rnn::wordrep::load_voca_vecs<voca_vecs_t::dim>(vocafile,w2vmodel_dataset,float_type)} {}
    InitializedNodes initialize_tree(std::string sentence) const{
        auto idxs = word2idx.getIndex(sentence);
        std::vector<node_t> val;
        val.reserve(idxs.size()*2-1);
        for(auto idx : idxs){
            node_t::prop_t prop{voca.getWordSpan(idx), voca_vecs[idx]};
            node_t node{std::move(prop)};
            val.push_back(node);
        }

        util::span_dyn<node_t> nodes{val};
        auto len_context = node_t::prop_t::len_context;
        auto n=nodes.length();
        assert(val.data() == nodes.data());
        for(decltype(n)i=0; i!=n; ++i){
            auto left_beg=i>len_context?i-len_context:0;
            auto right_end=i+len_context+1<n?i+len_context+1:n;
            auto lefts = nodes.subspan(left_beg,i-left_beg);
            auto rights= nodes.subspan(i+1,right_end-(i+1));
            nodes[i].prop.set_context(lefts, rights);
        }
        return InitializedNodes{std::move(val)};
    }
    voca_t voca;
    voca_idx_map_t word2idx;
    voca_vecs_t voca_vecs;
};


void print(util::cstring_span<> word){
    for (auto e:word)
        fmt::print("{}", e);
    fmt::print(" ");
}
void print_cnode(Node const &cnode) {
    for(auto left : cnode.prop.left_ctxs) if(left) print(left->prop.name);
    fmt::print(" __ ");
    print(cnode.prop.name);
    fmt::print(" __ ");
    for(auto right : cnode.prop.right_ctxs) if(right) print(right->prop.name);
    fmt::print(" , {}\n", cnode.prop.score);
}

auto activation_fun=[](int64_t i, auto const &x) {
    return util::math::Fun<rnn::config::activation>(x[i]);
};
auto apply_activation_fun=[](int64_t i, auto &out, auto const &x) {
    out[i] = activation_fun(i,x);;
};
auto activation_dfun=[](int64_t i, auto const &x) {
    return util::math::Fun<rnn::config::activation_df>(x[i]);
};

auto update_mesg_common_part=[](int64_t i, auto &mesg, auto const &weighted_sum) {
    mesg[i]*=activation_dfun(i, weighted_sum);
};

auto back_prop_grad_W=[](int64_t i,int64_t j, auto &grad,
                         auto const &mesg, auto const &weighted_sum)  {
    grad[i][j]+=mesg[i]*weighted_sum[j];
};
auto back_prop_grad_word=[](int64_t i,int64_t j, auto &grad,
                            auto const &mesg, auto const &w)  {
    grad[j]+=mesg[i]*w[i][j];
};

class CRNN{
public:
    using param_t = Param;
    using val_t   = param_t::val_t;
    using mesg_t  = Param::mesg_t;
    using node_t  = Node;
    using prop_t  = Node::prop_t;

    static constexpr auto dim = param_t::dim;

    static void update_message(mesg_t &mesg, node_t const &phrase){
        util::math::VecLoop_void<val_t,dim> vecloop_void{};
        vecloop_void(update_mesg_common_part, mesg.span, phrase.prop.vec_wsum);
    }
    static void accum_param_gradient(param_t &grad_sum, mesg_t const &mesg, node_t const &phrase){
        util::math::MatLoop_void<val_t,dim,dim> matloop_void{};

        grad_sum.bias += mesg.span;
        matloop_void(back_prop_grad_W, grad_sum.w_left, mesg.span, phrase.left->prop.vec);
        matloop_void(back_prop_grad_W, grad_sum.w_right, mesg.span, phrase.right->prop.vec);

        auto lenctx = prop_t::len_context;
        for(decltype(lenctx)i=0; i!= lenctx; ++i) {
            auto w_left_ctx=grad_sum.w_context_left[i];
            auto left_ctx = phrase.prop.left_ctxs[i];
            auto w_right_ctx=grad_sum.w_context_right[i];
            auto right_ctx = phrase.prop.right_ctxs[i];
            if(left_ctx!= nullptr) matloop_void(back_prop_grad_W, w_left_ctx,  mesg.span, left_ctx->prop.vec);
            if(right_ctx!= nullptr) matloop_void(back_prop_grad_W, w_right_ctx,  mesg.span, right_ctx->prop.vec);
        }
    }
    static void accum_left_wordvec_gradient(node_t const &phrase, mesg_t const &mesg, param_t const &param){
        util::math::MatLoop_void<val_t,dim,dim> matloop_void{};
        matloop_void(back_prop_grad_word, phrase.left->prop.vec_update, mesg.span, param.w_left);
    }
    static void accum_right_wordvec_gradient(node_t const &phrase, mesg_t const &mesg, param_t const &param){
        util::math::MatLoop_void<val_t,dim,dim> matloop_void{};
        matloop_void(back_prop_grad_word, phrase.right->prop.vec_update, mesg.span, param.w_right);
    }
};


auto grad_u_score_L2norm_i=[](int64_t i, auto &grad, auto factor_u, auto const &u_score,
                              auto factor_p, auto const &phrase)  {
    grad[i] += u_score[i]*factor_u + phrase[i]*factor_p;
};
auto grad_u_score_L2norm = [](auto &grad, auto const &u_score, auto const &phrase){
    using namespace util::math;
    constexpr auto dim = Param::dim;
    using val_t = Param::val_t;
    auto norm = norm_L2(u_score);
    auto score = dot(u_score, phrase);
    auto vecloop_void = VecLoop_void<val_t,dim>{};

    vecloop_void(grad_u_score_L2norm_i, grad,
                 -score/(norm*norm*norm), u_score,
                 val_t{1}/norm, phrase);
};
void backward_path(Param &grad, Param const &param, Node const &phrase){
    grad_u_score_L2norm(grad.u_score, param.u_score, phrase.prop.vec);
    auto factor = Param::val_t{1}/util::math::norm_L2(param.u_score);
    assert(factor==factor);
    Param::mesg_t mesg{param.u_score};
    mesg *=factor;
    detail::backward_path_detail<CRNN>(param, grad, phrase, mesg);
}

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
    vecloop_void(apply_activation_fun, self.vec, self.vec_wsum);
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

auto get_merge_history = [](auto const &parsed_sentence){
    auto tree = util::deserialize_binary_tree<util::Node>(parsed_sentence);
    auto merge_history=util::reconstruct_merge_history(std::move(tree));
    return merge_history;
};

void directed_forward_path(Param const &param, std::vector<Node*> top_nodes,
                    std::vector<size_t> const &merge_history) {
    for(auto idx_max : merge_history){
        auto it_new  = top_nodes[idx_max];
        if(idx_max!=0){
            auto it_left = top_nodes[idx_max-1];
            it_left->right=it_new;
            update_node(param, *it_left);
        }
        if(idx_max!=top_nodes.size()-1){
            auto it_right= top_nodes[idx_max+1];
            it_right->left=it_new;
            update_node(param, *it_right);
        }
        std::copy(top_nodes.cbegin()+idx_max+1,top_nodes.cend(),
                  top_nodes.begin()+idx_max);
        top_nodes.pop_back();
    }
}

std::vector<Node*> compose_leaf_nodes(Param const &param, InitializedNodes &nodes)  {
    auto& leaves = nodes.val;
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

class DPtable{
public:
    using idx_t = std::size_t;
    using node_t = Node;
    using val_t= node_t::prop_t::val_t;

    DPtable(InitializedNodes const &nodes)
    : n_words{nodes.val.size()}, raw(n_words*n_words, node_t::blank_node()),
      score_sums(raw.size(), std::numeric_limits<val_t>::lowest()),
      penalties(0) {
        for(decltype(n_words)i=0; i<n_words; ++i){
            get(i,i)=nodes.val[i];
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
    return "("+parsed_binary_tree_string(*node.left) +" " + parsed_binary_tree_string(*node.right)+")";
}

void print_all_descents(Node const &node) {
    std::cerr<< parsed_binary_tree_string(node) << " " <<node.prop.score << std::endl;
    if(node.left != nullptr) print_all_descents(*node.left);
    if(node.right!= nullptr) print_all_descents(*node.right);
}

auto adaptive_update_vec = [](int64_t i, auto &out, auto x, auto const &grad, auto const &adagrad_factor){
    out[i] += x *grad[i]/std::sqrt(adagrad_factor[i]+0.0000001);
};
auto accum_rmsprop_factor_vec = [](int64_t i, auto &out, auto const &vec){
    out[i] *=0.98;
    out[i] += 0.02*vec[i]*vec[i];
};
class RMSprop{
public:
    using param_t = Param;
    using val_t = param_t::val_t;
    static const int dim_param=param_t::len_raw;
    RMSprop(val_t scale): ada_scale{scale} {}
    void update(Param &param, Param const &grad){
        vecloop_void(accum_rmsprop_factor_vec, ada_factor_param.span, grad.span);
        vecloop_void(adaptive_update_vec, param.span, ada_scale, grad.span ,  ada_factor_param.span);
    }

private:
    param_t ada_factor_param{};
    util::math::VecLoop_void<val_t,dim_param> vecloop_void{};
    val_t ada_scale;
};

}//namespace rnn

namespace {

void copy(rnn::simple_model::Param const &ori, rnn::Param &dest){
    std::copy(ori.w_left.span.cbegin(), ori.w_left.span.cend(), dest.w_left.begin());
    std::copy(ori.w_right.span.cbegin(),ori.w_right.span.cend(),dest.w_right.begin());
    std::copy(ori.bias.span.cbegin(),   ori.bias.span.cend(),   dest.bias.begin());
    std::copy(ori.u_score.span.cbegin(),ori.u_score.span.cend(),dest.u_score.begin());
}

auto test_rnn_greedy_score(rnn::simple_model::Param &param,
                     rnn::simple_model::VocaInfo &rnn){
    using namespace rnn::simple_model;
    using namespace rnn::simple_model::detail;
    using namespace util::math;

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

    rnn::simple_model::Param grad{};
    for(auto i=n_words; i<nodes.size(); ++i){
        auto const &node=nodes[i];
        assert(node.is_combined());
        // print_all_descents(node);
        backward_path(grad, param, node);
    }
    rnn::type::float_t ds_grad{};
    auto matloop_void=MatLoop_void<Param::value_type, Param::dim, Param::dim>{};
    matloop_void(mul_sum_mat, ds_grad, grad.w_left.span, param.w_left.span);
    matloop_void(mul_sum_mat, ds_grad, grad.w_right.span, param.w_right.span);
    ds_grad += dot(grad.bias.span, param.bias.span);
    ds_grad += dot(grad.u_score.span, param.u_score.span);
    fmt::print("{} : ds_grad\n", ds_grad);
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

    rnn::simple_model::Param grad{};
    for(auto node : phrases){
        assert(node->is_combined());
        backward_path(grad, param, *node);
    }
    Param::value_type ds_grad{};
    using namespace util::math;
    auto matloop_void=MatLoop_void<Param::value_type, Param::dim, Param::dim>{};
    matloop_void(mul_sum_mat, ds_grad, grad.w_left.span, param.w_left.span);
    matloop_void(mul_sum_mat, ds_grad, grad.w_right.span, param.w_right.span);
    ds_grad += dot(grad.bias.span, param.bias.span);
    ds_grad += dot(grad.u_score.span, param.u_score.span);
    fmt::print("{} : ds_grad\n", ds_grad);

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
namespace test{

using namespace rnn::wordrep;
using namespace rnn::config;

Word operator"" _w (const char* word, size_t /*length*/)
{
    return Word{word};
}

Param::val_t get_full_greedy_score(Param const &param, InitializedNodes &nodes ) {
    auto top_nodes = compose_leaf_nodes(param, nodes);
    auto merge_history = foward_path(param, top_nodes);
    Param::val_t score{};
    for(auto node: top_nodes) score+= node->prop.score;
    return score;
}

void test_context_node(){
    rnn::Param param{};
    rnn::Param param2{};
    auto param_rnn1 = rnn::simple_model::randomParam(0.01);
    auto param_rnn2 = rnn::simple_model::randomParam(0.01);
    param_rnn1.bias.span *= rnn::type::float_t{0.0};
    param_rnn2.bias.span *= rnn::type::float_t{0.0};
    copy(param_rnn1, param);
    copy(param_rnn2, param2);
    param += param2;
    param_rnn1 += param_rnn2;
    rnn::simple_model::VocaInfo rnn{"data.h5", "1b.model.voca", "1b.model", util::DataType::sp};
    auto rnn_greedy_score = test_rnn_greedy_score(param_rnn1, rnn);
    auto rnn_dp_score = test_rnn_dp_score(param_rnn1, rnn);

    VocaInfo crnn{"data.h5", "1b.model.voca", "1b.model", util::DataType::sp};

    std::string sentence = u8"A symbol of British pound is £ .";
    {
        auto nodes = crnn.initialize_tree(sentence);
        assert(nodes.val.size() == 8);
//        assert(nodes.val[0].prop.right_ctxs[0]==&nodes.val[1]);
//        assert(nodes.val[0].prop.left_ctxs[0]== nullptr);
//        assert(nodes.val[2].prop.left_ctxs[0]==&nodes.val[0]);
//        assert(nodes.val[2].prop.left_ctxs[1]==&nodes.val[1]);

        util::Timer timer{};
        auto score = get_full_greedy_score(param, nodes);
        timer.here_then_reset("CRNN greedy forward path.");
        fmt::print("CRNN score : {}\n", score);
        for (auto &node: nodes.val) print_cnode(node);
        fmt::print("\n");
        Param grad{};
        for(auto const &node : nodes.val){
            if(!node.is_combined()) continue;
            backward_path(grad, param, node);
        }
        auto ds_grad = util::math::dot(grad.span, param.span);
        fmt::print("{} : ds_grad\n", ds_grad);
    }
    {
        auto nodes = crnn.initialize_tree(sentence);
        util::Timer timer{};

        DPtable table{nodes};
        table.compute(param);
        auto phrases = table.get_phrases();
        timer.here_then_reset("CRNN DP Forward path.");
        auto score_dp{0.0};
        for(auto phrase : phrases) score_dp += phrase->prop.score;
        fmt::print("{}:CRNN DP score. {} : score_sum.\n", score_dp, table.score_sum(0,7));
        auto root_node = table.root_node();
        print_all_descents(root_node);

        Param grad{};
        for(auto node : phrases){
            assert(node->is_combined());
            backward_path(grad, param, *node);
        }
        auto ds_grad = util::math::dot(grad.span, param.span);
        fmt::print("{} : ds_grad\n", ds_grad);
    }
}

void test_crnn_backward() {
    auto param = Param::random(0.05);
    auto dParam = Param::random(0.001);
    auto param1 = param+dParam;

    VocaInfo crnn{"data.h5", "1b.model.voca", "1b.model", util::DataType::sp};

    std::string sentence = u8"A symbol of British pound is £ .";

    auto nodes = crnn.initialize_tree(sentence);
    util::Timer timer{};

    DPtable table{nodes};
    table.compute(param);
    auto phrases = table.get_phrases();
    timer.here_then_reset("CRNN DP Forward path.");
    auto score_dp{0.0};
    for(auto phrase : phrases) score_dp += phrase->prop.score;
    fmt::print("{}:CRNN DP score. {} : score_sum.\n", score_dp, table.score_sum(0,7));

    Param grad{};
    for(auto node : phrases){
        assert(node->is_combined());
        backward_path(grad, param, *node);
    }
    auto ds_grad = util::math::dot(grad.span, dParam.span);
    fmt::print("{} : ds_grad\n", ds_grad);
    fmt::print("{} : expected score\n", ds_grad+score_dp);

    {
        DPtable table{nodes};
        table.compute(param1);
        auto phrases = table.get_phrases();
        auto score_dp{0.0};
        for(auto phrase : phrases) score_dp += phrase->prop.score;
        fmt::print("{}: CRNN DP score with param1. {} : score_sum.\n", score_dp, table.score_sum(0,7));
    }
}

Param get_dp_gradient(VocaInfo const &crnn, Param const &param, Param::val_t lambda,
                     SentencePair const &sent_pair){
    auto nodes = crnn.initialize_tree(sent_pair.original);
    DPtable table{nodes};
    table.compute(param, lambda, sent_pair.parsed);
    auto phrases = table.get_phrases();
    Param grad{};
    for(auto node : phrases){
        assert(node->is_combined());
        backward_path(grad, param, *node);
    }
    return grad;
}

//struct ParsedNodes{
//    ParsedNodes(InitializedNodes &&nodes)
//    : val{std::move(nodes.val)}{
//        auto top_nodes = compose_leaf_nodes(param, nodes);
//        auto merge_history = get_merge_history(sent_pair.parsed);
//        directed_forward_path(param, top_nodes, merge_history);
//    }
//    std::vector<Node> val;
//};
Param get_directed_grad(VocaInfo const &crnn, Param const &param,
                        SentencePair const &sent_pair){
    auto nodes = crnn.initialize_tree(sent_pair.original);
    auto top_nodes = compose_leaf_nodes(param, nodes);
    auto merge_history = get_merge_history(sent_pair.parsed);
    directed_forward_path(param, top_nodes, merge_history);
    Param grad{};
    for(auto node : top_nodes){
        assert(node->is_combined());
        backward_path(grad, param, *node);
    }
    return grad;
}

Param::val_t parsed_scoring_sentence(VocaInfo const &rnn, Param const &param,
                                     SentencePair const &sent_pair){
    auto parsed_sentence=sent_pair.parsed;
    auto merge_history = util::get_merge_history(parsed_sentence);
    auto nodes = rnn.initialize_tree(sent_pair.original);
    auto top_nodes = compose_leaf_nodes(param, nodes);
    directed_forward_path(param, top_nodes, merge_history);
    auto score{0.0};
    for(auto phrase : top_nodes) score += phrase->prop.score;
    return score;
}
Param::val_t parsed_scoring_dataset(VocaInfo const &rnn, Param const &param,
                                    SentencePairs const &dataset){
    auto &lines = dataset.val;
    auto get_score=[&](auto const &sent_pair){
        return parsed_scoring_sentence(rnn, param, sent_pair);
    };
    auto score_accum = util::parallel_reducer(lines.cbegin(), lines.cend(), get_score, Param::val_t{});
    return score_accum;
}
Param::val_t dp_scoring_sentence(VocaInfo const &rnn, Param const &param,
                                 DPtable::val_t lambda, SentencePair const &sent_pair) {
    auto nodes = rnn.initialize_tree(sent_pair.original);
    DPtable table{nodes};
    table.compute(param, lambda, sent_pair.parsed);
    auto phrases = table.get_phrases();
    auto score_dp{0.0};
    for(auto phrase : phrases) score_dp += phrase->prop.score;
    return score_dp;
}
Param::val_t dp_scoring_dataset(VocaInfo const &rnn, Param const &param,
                                DPtable::val_t lambda, SentencePairs const &dataset){
    auto &lines = dataset.val;
    auto get_score=[&rnn,&param,&lambda](auto sent_pair){
        return dp_scoring_sentence(rnn, param, lambda, sent_pair);
    };
    auto score_accum = util::parallel_reducer(lines.cbegin(), lines.cend(), get_score, Param::val_t{});
    return score_accum;
}


void test_crnn_directed_backward() {
    auto param = Param::random(0.05);
    auto dParam = Param::random(0.001);
    auto param1 = param+dParam;

    VocaInfo crnn{"data.h5", "1b.model.voca", "1b.model", util::datatype_from_string("float32")};

    auto sentence_orig = u8"A symbol of British pound is £ .";
    auto sentence_parsed = u8"(((((A symbol) of) (British pound)) (is £)) .)";
    auto merge_history = get_merge_history(sentence_parsed);

    auto nodes = crnn.initialize_tree(sentence_orig);
    auto top_nodes = compose_leaf_nodes(param, nodes);
    directed_forward_path(param, top_nodes, merge_history);
    auto score_label{0.0};
    for(auto phrase : top_nodes) score_label += phrase->prop.score;
    fmt::print("{}:CRNN label score.\n", score_label);

    Param grad{};
    for(auto node : top_nodes){
        assert(node->is_combined());
        backward_path(grad, param, *node);
    }
    auto ds_grad = util::math::dot(grad.span, dParam.span);
    fmt::print("{} : ds_grad\n", ds_grad);
    fmt::print("{} : expected score\n", ds_grad+score_label);

    {
        auto nodes = crnn.initialize_tree(sentence_orig);
        auto top_nodes = compose_leaf_nodes(param1, nodes);
        directed_forward_path(param1, top_nodes, merge_history);
        auto score_label{0.0};
        for(auto phrase : top_nodes) score_label += phrase->prop.score;
        fmt::print("{}:CRNN label score with param1.\n", score_label);
    }

}


void test_grad_parallel_reduce(){
    VocaInfo crnn{"data.h5", "1b.model.voca", "1b.model", util::datatype_from_string("float32")};
    auto testset_parsed=ParsedSentences{"a.sample.stanford"};
    auto testset_orig=TokenizedSentences{"a.sample"};
    rnn::simple_model::VocaInfo rnn{"data.h5", "1b.model.voca", "1b.model", util::datatype_from_string("float32")};
//    rnn::simple_model::VocaInfo rnn{"news_wsj.h5", "news_wsj.voca", "news_wsj", util::datatype_from_string("float64")};
//    VocaInfo crnn{"news_wsj.h5", "news_wsj.voca", "news_wsj", util::datatype_from_string("float64")};
//    auto testset_parsed=ParsedSentences{"news_wsj.s2010.test.stanford"};
//    auto testset_orig=TokenizedSentences{"news_wsj.s2010.test"};
    auto testset = SentencePairs{testset_parsed,testset_orig};
    auto beg=testset.val.cbegin();
    auto end=testset.val.cend();

    auto param_rnn = rnn::simple_model::randomParam(0.05);
    {
        auto get_label_grad=[&](auto const &sent_pair){
            return rnn::simple_model::get_directed_grad(rnn, param_rnn, sent_pair);
        };
        auto gradient_rnn = util::parallel_reducer(beg, end, get_label_grad, rnn::simple_model::Gradient{});
        auto grad_rnn = gradient_rnn.param;
        fmt::print("{} : RNN\n", util::math::sum(grad_rnn.bias.span)+util::math::sum(grad_rnn.u_score.span)
        +util::math::sum(grad_rnn.w_left.span)+util::math::sum(grad_rnn.w_right.span));
    }

//    auto param = Param::random(0.05);
//    param.bias *= 0.0;
    Param param{};
    copy(param_rnn, param);

    auto get_label_grad=[&](auto const &sent_pair){
        return get_directed_grad(crnn, param, sent_pair);
    };

    auto grad0 = util::parallel_reducer(beg, end, get_label_grad, Param{});
    auto n = testset.val.size();
    std::vector<Param> grads(n);
    tbb::parallel_for(decltype(n){0}, n, [&](auto i) {
        grads[i]=get_label_grad(testset.val[i]);
    });
    Param grad1{};
    for(auto &grad_i : grads) grad1+=grad_i;

    std::vector<Param> grads2(n);
    for(decltype(n) i=0; i!=n; ++i){
        grads2[i]=get_label_grad(testset.val[i]);
    };
    Param grad2{};
    for(auto &grad_i : grads2) grad2+=grad_i;

    Param grad_serial{};
    for(auto &sent_pair:testset.val) grad_serial+=get_label_grad(sent_pair);
    fmt::print("{} vs {} vs {} vs {}\n", util::math::sum(grad0.span), util::math::sum(grad1.span),
               util::math::sum(grad2.span), util::math::sum(grad_serial.span));
    assert(grad1.span == grad_serial.span);
}

void test_minibatch_crnn(){
    VocaInfo rnn{"news_wsj.h5", "news_wsj.voca", "news_wsj", util::datatype_from_string("float64")};
    auto testset_parsed=ParsedSentences{"news_wsj.s2010.test.stanford"};
    auto testset_orig=TokenizedSentences{"news_wsj.s2010.test"};
    auto testset = SentencePairs{testset_parsed,testset_orig};

    auto lambda=0.05;
    auto param = Param::random(0.05);
    param.bias *= 0.0;
    auto dParam = Param::random(0.001);

    auto get_label_grad=[&](auto const &sent_pair){
        return get_directed_grad(rnn, param, sent_pair);
    };
    auto get_dp_grad=[&](auto const &sent_pair){
        return get_dp_gradient(rnn, param, lambda, sent_pair);
    };
    auto score_diff=[&](){
        auto score_label = parsed_scoring_dataset(rnn, param, testset);
        auto score_dp= dp_scoring_dataset(rnn, param, lambda, testset);
        return score_label-score_dp;
    };


    auto beg=testset.val.cbegin();
    auto end=testset.val.cend();

    util::Timer timer{};
    {
    auto grad_label = util::parallel_reducer(beg, end, get_label_grad, Param{});
    timer.here_then_reset("CRNN label gradient.");
    auto param1{param}; param1 += dParam;
    auto param2{param}; param2 -= dParam;
    auto score_label1 = parsed_scoring_dataset(rnn, param1, testset);
    auto score_label2 = parsed_scoring_dataset(rnn, param2, testset);
    auto ds_score = 2*util::math::dot(grad_label.span, dParam.span);
    fmt::print("Label Score difference : {} vs {} = {} - {}\n",
               ds_score, score_label1-score_label2, score_label1, score_label2);}


    {
    auto grad_dp = util::parallel_reducer(beg, end, get_dp_grad, Param{});
    timer.here_then_reset("CRNN DP gradient.");
    auto param1{param}; param1 += dParam;
    auto param2{param}; param2 -= dParam;
    auto score_label1 = dp_scoring_dataset(rnn, param1, lambda, testset);
    auto score_label2 = dp_scoring_dataset(rnn, param2, lambda, testset);
    auto ds_score = 2*util::math::dot(grad_dp.span, dParam.span);
    fmt::print("DP Score difference : {} vs {} = {} - {}\n",
               ds_score, score_label1-score_label2, score_label1, score_label2);}

}


void write_to_disk(Param const &param, std::string param_name){
    using namespace util::io;
    auto param_raw = param.serialize();
//    H5file h5store{H5name{"crnn_params.h5"}, hdf5::FileMode::create};
    H5file h5store{H5name{"crnn_params.h5"}, hdf5::FileMode::rw_exist};
    h5store.writeRawData(H5name{param_name}, param_raw);
}

void train_crnn(nlohmann::json const &config){
    Logger logger{"crnn", "logs/basic.txt"};
    auto write_param=[&logger](auto i_minibatch, auto const &param){
        std::stringstream ss;
        ss << "crnn." << logger.uid_str() <<"."<<i_minibatch;
        write_to_disk(param, ss.str());
    };

    auto n_minibatch=rnn::config::n_minibatch;
    auto testset_parsed=ParsedSentences{util::get_string_val(config,"testset_parsed")};
    auto testset_orig=TokenizedSentences{util::get_string_val(config,"testset")};
    auto trainset_parsed=ParsedSentences{util::get_string_val(config,"trainset_parsed")};
    auto trainset_orig=TokenizedSentences{util::get_string_val(config,"trainset")};
    auto testset = SentencePairs{testset_parsed,testset_orig};
    auto trainset = SentencePairs{trainset_parsed,trainset_orig};

    VocaInfo rnn{config["wordvec_store"], config["voca_name"], config["w2vmodel_name"],
                 util::datatype_from_string(config["float_t"])};
    auto lambda=0.05;
    auto param = Param::random(0.05);
    param.bias *= 0.0;

    auto get_label_grad=[&](auto const &sent_pair){
        return get_directed_grad(rnn, param, sent_pair);
    };
    auto get_dp_grad=[&](auto const &sent_pair){
        return get_dp_gradient(rnn, param, lambda, sent_pair);
    };
    auto score_diff=[&](){
        auto score_label = parsed_scoring_dataset(rnn, param, testset);
        auto score_dp= dp_scoring_dataset(rnn, param, lambda, testset);
        return score_label-score_dp;
    };


    logger.info("Prepared data.");

    logger.info("Begin training");
    int64_t i_minibatch{};
    logger.log_testscore(i_minibatch, score_diff());
    fmt::print("{} :voca size.\n", rnn.voca_vecs.size());
    write_param(i_minibatch,param);

    RMSprop optimizer{0.001};
    auto &pairs = trainset.val;
    for(auto epoch=0; epoch<n_epoch; ++epoch){
        for(auto it=pairs.cbegin();it <pairs.cend(); it+= n_minibatch){
            auto beg=it;
            auto end=beg+n_minibatch;
            end=end<pairs.cend()?end:pairs.cend();

            auto grad_label = util::parallel_reducer(beg, end, get_label_grad, Param{});
            optimizer.update(param, grad_label);

            auto grad_dp = util::parallel_reducer(beg, end, get_dp_grad, Param{});
            grad_dp *=-1.0;
            optimizer.update(param, grad_dp);

            ++i_minibatch;
            if(i_minibatch%100==0) {
                logger.log_testscore(i_minibatch,score_diff());
                write_param(i_minibatch,param);
            }
        }
    }
    logger.info("Finish one iteration");
}

Param load_param(std::string const &h5_name,
                 std::string const &param_name, util::DataType param_type) {
    using namespace util::io;
    H5file param_storage{H5name{h5_name}, hdf5::FileMode::read_exist};
    if(param_type == util::DataType::sp){
        return Param{param_storage.getRawData<double>(H5name{param_name})};
    } else if(param_type == util::DataType::dp){
        return Param{param_storage.getRawData<double>(H5name{param_name})};
    }
    assert(0);
}

void crnn_parser(char **argv){
    Logger logger{"crnn", "logs/basic.txt"};

    VocaInfo rnn{"news_wsj.h5", "news_wsj.voca", "news_wsj", util::datatype_from_string("float64")};

    auto param = load_param("crnn_params.h5", argv[1], util::DataType::dp);
//    auto param = Param::random(0.05);
//    param.bias *= 0.0;
    auto inputset =TokenizedSentences{argv[2]};
    auto n_sents=inputset.val.size();
    std::vector<std::string> parsed_sents(n_sents);

    tbb::parallel_for(decltype(n_sents){0}, n_sents, [&](auto i) {
        auto line=inputset.val[i];
        auto nodes = rnn.initialize_tree(line);
        DPtable table{nodes};
        table.compute(param);
        auto phrases = table.get_phrases();
        if(phrases.size()) {
            auto root_node=table.root_node();
            parsed_sents[i]=parsed_binary_tree_string(root_node);
        } else {
            //std::cout << line << std::endl;
            parsed_sents[i]=line;
        }
    });
    for(auto sent:parsed_sents)
        std::cout<<sent<<std::endl;
}

}//namespace rnn::test
}//namespace rnn

