#pragma once

#include <utility>
#include <unordered_map>

#include "dataset.h"
#include "parser/param.h"
#include "parser/compute.h"
#include "parser/node.h"
#include "parser/voca.h" // for TrainData
#include "parser/wordvec.h" // for TrainData

#include "utils/linear_algebra.h"
#include "utils/loop_gen.h"
#include "utils/parallel.h" //TODO: remove this by refactoring scoring_minibatch 

namespace rnn{
namespace simple_model{
using WordBlock= wordrep::WordBlock_base<rnn::config::word_dim>;
struct InializedLeafNodes{
    InializedLeafNodes(UninializedLeafNodes &&nodes,
                       WordBlock const &word_block) : val{std::move(nodes.val)} {
        //TODO: following is inefficient. Make a separated class, LeafNode?? Or reuse word_block        
        for(decltype(val.size())i=0; i<val.size(); ++i){
            val[i].vec=Param::vec_type{word_block[i]};
        }
    }
    std::vector<tree::Node> val;
};

struct VocaInfo{
    using voca_t = rnn::wordrep::Voca;
    using voca_idx_map_t = rnn::wordrep::VocaIndexMap;
    using voca_vecs_t = WordBlock;
    VocaInfo(std::string vocafile, std::string voca_dataset, 
             std::string w2vmodel_dataset, util::DataType float_type)
    : voca{rnn::wordrep::load_voca(vocafile, voca_dataset)}, word2idx{voca.indexing()},
      voca_vecs{rnn::wordrep::load_voca_vecs<rnn::config::word_dim>(vocafile,w2vmodel_dataset,float_type)} {}
    InializedLeafNodes initialize_tree(std::string sentence) const {
        auto idxs = word2idx.getIndex(sentence);
        auto word_block = voca_vecs.getWordVec(idxs);
        auto words = util::string::split(sentence);
        auto nodes = construct_nodes_with_reserve(words, idxs);
        return InializedLeafNodes{std::move(nodes), word_block};
    }
    voca_t voca;
    voca_idx_map_t word2idx;
    voca_vecs_t voca_vecs;
};

struct SparseGrad{
    using key_t = WordBlock::idx_t;
    using val_t = Param::vec_type;
    std::unordered_map<key_t,val_t> val;
};
SparseGrad& operator +=(SparseGrad& out, const SparseGrad& x);
SparseGrad& operator -=(SparseGrad& out, const SparseGrad& x);
SparseGrad& operator *=(SparseGrad& out, Param::value_type x);
SparseGrad operator +(const SparseGrad& x, const SparseGrad& y);
SparseGrad operator -(const SparseGrad& x, const SparseGrad& y);
SparseGrad operator *(const SparseGrad& x, Param::value_type v);

struct Gradient{
    Param param{};
    SparseGrad words{};
};
Gradient& operator +=(Gradient& out, const Gradient& x);
Gradient& operator -=(Gradient& out, const Gradient& x);
Gradient operator +(const Gradient& x, const Gradient& y);
Gradient operator -(const Gradient& x, const Gradient& y);



using detail::DPtable;
DPtable dp_merging(Param const &param, InializedLeafNodes &initialized_nodes);
DPtable dp_merging_with_penalty(Param const &param,
                                InializedLeafNodes &initialized_nodes,
                                DPtable::val_t lambda,
                                SentencePair const &sent_pair);
Param::value_type get_full_dp_score(Param const &param,
                                    InializedLeafNodes &initialized_nodes,
                                    DPtable::val_t lambda,
                                    SentencePair const &sent_pair);
Param::value_type dp_scoring_dataset(VocaInfo const &rnn, Param const &param, 
                                     rnn::type::float_t lambda, SentencePairs const &dataset);

Param::value_type get_full_greedy_score(Param const &param, InializedLeafNodes &nodes);
Param::value_type greedy_scoring_dataset(VocaInfo const &rnn, Param const &param, 
                                  TokenizedSentences const &dataset);

template<typename T>
Param::value_type greedy_scoring_minibatch(VocaInfo const &rnn, Param const &param, 
                                    T beg, T end){
    using rnn::type::float_t;
    auto get_score=[&rnn,&param](auto sentence){
        auto nodes = rnn.initialize_tree(sentence);
        return get_full_greedy_score(param, nodes);
    };
    auto score_accum = util::parallel_reducer(beg, end, get_score, float_t{});
    return score_accum;
}

Param::value_type scoring_parsed_sentence(VocaInfo const &rnn, Param const &param,
                                          SentencePair const &sent_pair);

Param::value_type scoring_parsed_dataset(VocaInfo const &rnn, Param const &param, 
                                         SentencePairs const &dataset);

// score(W_left, W_right, bias, u)= score_1(W_left, W_right, bias, u) 
//                                  + score_2(W_left, W_right, bias, u)
//                                  + .. 
//                                  + score_(n-1)
// score_1 = f(A*f(A*f(...)+b)+b)
Gradient get_greedy_gradient(Param const &param, InializedLeafNodes &nodes );
Gradient get_dp_gradient(Param const &param, rnn::type::float_t lambda,
                         InializedLeafNodes &init_nodes,
                         SentencePair const &sent_pair); 
Gradient get_directed_grad(VocaInfo const &rnn, Param const &param, 
                        SentencePair const &sent_pair);
    

}//namespace rnn::simple_model
}//namespace rnn
