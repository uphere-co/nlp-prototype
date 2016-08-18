#pragma once

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

struct InializedLeafNodes{
    InializedLeafNodes(UninializedLeafNodes &&nodes,
                       rnn::wordrep::WordBlock const &word_block) : val{std::move(nodes.val)} {
        //TODO: following is inefficient. Make a separated class, LeafNode?? Or reuse word_block        
        for(decltype(val.size())i=0; i<val.size(); ++i){
            val[i].vec=Param::vec_type{word_block[i]};
        }
    }
    std::vector<tree::Node> val;
};

struct VocaInfo{
    VocaInfo(std::string vocafile, std::string voca_dataset, 
             std::string w2vmodel_dataset, int word_dim)
    : voca{rnn::wordrep::load_voca(vocafile, voca_dataset)}, word2idx{voca.indexing()},
      voca_vecs{rnn::wordrep::load_voca_vecs(vocafile,w2vmodel_dataset,word_dim,util::DataType::sp)} {}
    InializedLeafNodes initialize_tree(std::string sentence) const {
        auto idxs = word2idx.getIndex(sentence);
        auto word_block = voca_vecs.getWordVec(idxs);
        auto words = util::string::split(sentence);    
        auto nodes = construct_nodes_with_reserve(words);
        return InializedLeafNodes{std::move(nodes), word_block};
    }
    rnn::wordrep::Voca voca;
    rnn::wordrep::VocaIndexMap word2idx;
    rnn::wordrep::WordBlock voca_vecs;
};

struct TokenizedSentences{
    using c_iter = std::vector<std::string>::const_iterator;
    TokenizedSentences(std::string tokenized_file);
    std::vector<std::string> val;
};
Param::value_type get_full_score(Param const &param, InializedLeafNodes &nodes);
Param::value_type scoring_dataset(VocaInfo const &rnn, Param const &param, 
                                  TokenizedSentences const &dataset);

template<typename T>
Param::value_type scoring_minibatch(VocaInfo const &rnn, Param const &param, 
                                    T beg, T end){
    using rnn::type::float_t;
    auto get_score=[&rnn,&param](auto sentence){
        auto nodes = rnn.initialize_tree(sentence);
        return get_full_score(param, nodes);
    };
    auto score_accum = util::parallel_reducer(beg, end, get_score, float_t{});
    return score_accum;
}

// score(W_left, W_right, bias, u)= score_1(W_left, W_right, bias, u) 
//                                  + score_2(W_left, W_right, bias, u)
//                                  + .. 
//                                  + score_(n-1)
// score_1 = f(A*f(A*f(...)+b)+b)
Param get_gradient(Param const &param, InializedLeafNodes &nodes );


}//namespace rnn::simple_model
}//namespace rnn
