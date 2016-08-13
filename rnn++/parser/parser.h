#pragma once

#include "parser/param.h"
#include "parser/compute.h"
#include "parser/node.h"
#include "parser/voca.h" // for TrainData
#include "parser/wordvec.h" // for TrainData

#include "utils/linear_algebra.h"
#include "utils/loop_gen.h"


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

struct TrainData{
    TrainData() : voca{rnn::wordrep::load_voca()}, 
                  word2idx{voca.indexing()},
                  voca_vecs{rnn::wordrep::load_voca_vecs()} {}
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

// score(W_left, W_right, bias, u)= score_1(W_left, W_right, bias, u) 
//                                  + score_2(W_left, W_right, bias, u)
//                                  + .. 
//                                  + score_(n-1)
// score_1 = f(A*f(A*f(...)+b)+b)
Param get_gradient(Param const &param, InializedLeafNodes &nodes );

}//namespace rnn::simple_model
}//namespace rnn
