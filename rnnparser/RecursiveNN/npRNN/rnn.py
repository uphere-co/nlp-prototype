# -*- coding: utf-8 -*-
import numpy as np

from plain import *
from param import Param
from node import RNNnode
from tree_utils import Node, NodeTree

float_type = np.float32
int_type = np.int64

class Parser(object):
    def __init__(self, activation_f, activation_df):
        self.activation_f = activation_f
        self.activation_df= activation_df

    def forward(self, words_vec, param):
        n_word = len(words_vec)
        n_phrase = n_word - 1
        W    = np.tile(param.W,(n_phrase,1,1))
        bias = np.tile(param.bias,(n_phrase,1))
        u_score = param.u_score
        return self.forward_separted_param(words_vec, W,bias,u_score)
    def forward_separted_param(self, words_vec, W,bias,u_score):
        n_word = len(words_vec)
        n_phrase = n_word - 1
        n_iter= n_phrase
        merge_history=[]
        phrases=np.empty((n_phrase,words_vec.shape[1]))
        words = words_vec
        scores = np.empty(n_iter)
        for i in range(n_iter):
            wordLRs = np.concatenate([words[:-1],words[1:]], axis=1)
            xs = hidden_vectorized(W[i],bias[i],wordLRs)
            hs = self.activation_f(xs)
            hs_scores = scoring(u_score, hs)
            loc=np.argmax(hs_scores)
            words = np.concatenate([words[:loc+1], words[loc+2:]],axis=0)
            words[loc]=hs[loc]
            merge_history.append(loc)
            phrases[i]=hs[loc]
            scores[i]=hs_scores[loc]

        whole_words = np.concatenate([words_vec, phrases],axis=0)
        return merge_history, scores, whole_words

    def backward(self, phrases, param):
        grad   =param.zero()
        grad.W =self.backward_W(phrases, param)
        grad.bias =self.backward_b(phrases, param)
        grad.u_score =self.backward_u(phrases, param)
        return grad

    def backward_W_partial(self, node, param, n_phrase):
        left_factor,W,b, =param.u_score, param.W, param.bias
        grads=np.zeros((n_phrase,)+W.shape)
        back_propagation(node, left_factor,W,b, grads, is_W=True)
        return np.sum(grads,0)
    def backward_W(self, phrases, param):
        grad = np.zeros(param.W.shape)
        for node in phrases:
            grad +=self.backward_W_partial(node, param, len(phrases))
        return grad
    def backward_b_partial(self, node, param, n_phrase):
        left_factor,W,b, =param.u_score, param.W, param.bias
        grads=np.zeros((n_phrase,)+b.shape)
        back_propagation(node, left_factor,W,b, grads, is_W=False)
        return np.sum(grads,0)
    def backward_b(self, phrases, param):
        grad = np.zeros(param.bias.shape)
        for node in phrases:
            grad +=self.backward_b_partial(node, param, len(phrases))
        return grad
    def backward_u(self, phrases, param):
        grad = np.zeros(param.u_score.shape)
        for node in phrases:
            grad += node.vec
        return grad
    def merge_words(self, words,wordvec, param):
        merge_history,_, wordvecs=self.forward(wordvec, param)
        leaf_nodes=[RNNnode(word) for word in words]
        nodes, _=NodeTree.directed_merge(leaf_nodes,merge_history)
        RNNnode.set_value_views(nodes, wordvecs)
        phrases=nodes[len(words):]
        return phrases
