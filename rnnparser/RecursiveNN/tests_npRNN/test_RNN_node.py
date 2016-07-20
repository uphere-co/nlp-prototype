# -*- coding: utf-8 -*-
import pytest
import numpy as np
import pandas as pd

from npRNN.tree_utils import Node, NodeTree
from npRNN.node import RNNnode
from npRNN.rnn import Params, Parser
from npRNN.plain import back_propagation

def test_value_views():
    sentence='a name of the cat on a hat'
    merge_history=[5, 2, 2, 1, 0, 1, 0]
    expected_merged_nodes= '[a, name, of, the, cat, on, a, hat, (on a), (of the), ((of the) cat), (name ((of the) cat)), (a (name ((of the) cat))), ((on a) hat), ((a (name ((of the) cat))) ((on a) hat))]'

    leaf_words=sentence.split()
    leaf_nodes=[RNNnode(word) for word in leaf_words]
    nodes, _=NodeTree.directed_merge(leaf_nodes,merge_history)
    assert str(nodes)==expected_merged_nodes

    n_words = (len(nodes)+1)/2
    assert nodes[7].iteration is None
    assert nodes[n_words].iteration ==0
    assert nodes[-1].iteration==n_words-2

    wordvecs = np.zeros((len(leaf_words)*2-1, 100))
    RNNnode.set_value_views(nodes, wordvecs)
    for node,val in zip(nodes,wordvecs):
        assert np.all(node.vec==val)
    wordvecs[:]=1
    for node,val in zip(nodes,wordvecs):
        assert np.all(node.vec==val)
    nodes[3].vec=3
    assert np.all(wordvecs[:2]==1)
    assert np.all(wordvecs[3]==3)
    assert np.all(wordvecs[4:]==1)

def test_numercally_back_propagation():
    word2vec=pd.read_pickle('tests_npRNN/voca.pickle')
    ran=lambda x : np.random.random(x).astype(float_type)-0.5
    delta= lambda x ,scale: scale*ran(x.shape)
    float_type=np.float32
    sentence='a name of the cat on a hat'
    input_words=sentence.split()
    words_vec=word2vec.ix[input_words].values.astype(float_type)

    dim=words_vec.shape[1]

    W=ran((dim,dim*2))
    bias=ran(dim)
    u_score=ran(dim)
    params = Params(W,bias,u_score)

    scale=0.0001
    max_relative_error=0.02
    dW=delta(W,scale)
    db=delta(bias,scale)
    du=delta(u_score,scale)

    n_words = len(input_words)
    rnn=Parser(params, np.tanh, lambda x : np.cosh(x)**-2)
    merge_history,scores0, wordvecs=rnn.forward(words_vec)
    Ws = np.array([W.copy() for _ in range(n_words-1)])
    Ws += dW
    _,scores1, _ =rnn.forward(words_vec, Ws)
    ds_exact = np.sum(scores1-scores0)

    leaf_nodes=[RNNnode(word) for word in input_words]
    nodes, _=NodeTree.directed_merge(leaf_nodes,merge_history)
    RNNnode.set_value_views(nodes, wordvecs)
    phrases=nodes[n_words-1:]
    grad = np.zeros(W.shape)
    for node in phrases:
        gradWs = np.zeros((n_words-1,)+W.shape)
        back_propagation(node, params.u_score,params.W,params.bias, gradWs)
        grad += np.sum(gradWs,0)

    ds= np.sum(grad*dW)

    np.testing.assert_allclose(ds,ds_exact, rtol=scale*100)
