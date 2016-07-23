# -*- coding: utf-8 -*-
import pytest
import numpy as np
import pandas as pd

from npRNN.tree_utils import Node, NodeTree
from npRNN.node import RNNnode
from npRNN.rnn import Parser
from npRNN.param import Param

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
    sentence='a name of the cat on a hat'
    input_words=sentence.split()
    words_vec=word2vec.ix[input_words].values.astype(np.float32)

    n_words = len(input_words)
    dim=words_vec.shape[1]

    scale=0.0001
    max_relative_error=0.03
    delta = Param.random(dim, n_words)
    delta.W*=scale
    delta.bias*= scale*dim
    delta.u_score*= scale*dim

    rnn=Parser(np.tanh, lambda x : np.cosh(x)**-2)
    param0 = Param.random(dim, n_words)
    merge_history,scores0, wordvecs=rnn.forward(words_vec, param0)
    leaf_nodes=[RNNnode(word) for word in input_words]
    nodes, _=NodeTree.directed_merge(leaf_nodes,merge_history)
    RNNnode.set_value_views(nodes, wordvecs)
    phrases=nodes[n_words:]

    param1 = param0.copy()
    param1.W += delta.W
    _,scores1, _ =rnn.forward(words_vec, param1)
    ds_exact = np.sum(scores1-scores0)
    grad =rnn.backward_W(phrases, param0)
    ds= np.sum(grad*delta.W)
    print ds, ds_exact
    np.testing.assert_allclose(ds, ds_exact, max_relative_error)

    param1 = param0.copy()
    param1.bias += delta.bias
    _,scores1, _ =rnn.forward(words_vec, param1)
    ds_exact = np.sum(scores1-scores0)
    grad =rnn.backward_b(phrases, param0)
    ds= np.sum(grad*delta.bias)
    print ds, ds_exact
    np.testing.assert_allclose(ds, ds_exact, max_relative_error)

    param1 = param0.copy()
    #param1.u_score += delta.u_score
    param1.u_score += delta.u_score
    _,scores1, _ =rnn.forward(words_vec, param1)
    ds_exact = np.sum(scores1-scores0)
    grad =rnn.backward_u(phrases, param0)
    ds= np.sum(grad*delta.u_score)
    print ds, ds_exact
    np.testing.assert_allclose(ds, ds_exact, max_relative_error)
