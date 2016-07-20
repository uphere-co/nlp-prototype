import pytest
import numpy as np
import pandas as pd

from npRNN.tree_utils import Node, NodeTree
from npRNN.rnn import Parser
from npRNN.node import RNNnode

def test_value_views():
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

    scale=0.0001
    max_relative_error=0.02
    dW=delta(W,scale)
    db=delta(bias,scale)
    du=delta(u_score,scale)

    rnn=Parser(u_score,W,bias, np.tanh, lambda x : np.cosh(x)**-2)

    merge_history,scores,phrases=rnn.forward(words_vec)
    whole_words = np.concatenate([words_vec, phrases])
    scores,merge_history

    vecs = np.concatenate([words_vec,phrases])
    #Concatenaton does copy:
    assert np.all(words_vec[1]==vecs[1])
    vecs[1]=1
    assert np.all(words_vec[1]!=vecs[1])

    vecs = np.concatenate([words_vec,phrases])
    nodes=[RNNnode(word) for word in input_words]
    whole_nodes, _=NodeTree.directed_merge(nodes,merge_history)
    RNNnode.set_value_views(whole_nodes, vecs)

    for node,val in zip(whole_nodes,vecs):
        assert np.all(node.vec==val)
    vecs[:]=1
    for node,val in zip(whole_nodes,vecs):
        assert np.all(node.vec==val)
    whole_nodes[3].vec=3
    assert np.all(vecs[3]==3)
