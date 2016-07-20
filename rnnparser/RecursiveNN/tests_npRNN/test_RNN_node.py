# -*- coding: utf-8 -*-
import pytest
import numpy as np
import pandas as pd

from npRNN.tree_utils import Node, NodeTree
from npRNN.node import RNNnode

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
