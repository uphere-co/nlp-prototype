import pytest

from npRNN.tree_utils import Vec, VecTree
def test_merge_results():
    #sentence='I know a name of the cat on a hat'
    sentence='a name of the cat on a hat'
    words=[Vec(word) for word in sentence.split()]

    tree=VecTree(words, [0, 5, 3, 1, 2, 0, 0])
    assert tree.phrase.name =='(((a name) (of the)) ((cat on) (a hat)))'
    assert tree.phrase.depth==3
    assert tree.history == [0, 5, 3, 1, 2, 0, 0]

    tree=VecTree(words, [0, 5, 0, 0, 1, 1, 0])
    assert tree.phrase.name =='((((a name) of) the) ((cat on) (a hat)))'
    assert tree.phrase.depth==4
    assert tree.history == [0, 5, 0, 0, 1, 1, 0]
