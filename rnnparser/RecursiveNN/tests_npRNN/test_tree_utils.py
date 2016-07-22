import pytest

from npRNN.tree_utils import Node, NodeTree
def test_merge_results():
    #sentence='I know a name of the cat on a hat'
    sentence='a name of the cat on a hat'
    words=[Node(word) for word in sentence.split()]

    tree=NodeTree(words, [0, 5, 3, 1, 2, 0, 0])
    assert tree.phrase.name =='(((a name) (of the)) ((cat on) (a hat)))'
    assert tree.phrase.depth==3
    assert tree.history == [0, 5, 3, 1, 2, 0, 0]

    tree=NodeTree(words, [0, 5, 0, 0, 1, 1, 0])
    assert tree.phrase.name =='((((a name) of) the) ((cat on) (a hat)))'
    assert tree.phrase.depth==4
    assert tree.history == [0, 5, 0, 0, 1, 1, 0]

    tree=NodeTree(words, [2,0,3,2,2,0,0])
    assert tree.phrase.name =='(((a name) (of the)) ((cat (on a)) hat))'
    assert tree.phrase.depth==4
    assert tree.history == [2,0,3,2,2,0,0]

def test_merge_dicrection():
    sentence='a name of the cat on a hat'
    words=[Node(word) for word in sentence.split()]
    merge_history=[3,1,1,0,2,1,0]
    all_nodes, _ =NodeTree.directed_merge(words,merge_history)
    print all_nodes
    composites=all_nodes[len(words):]
    print composites
    left_merged=NodeTree.get_merge_direction(composites)

    expected_left_merged = [[True, False, False, True],[True, True, False, True],\
        [True, False, True],[True, True],[True, False, False],[True, False],[True]]
    assert left_merged == expected_left_merged
    depths = [x.depth for x in composites]
    assert depths==[1, 1, 2, 3, 1, 2, 4]
