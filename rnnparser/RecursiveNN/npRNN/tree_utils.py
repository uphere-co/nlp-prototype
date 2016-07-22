# -*- coding: utf-8 -*-
import numpy as np

class Node(object):
    @classmethod
    def merge(cls,wordL, wordR):
        node = cls('(%s %s)'%(wordL.name,wordR.name))
        node.left = wordL
        node.left.parent=node
        node.right = wordR
        node.right.parent=node
        node.depth = max(wordL.depth, wordR.depth)+1
        return node
    def __init__(self, word):
        self.depth=0
        self.name=word
        self.left=None
        self.right=None
        self.parent=None
    def __unicode__(self):
        return self.name
    def __repr__(self):
        return self.__unicode__()
    def other_leaf(self, leaf):
        if leaf is self.left:
            return self.right
        return self.left
    def iter(self):
        if self.depth==0:
            return []
        return self.left.iter() + [self] + self.right.iter()
    def iter2(self):
        if self.left is not None:
            yield self.left.iter2()
        yield self
        if self.right is not None:
            yield self.right.iter2()

class NodeTree(object):
    @classmethod
    def random_merge(cls,word_nodes):
        node_type = type(word_nodes[0])
        words=list(word_nodes)
        words_in_tree=list(word_nodes)
        merge_history=[]
        while len(words)>1:
            idx_beg=np.random.choice(range(len(words)-1))
            merge_history.append(idx_beg)
            wordL,wordR=words[idx_beg],words[idx_beg+1]
            new_word = node_type.merge(wordL,wordR)
            words[idx_beg:idx_beg+2]= [new_word]
            words_in_tree.append(new_word)
        return words_in_tree, merge_history
    @classmethod
    def directed_merge(cls,word_nodes, merge_history):
        node_type = type(word_nodes[0])
        merge_history=list(merge_history)
        words=list(word_nodes)
        words_in_tree=list(word_nodes)
        for iteration, idx_beg in enumerate(merge_history):
            wordL,wordR=words[idx_beg],words[idx_beg+1]
            new_word = node_type.merge(wordL,wordR)
            new_word.iteration = iteration
            words[idx_beg:idx_beg+2]= [new_word]
            words_in_tree.append(new_word)
        return words_in_tree, merge_history
    @classmethod
    def get_merge_direction(cls,composites):
        left_merged=[[] for _ in composites]
        for i,node in enumerate(composites):
            left_merged[i].append(True)
            parent=node.parent
            while parent is not None:
                if parent.left is node:
                    left_merged[i].append(True)
                else:
                    left_merged[i].append(False)
                parent,node=parent.parent,parent
        return left_merged
    @classmethod
    def words_merged(cls, node):
        words = [node.left, node.right]
        while node.parent is not None:
            words.append(node.parent.other_leaf(node))
            node=node.parent
        return words
    def __init__(self, words, history=None):
        if history is None:
            new_words, merge_history = NodeTree.random_merge(words)
        else:
            new_words, merge_history = NodeTree.directed_merge(words, history)
        self.history=merge_history
        self.phrase=new_words[-1]
