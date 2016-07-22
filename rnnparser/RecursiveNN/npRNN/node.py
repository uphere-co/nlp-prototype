# -*- coding: utf-8 -*-
class RNNnode(object):
    @classmethod
    def propagateParent(cls, node, parent):
        if node is not None:
            node._add_parent(parent)
            cls.propagateParent(node.left, parent)
            cls.propagateParent(node.right, parent)
    @classmethod
    def merge(cls,nodeL, nodeR):
        node = cls('(%s %s)'%(nodeL.name,nodeR.name))
        node.left=nodeL
        node.right=nodeR
        cls.propagateParent(node.left, node)
        cls.propagateParent(node.right, node)
        return node
    @classmethod
    def set_value_views(cls, nodes, values):
        for node,val in zip(nodes,values):
            node._vec = val
    def __init__(self, name, vec=None):
        self.name = name
        self._vec = None
        self.iteration = None
        self.parents = []
        self.left = None
        self.right = None
    @property
    def vec(self):
        return self._vec
    @vec.setter
    def vec(self,val):
        self._vec[:]=val
    def __unicode__(self):
        return self.name
    def __repr__(self):
        return self.__unicode__()
    def _add_parent(self,node):
        self.parents.append(node)
