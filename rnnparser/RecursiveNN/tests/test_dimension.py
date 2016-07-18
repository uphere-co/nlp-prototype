#-*- coding: utf-8 -*-
import pytest
import numpy as np
from recursiveNN.nodes import Val,Var,VSF, Add,Concat,Mul,Dot,CTimes,Transpose, reset_NodeDict
from recursiveNN.math import IsZero,IsAllOne,IsIdentity, IsScalar,IsVector,IsMatrix


ran = lambda x : np.random.random(x).astype(np.float32)

def test_Dot():
    a=Var('A',ran((2,3)))
    b=Var('b',ran((3,)))
    ab=Dot(a,b)
    assert ab.val.shape == (2,)
    assert ab.diff(b)==a


def declareAndCache(name, val=None):
    var=Var(name, val)
    var.cache()
    return var

def _RNN():
    ran=lambda x : np.random.random(x)-0.5
    phrase_expr = lambda W_mat,word_left, word_right,bias : VSF('tanh', Add(Dot(W_mat, Concat(word_left,word_right)), bias))
    W=declareAndCache(u'W')
    W.val=ran((200,400))
    bias=declareAndCache(u'b',)
    bias.val=ran((200,))
    u_score=declareAndCache(u'u_score',)
    u_score.val=ran((200,))
    assert Var(u'W') is W
    phrase_w1w2=lambda w1,w2 : phrase_expr(W,w1,w2,bias)

    w1=declareAndCache(u'w1')
    w2=declareAndCache(u'w2')
    w1.val,w2.val = ran(200),ran(200)
    score = lambda phrase : Dot(u_score,phrase)
    #score(phrase_w1w2(w1,w2)).diff(W)
    print unicode(phrase_w1w2(w1,w2).diff(W))
