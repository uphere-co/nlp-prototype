#-*- coding: utf-8 -*-
import os
import sys
myPath = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, myPath + '/../')

import pytest
import numpy as np
from recursiveNN.models import Word,Phrase, RecursiveNN,Word2VecFactory
from recursiveNN.nodes import Val,Var,VSF, Add,Mul,Dot,CTimes,Transpose
from recursiveNN.differentiation import Differentiation

def assert_all(x):
    assert(np.all(x))

def test_RecursiveNNDifferentiation():
    ran=np.random.random
    h0,w0,b0,p0     =Var('h0'),Var('W0'),Var('b0'),Var('p0')
    vh0,vw0,vb0,vp0 =ran((3,1)),ran((4,3)),ran((4,1)),ran((4,1))
    h0.val,w0.val,b0.val,p0.val=vh0,vw0,vb0,vp0
    h1=VSF('tanh',Add(Dot(w0,h0),b0))
    s0=Dot(Transpose(p0),h1)
    
    assert('%r'%h1=="VSF('tanh')(Add(Dot(Var('W0'),Var('h0')),Var('b0')))")
    assert(unicode(h1)==u'tanh(W0⋅h0+b0)')
    assert(unicode(s0)==u'p0ᵀ⋅tanh(W0⋅h0+b0)')
    assert_all(s0.val==vp0.T.dot(np.tanh(vw0.dot(vh0)+vb0)))  
    