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

def test_GradientNumericalChecks():
    ran=np.random.random
    n=5
    h0,w0l,w0r,b0,u0     =Var('h0'),Var('W0l'),Var('W0r'),Var('b0'),Var('u0')
    vh0,vw0l,vw0r,vb0,vu0 =ran((n,1)),ran((n,n)),ran((n,n)),ran((n,1)),ran((n,1))
    h0.val,w0l.val,w0r.val,b0.val,u0.val=vh0,vw0l,vw0r,vb0,vu0
    
    rnn=RecursiveNN(w0l,w0r,b0,u0)
    the,cat,on,a,hat=Word('the'),Word('cat'),Word('on'),Word('a'),Word('hat')
    the_cat=rnn.combineTwoNodes(the,cat)
    a_hat=rnn.combineTwoNodes(a,hat)
    the_cat_on=rnn.combineTwoNodes(the_cat,on)
    the_cat_on_a_hat=rnn.combineTwoNodes(the_cat_on,a_hat)
    assert unicode(the_cat_on_a_hat)==u'(((the,cat),on),(a,hat))'
    score=rnn.score(the_cat_on)
    assert unicode(score)==u'u0ᵀ⋅tanh(W0l⋅tanh(W0l⋅the+W0r⋅cat+b0)+W0r⋅on+b0)'
    
    s0=score.val
    gradient=Differentiation(score, w0l)
    #TODO:Check parents management and makes the for-loop works.
    #for i in range(10):
    diff=0.001*ran(w0l.val.shape)
    ds_grad=np.sum(diff*gradient.val)
    tmp=gradient.val
    np.random.shuffle(tmp)
    ds_ran=np.sum(diff*tmp)
    w0l.val += diff
    s1=score.val
    ds=s1-s0
    print ds_grad/ds, ds_ran/ds
    assert(abs(ds-ds_grad)<abs(ds-ds_ran))
    np.testing.assert_allclose(ds, ds_grad, rtol=1e-2, atol=0)
    
    
def test_IterativeParsing():
    ran=np.random.random
    n=5
    h0,w0l,w0r,b0,u0     =Var('h0'),Var('W0l'),Var('W0r'),Var('b0'),Var('u0')
    vh0,vw0l,vw0r,vb0,vu0 =ran((n,1)),ran((n,n)),ran((n,n)),ran((n,1)),ran((n,1))
    h0.val,w0l.val,w0r.val,b0.val,u0.val=vh0,vw0l,vw0r,vb0,vu0
    
    rnn=RecursiveNN(w0l,w0r,b0,u0)
    the,cat,on,hat=Word('the'),Word('cat'),Word('on'),Word('hat')
    
    nodes=[the,cat,on,the,hat]
    assert nodes[0] is nodes[3]
    
    sentence=rnn.combineSentence(nodes)
    print unicode(sentence)
    assert 0
    
    
    
    
