#-*- coding: utf-8 -*-
import os
import sys
myPath = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, myPath + '/../')

import pytest
import numpy as np
from recursiveNN.models import Word,Phrase, RecursiveNN,Word2VecFactory
from recursiveNN.nodes import Val,Var,VSF, Add,Mul,Dot,CTimes,Transpose, reset_NodeDict
from recursiveNN.differentiation import Differentiation

   
    
def assert_all(x):
    assert(np.all(x))

def test_GradientNumericalChecks(reset_NodeDict):
    ran=lambda x : np.random.random(x)-0.5
    n=5
    h0,w0,b0,u0     =Var('h0'),Var('W0'),Var('b0'),Var('u0')
    vh0,vw0,vb0,vu0 =ran((n,1)),ran((n,2*n)),ran((n,1)),ran((n,1))
    h0.val,w0.val,b0.val,u0.val=vh0,vw0,vb0,vu0
    
    rnn=RecursiveNN(w0,b0,u0)
    the,cat,on,a,hat=Word('the'),Word('cat'),Word('on'),Word('a'),Word('hat')
    the_cat=rnn.combineTwoNodes(the,cat)
    a_hat=rnn.combineTwoNodes(a,hat)
    the_cat_on=rnn.combineTwoNodes(the_cat,on)
    the_cat_on_a_hat=rnn.combineTwoNodes(the_cat_on,a_hat)
    assert unicode(the_cat_on_a_hat)==u'(((the,cat),on),(a,hat))'
    assert unicode(rnn.score(the_cat))==u'u0ᵀ⋅tanh(W0⋅(the⊕cat)+b0)'
    assert unicode(rnn.score(the_cat_on))==u'u0ᵀ⋅tanh(W0⋅((the,cat)⊕on)+b0)'
    
    score=rnn.score(the_cat_on_a_hat)
    s0=score.val
    gradient=Differentiation(score, w0)
    #TODO:Check parents management and makes the for-loop works.
    #for i in range(10):
    diff=0.001*ran(w0.val.shape)
    ds_grad=np.sum(diff*gradient.val)
    tmp=gradient.val
    np.random.shuffle(tmp)
    ds_ran=np.sum(diff*tmp)
    w0.val += diff
    s1=score.val
    ds=s1-s0
    print ds_grad/ds, ds_ran/ds
    assert(abs(ds-ds_grad)<abs(ds-ds_ran))
    np.testing.assert_allclose(ds, ds_grad, rtol=1e-2, atol=0)
    
    assert score.isContain(w0)
    assert not score.isContain(Var('xx'))
    
    
def test_IterativeParsing(reset_NodeDict):
    ran=lambda x : np.random.random(x)-0.5
    n=5
    h0,w0,b0,u0     =Var('h0'),Var('w0'),Var('b0'),Var('u0')
    vh0,vw0,vb0,vu0 =ran((n,1)),ran((n,2*n)),ran((n,1)),ran((n,1))
    h0.val,w0.val,b0.val,u0.val=vh0,vw0,vb0,vu0
    
    rnn=RecursiveNN(w0,b0,u0)
    the,cat,on,hat=Word('the'),Word('cat'),Word('on'),Word('hat')
    
    nodes=[the,cat,on,the,hat]
    assert nodes[0] is nodes[3]
    
    sentence, score=rnn.combineToSentence(nodes)
    print unicode(sentence), score.val
    print sentence
    print '%s'%sentence
    assert 0
    
    
import zmq
def test_GettingWord2Vec(reset_NodeDict):
    context = zmq.Context()
    socket = context.socket(zmq.REQ)
    socket.connect ("tcp://localhost:10100" )
    for query in ['lepton', 'lepton electron', 'muon electron', 'electron muon tau', 'Chromium']:
        socket.send(query)
        dtype,bytes = socket.recv().split(',')
        #np.fromstring(bytes,dtype=dtype)    
    #assert 0
    
