# -*- coding: utf-8 -*-
import sys
import os
sys.path.insert(0, os.environ.get('HOME')+'/nlp-prototype/rnnparser/RecursiveNN/')

import numpy as np
import pytest

from recursiveNN.nodes import Val,Var,VSF, Add,Mul,Dot,CTimes,Transpose, Node

def test_variables_of_expressions():
    ran=lambda x : np.random.random(x)-0.5
    x=Var('x',ran((1,4)))
    y=Var('y',ran((1,4)))
    z=Var('z',ran((1,4)))
    expr=Add(Add(Add(x,y),x),z)
    for var in ['x','y','z'] :
        assert var in expr.variables
        assert list(expr.variables).count(var)==1

def test_expressions_code_generation():
    ran=lambda x : np.random.random(x)-0.5
    x=Var('x',ran((1,4)))
    y=Var('y',ran((4,1)))
    a=Var('a',ran((5,1)))
    b=Var('b',ran((6,1)))
    D,C,B,A = Var('D', ran((4,4))), Var('C', ran((4,6))), Var('B', ran((6,5))), Var('A', ran((5,4)))

    ns={}
    f = Dot(Dot(x,C),Dot(B,VSF('sin',Add(Dot(A,y),a))))
    assert f.code()=='np.dot(np.dot(x,C),np.dot(B,np.sin(np.add(np.dot(A,y),a))))'
    ns=Node.Compile('f',f, ns)
    assert ns['f'](A=A.val,B=B.val,C=C.val, a=a.val,x=x.val,y=y.val)==f.val
    g = Dot(Dot(x,C),VSF('sin',Add(Dot(B,VSF('sin',Add(Dot(A,y),a))),b)))
    assert g.code()=='np.dot(np.dot(x,C),np.sin(np.add(np.dot(B,np.sin(np.add(np.dot(A,y),a))),b)))'
    ns=Node.Compile('g',g, ns)
    assert ns['g'](A=A.val,B=B.val,C=C.val, a=a.val,b=b.val,x=x.val,y=y.val)==g.val
    
    h,w,b,u,w12     =Var('h'),Var('W'),Var('b'),Var('u'), Var('word12')
    phrase=VSF('tanh', Add(Dot(w, h), b))
    score=Dot(u,phrase)
    assert phrase.code()=='np.tanh(np.add(np.dot(W,h),b))'
    assert score.code()=='np.dot(u,np.tanh(np.add(np.dot(W,h),b)))'
