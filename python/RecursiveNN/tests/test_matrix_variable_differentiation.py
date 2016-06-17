#-*- coding: utf-8 -*-
import os
import sys
myPath = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, myPath + '/../')

import pytest
import numpy as np
from recursiveNN.nodes import Word,Phrase, Val,Var,VSF, Add,Mul,Dot,CTimes,Transpose,Sum0
from recursiveNN.math import IsZero,IsAllOne,IsIdentity, IsScalar,IsVector,IsMatrix, NormalizedMatrix
from recursiveNN.differentiation import Differentiation, _Diff


def test_MatrixDifferentiation_internal():
    w=Var('w',[[1,2],[2,3]])
    x,y,z = Var('x',1), Var('y',[1,2]),Var('z',[1,2])
    with pytest.raises(ValueError):
        _Diff(w, x, y, z)
        
        
def test_VectorAndMatrixVariables():
    ran=np.random.random
        
    x=Var('x',ran((1,4)))
    y=Var('y',ran((4,1)))
    a=Var('a',ran((5,1)))
    b=Var('b',ran((6,1)))
    D,C,B,A = Var('D', ran((4,4))), Var('C', ran((4,6))), Var('B', ran((6,5))), Var('A', ran((5,4)))

    assert(x.val.shape==(1,4))
    assert IsZero(Differentiation(Dot(x, y), D))
    assert IsZero(Differentiation(Dot(x, y), a))
    assert unicode(Differentiation(Dot(x, y), y))==u'xᵀ'
    assert unicode(Differentiation(Dot(x, y), x))==u'yᵀ'
    
    xDy = Dot(x,Dot(D,y))
    assert unicode(xDy)==u'x⋅D⋅y'
    assert unicode(Differentiation(xDy, y))==u'[x⋅D]ᵀ'
    assert unicode(Differentiation(xDy, x))==u'[D⋅y]ᵀ'
    xDy = Dot(Dot(x,D),y)
    assert unicode(xDy)==u'x⋅D⋅y'
    assert unicode(Differentiation(xDy, y))==u'[x⋅D]ᵀ'
    assert unicode(Differentiation(xDy, x))==u'[D⋅y]ᵀ'
    
    xCBAy = Dot(x,Dot(C,Dot(B,Dot(A,y))))
    assert unicode(xCBAy)==u'x⋅C⋅B⋅A⋅y'
    assert unicode(Differentiation(xCBAy, x))==u'[C⋅B⋅A⋅y]ᵀ'
    assert unicode(Differentiation(xCBAy, y))==u'[x⋅C⋅B⋅A]ᵀ'
    xCBAy = Dot(Dot(x,C),Dot(B,Dot(A,y)))
    assert unicode(xCBAy)==u'x⋅C⋅B⋅A⋅y'
    assert unicode(Differentiation(xCBAy, x))==u'[C⋅B⋅A⋅y]ᵀ'
    assert unicode(Differentiation(xCBAy, y))==u'[x⋅C⋅B⋅A]ᵀ'
    
    xfy = Dot(x,VSF('sin',y))
    assert unicode(xfy)==u'x⋅sin(y)'
    assert unicode(Differentiation(xfy,y))==u'[x⊗[cos(y)]ᵀ]ᵀ'
    
    f = Dot(Dot(x,C),Dot(B,VSF('sin',Add(Dot(A,y),a))))
    assert unicode(f)==u'x⋅C⋅B⋅sin(A⋅y+a)'
    assert unicode(Differentiation(f,y))==u'[x⋅C⋅B⊗[cos(A⋅y+a)]ᵀ⋅A]ᵀ'
    assert unicode(Differentiation(f,a))==u'[x⋅C⋅B⊗[cos(A⋅y+a)]ᵀ]ᵀ'
    
    g = Dot(Dot(x,C),VSF('sin',Add(Dot(B,VSF('sin',Add(Dot(A,y),a))),b)))
    assert unicode(g) == u'x⋅C⋅sin(B⋅sin(A⋅y+a)+b)'
    #assert unicode(Differentiation(g,x))==u''
    #assert unicode(Differentiation(g,b))==u''
    #assert unicode(Differentiation(g,a))==u''
    #assert unicode(Differentiation(g,y))==u''
    
    dgdx=Differentiation(g,x)
    dgdb=Differentiation(g,b)
    dgda=Differentiation(g,a)
    dgdy=Differentiation(g,y)
    delta=NormalizedMatrix(ran(x.val.shape), 0.01)

def test_GradientNumericalChecks():
    ran=np.random.random
    #0.01 is may not small enough for g=x⋅C⋅sin(B⋅sin(A⋅y+a)+b).
    scale=0.001
    p=[]
    p_ran=[]
    for i in range(10):
        x=Var('x',ran((1,4)))
        y=Var('y',ran((4,1)))
        a=Var('a',ran((5,1)))
        b=Var('b',ran((6,1)))
        D,C,B,A = Var('D', ran((4,4))), Var('C', ran((4,6))), Var('B', ran((6,5))), Var('A', ran((5,4)))
        g = Dot(Dot(x,C),VSF('sin',Add(Dot(B,VSF('sin',Add(Dot(A,y),a))),b)))
                
        var=y
        gradient=Differentiation(g,var)
        var0=var.val
        delta=NormalizedMatrix(ran(var.val.shape), scale)
        rand_grad=gradient.val.copy()
        np.random.shuffle(rand_grad)
        rand_grad=NormalizedMatrix(ran(gradient.val.shape), gradient.val.sum())
        dg_ran = float(delta.T.dot(rand_grad))
        dg_grad = float(delta.T.dot(gradient.val))
        g0=g.val
        var.val = var0 + delta
        g1=g.val
        dg=g1-g0
        p.append(dg/dg_grad)
        p_ran.append(dg/dg_ran)
    p=np.array(p)
    p_ran=np.array(p_ran)
    
    precision=np.abs(np.mean(p)-1)
    precision_ran=np.abs(np.mean(p_ran)-1)           
    assert precision < 10*scale
    assert precision < precision_ran
    
def test_NumericalVerification():
    pass