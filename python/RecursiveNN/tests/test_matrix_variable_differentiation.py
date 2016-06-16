#-*- coding: utf-8 -*-
import os
import sys
myPath = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, myPath + '/../')

import pytest
import numpy as np
from recursiveNN.nodes import Word,Phrase, Val,Var,Fun,VSF, Add,Mul,Dot,CTimes,Transpose,Sum0
from recursiveNN.math import IsZero,IsAllOne,IsIdentity, IsScalar,IsVector,IsMatrix
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
    assert unicode(Differentiation(Dot(x, y), y))==u'(x)ᵀ'
    assert unicode(Differentiation(Dot(x, y), x))==u'(y)ᵀ'
    
    xDy = Dot(x,Dot(D,y))
    assert unicode(xDy)==u'x⋅D⋅y'
    assert unicode(Differentiation(xDy, y))==u'(x⋅D)ᵀ'
    assert unicode(Differentiation(xDy, x))==u'(D⋅y)ᵀ'
    xDy = Dot(Dot(x,D),y)
    assert unicode(xDy)==u'x⋅D⋅y'
    assert unicode(Differentiation(xDy, y))==u'(x⋅D)ᵀ'
    assert unicode(Differentiation(xDy, x))==u'(D⋅y)ᵀ'
    
    xCBAy = Dot(x,Dot(C,Dot(B,Dot(A,y))))
    assert unicode(xCBAy)==u'x⋅C⋅B⋅A⋅y'
    assert unicode(Differentiation(xCBAy, x))==u'(C⋅B⋅A⋅y)ᵀ'
    assert unicode(Differentiation(xCBAy, y))==u'(x⋅C⋅B⋅A)ᵀ'
    xCBAy = Dot(Dot(x,C),Dot(B,Dot(A,y)))
    assert unicode(xCBAy)==u'x⋅C⋅B⋅A⋅y'
    assert unicode(Differentiation(xCBAy, x))==u'(C⋅B⋅A⋅y)ᵀ'
    assert unicode(Differentiation(xCBAy, y))==u'(x⋅C⋅B⋅A)ᵀ'
    
    xfy = Dot(x,VSF('sin',y))
    assert unicode(xfy)==u'x⋅sin(y)'
    assert unicode(Differentiation(xfy,y))==u'(x⊗(cos(y))ᵀ)ᵀ'
    
    f = Dot(Dot(x,C),Dot(B,VSF('sin',Add(Dot(A,y),a))))
    assert unicode(f)==u'x⋅C⋅B⋅sin(A⋅y+a)'
    assert unicode(Differentiation(f,y))==u'(x⋅C⋅B⊗(cos(A⋅y+a))ᵀ⋅A)ᵀ'
    assert unicode(Differentiation(f,a))==u'(x⋅C⋅B⊗(cos(A⋅y+a))ᵀ)ᵀ'
    
    g = Dot(Dot(x,C),VSF('sin',Add(Dot(B,VSF('sin',Add(Dot(A,y),a))),b)))
    assert unicode(g) == u'x⋅C⋅sin(B⋅sin(A⋅y+a)+b)'
    assert unicode(Differentiation(g,x))==u''
    assert unicode(Differentiation(g,b))==u''
    assert unicode(Differentiation(g,a))==u''
    assert unicode(Differentiation(g,y))==u''
    
    
def test_NumericalVerification():
    pass