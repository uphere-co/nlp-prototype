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
    assert str(Differentiation(Dot(x, y), y))=='(x).T'
    assert str(Differentiation(Dot(x, y), x))=='(y).T'
    
    xDy = Dot(x,Dot(D,y))
    assert str(xDy)=='x⋅D⋅y'
    assert str(Differentiation(xDy, y))=='(x⋅D).T'
    assert str(Differentiation(xDy, x))=='(D⋅y).T'
    xDy = Dot(Dot(x,D),y)
    assert str(xDy)=='x⋅D⋅y'
    assert str(Differentiation(xDy, y))=='(x⋅D).T'
    assert str(Differentiation(xDy, x))=='(D⋅y).T'
    
    xCBAy = Dot(x,Dot(C,Dot(B,Dot(A,y))))
    assert str(xCBAy)=='x⋅C⋅B⋅A⋅y'
    assert str(Differentiation(xCBAy, x))=='(C⋅B⋅A⋅y).T'
    assert str(Differentiation(xCBAy, y))=='(x⋅C⋅B⋅A).T'
    xCBAy = Dot(Dot(x,C),Dot(B,Dot(A,y)))
    assert str(xCBAy)=='x⋅C⋅B⋅A⋅y'
    assert str(Differentiation(xCBAy, x))=='(C⋅B⋅A⋅y).T'
    assert str(Differentiation(xCBAy, y))=='(x⋅C⋅B⋅A).T'
    
    xfy = Dot(x,VSF('sin',y))
    assert str(xfy)=='x⋅sin(y)'
    assert str(Differentiation(xfy,y))=='(x⊗(cos(y)).T).T'
    
    f = Dot(Dot(x,C),Dot(B,VSF('sin',Add(Dot(A,y),a))))
    assert str(f)=='x⋅C⋅B⋅sin(A⋅y+a)'
    assert str(Differentiation(f,y))=='(x⋅C⋅B⊗(cos(A⋅y+a)).T⋅A).T'
    assert str(Differentiation(f,a))=='(x⋅C⋅B⊗(cos(A⋅y+a)).T).T'
    
    g = Dot(Dot(x,C),VSF('sin',Add(Dot(B,VSF('sin',Add(Dot(A,y),a))),b)))
    assert str(g) == 'x⋅C⋅sin(B⋅sin(A⋅y+a)+b)'
    assert str(Differentiation(g,x))=='(C⋅sin(B⋅sin(A⋅y+a)+b)).T'
    assert str(Differentiation(g,b))==''
    assert str(Differentiation(g,a))==''
    assert str(Differentiation(g,y))==''
    
    
def test_NumericalVerification():
    pass