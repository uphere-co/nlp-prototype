#-*- coding: utf-8 -*-
import pytest
import numpy as np
from recursiveNN.nodes import Val,Var,VSF, Add,Mul,Dot,CTimes,Transpose, reset_NodeDict, deregister
from recursiveNN.math import IsZero,IsAllOne,IsIdentity, IsScalar,IsVector,IsMatrix, NormalizedMatrix
from recursiveNN.differentiation import Differentiation, _Diff


def test_MatrixDifferentiation_internal(reset_NodeDict):
    w=Var('w',[[1,2],[2,3]])
    x,y,z = Var('x',1), Var('y',[1,2]),Var('z',[1,2])
    with pytest.raises(ValueError):
        _Diff(w, x, y, z)

def test_VectorAndMatrixVariables(reset_NodeDict):
    ran=lambda x : np.random.random(x)-0.5

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
    assert unicode(Differentiation(xfy,y))==u'[x⨯[cos(y)]ᵀ]ᵀ'

    f = Dot(Dot(x,C),Dot(B,VSF('sin',Add(Dot(A,y),a))))
    assert unicode(f)==u'x⋅C⋅B⋅sin(A⋅y+a)'
    assert unicode(Differentiation(f,y))==u'[x⋅C⋅B⨯[cos(A⋅y+a)]ᵀ⋅A]ᵀ'
    assert unicode(Differentiation(f,a))==u'[x⋅C⋅B⨯[cos(A⋅y+a)]ᵀ]ᵀ'

    g = Dot(Dot(x,C),VSF('sin',Add(Dot(B,VSF('sin',Add(Dot(A,y),a))),b)))
    assert unicode(g) == u'x⋅C⋅sin(B⋅sin(A⋅y+a)+b)'
    assert unicode(Differentiation(g,x))==u'[C⋅sin(B⋅sin(A⋅y+a)+b)]ᵀ'
    assert unicode(Differentiation(g,b))==u'[x⋅C⨯[cos(B⋅sin(A⋅y+a)+b)]ᵀ]ᵀ'
    assert unicode(Differentiation(g,a))==u'[x⋅C⨯[cos(B⋅sin(A⋅y+a)+b)]ᵀ⋅B⨯[cos(A⋅y+a)]ᵀ]ᵀ'
    assert unicode(Differentiation(g,y))==u'[x⋅C⨯[cos(B⋅sin(A⋅y+a)+b)]ᵀ⋅B⨯[cos(A⋅y+a)]ᵀ⋅A]ᵀ'
    print unicode(Differentiation(g,A))
    assert unicode(Differentiation(g,A))==u'[x⋅C⨯[cos(B⋅sin(A⋅y+a)+b)]ᵀ⋅B⨯[cos(A⋅y+a)]ᵀ]ᵀ⨯yᵀ'
    assert unicode(Differentiation(g,B))==u'[x⋅C⨯[cos(B⋅sin(A⋅y+a)+b)]ᵀ]ᵀ⨯[sin(A⋅y+a)]ᵀ'
    assert unicode(Differentiation(g,C))==u'xᵀ⨯[sin(B⋅sin(A⋅y+a)+b)]ᵀ'

    dgdx=Differentiation(g,x)
    dgdb=Differentiation(g,b)
    dgda=Differentiation(g,a)
    dgdy=Differentiation(g,y)
    delta=NormalizedMatrix(ran(x.val.shape), 0.01)

def test_ReuseExistingExpressions(reset_NodeDict):
    ran=lambda x : np.random.random(x)-0.5
    x,y,a,b=Var('x'),Var('y'),Var('a'),Var('b')
    D,C,B,A = Var('D'), Var('C'), Var('B'), Var('A')
    x.val,y.val,a.val,b.val=ran((1,4)),ran((4,1)),ran((5,1)),ran((6,1))
    D.val,C.val,B.val,A.val = ran((4,4)), ran((4,6)), ran((6,5)),ran((5,4))

    xDy = Dot(x,Dot(D,y))
    xDy.cache()
    assert Var('x') is xDy.x
    assert  Dot(D,y).y is Var('y')
    assert  Dot(D,y) is xDy.y
    xDy2 = Dot(Dot(x,D),y)
    tmp=Differentiation(xDy2, x)
    tmp2=Differentiation(xDy2, x)
    assert tmp2 is not tmp
    tmp.cache()
    tmp2=Differentiation(xDy2, x)
    assert tmp2 is tmp
    tmp3=Differentiation(xDy, x)
    assert tmp3 is tmp

    f = Dot(Dot(x,C),Dot(B,VSF('sin',Add(Dot(A,y),a))))
    f.cache()
    dfdy=Differentiation(f,y)
    dfdy2=Differentiation(f,y)
    assert dfdy2 is not dfdy
    dfdy.cache()
    dfdy2=Differentiation(f,y)
    assert dfdy2 is dfdy
    dfda=Differentiation(f,a)
    assert dfda.var is dfdy.var.x

    g = Dot(Dot(x,C),VSF('sin',Add(Dot(B,VSF('sin',Add(Dot(A,y),a))),b)))
    g.cache()
    assert g.y.var.x is f.y
    dgdA=Differentiation(g,A)
    dgdA.cache()
    dgdB=Differentiation(g,B)
    print unicode(dgdB)
    assert dgdB.y.var is f.y.y
    assert dgdA.x.var.x.x is dgdB.x.var

def test_GradientNumericalChecks(reset_NodeDict):
    ran=lambda x : np.random.random(x)-0.5
    x,y,a,b=Var('x'),Var('y'),Var('a'),Var('b')
    D,C,B,A = Var('D'), Var('C'), Var('B'), Var('A')
    #g:= x⋅C⋅sin(B⋅sin(A⋅y+a)+b)
    g = Dot(Dot(x,C),VSF('sin',Add(Dot(B,VSF('sin',Add(Dot(A,y),a))),b)))
    g.cache()

    #0.01 is may not small enough for g=x⋅C⋅sin(B⋅sin(A⋅y+a)+b).
    scale=0.001
    for var in [B,A,y,a]:
        p=[]
        p_ran=[]
        for i in range(3):
            x.val,y.val,a.val,b.val=ran((1,4)),ran((4,1)),ran((5,1)),ran((6,1))
            #D,C,B,A = Var('D', ran((4,4))), Var('C', ran((4,6))), Var('B', ran((6,5))), Var('A', ran((5,4)))
            D.val,C.val,B.val,A.val = ran((4,4)), ran((4,6)), ran((6,5)),ran((5,4))
            gradient=Differentiation(g,var)
            gradient.cache()
            var0=var.val
            delta=NormalizedMatrix(ran(var.val.shape), scale)
            rand_grad=gradient.val.copy()
            np.random.shuffle(rand_grad)
            rand_grad=NormalizedMatrix(ran(gradient.val.shape), gradient.val.sum())
            dg_ran = np.sum(delta*rand_grad)
            dg_grad = np.sum(delta*gradient.val)
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
