#-*- coding: utf-8 -*-
# -*- coding: utf-8 -*-

import os
import sys
myPath = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, myPath + '/../')

import pytest
import numpy as np
from recursiveNN.nodes import Val,Var,VSF, Add,Mul,Dot,CTimes,Transpose,Sum0, reset_NodeDict
from recursiveNN.math import IsZero,IsAllOne,IsIdentity, IsScalar,IsVector,IsMatrix


def test_ElementaryTypes(reset_NodeDict):
    val0 = Val(0)
    val2 = Val(2)
    val10= Val(10)
    val=Mul(Val(2),Val(10))
    assert(unicode(val)=='2*10')
    assert(unicode(val0)=='0')
    assert(unicode(val10)=='10')

def test_ValImmunity():
    v1=Val(1)
    with pytest.raises(TypeError):
        v1.val=2
    assert v1.val == 1
def test_Equality(reset_NodeDict):
    x =Var('x')
    y =Var('y')
    fx=VSF('f',x)
    #Each expressions are singletons.
    assert(x ==Var('x'))
    assert(fx==VSF('f',x))
    assert(Var('x') == x)
    assert(Val(1)==Val(1))
    #Values are simplified to scalar when possible:
    assert(Val(1)==Val([1]))
    assert(Val(1)==Val([[1]]))

def test_DiffOperations(reset_NodeDict):
    x =Var('x')
    y =Var('y')
    fx=VSF('f',x)
    fy=VSF('f',y)
    gx=VSF('g',x)
    gy=VSF('g',y)
    gfx=VSF('g',fx)
    zero=Val(0)
    one=Val(1)
    assert(zero==Val(0))
    assert(zero!=one)
    assert(fx!=gx)
    assert(unicode(Var('x'))=='x')
    assert(unicode(fx)== 'f(x)')
    assert(unicode(gfx)== 'g(f(x))')
    assert(unicode(Mul(fx,gx)) == 'f(x)*g(x)')
    assert(unicode(Mul(fx,gy)) == 'f(x)*g(y)')
    assert(unicode(Mul(gfx,gy)) == 'g(f(x))*g(y)')
    z='0.0'
    i='1.0'
    assert(unicode(Mul(fx,gx).diff_no_simplify(x)) == u"f`(x)⊗%s⊗g(x)+f(x)⊗g`(x)⊗%s"%(i,i))
    assert(unicode(Mul(fx,gy).diff_no_simplify(x)) == u"f`(x)⊗%s⊗g(y)+f(x)⊗g`(y)⊗%s"%(i,z))
    assert(unicode(fx.diff_no_simplify(x))==u"f`(x)⊗%s"%i)
    assert(unicode(gfx.diff_no_simplify(x))==u"g`(f(x))⊗f`(x)⊗%s"%i)
    assert(unicode(gfx.diff_no_simplify(y))==u"g`(f(x))⊗f`(x)⊗%s"%z)
    assert(unicode(Mul(gfx,gy).diff_no_simplify(x)) ==\
           u'g`(f(x))⊗f`(x)⊗%s⊗g(y)+g(f(x))⊗g`(y)⊗%s'%(i,z))
    assert(unicode(Mul(gfx,gy).diff_no_simplify(y)) ==\
           u'g`(f(x))⊗f`(x)⊗%s⊗g(y)+g(f(x))⊗g`(y)⊗%s'%(z,i))

def test_SimplifyZeroAndOne(reset_NodeDict):
    assert(IsIdentity(Val(1)))
    assert(Mul(Val(1),Val(1)).simplify()==Val(1))
    assert(Mul(Mul(Val(1),Val(1)),Mul(Val(1),Mul(Val(1),Mul(Val(1),Val(1))))).simplify()==Val(1))
    assert(Mul(Mul(Val(1),Val(1)),Mul(Val(1),Mul(Val(1),Mul(Val(1),Val(0))))).simplify()==Val(0))
    x =Var('x')
    y =Var('y')
    fx=VSF('f',x)
    fy=VSF('f',y)
    gx=VSF('g',x)
    gy=VSF('g',y)
    hx=VSF('h',x)
    gfx=VSF('g',fx)
    zero=Val(0)
    one=Val(1)
    assert(unicode(Mul(one,Add(x,y)))=='1*{x+y}')
    assert(Mul(one,Add(x,y)).expression()=='1*{x+y}')
    assert(unicode(Mul(one,Add(x,y)).simplify())=='x+y')
    assert(Add(fx, zero).simplify()==fx)
    assert(Mul(fx, zero).simplify()==zero)
    assert(Mul(fx, one).simplify()==fx)
    assert(Mul(zero, gy).simplify()==zero)
    assert(Mul(gx, Mul(fx, zero)).simplify()==zero)
    assert(Mul(gy, Mul(gx, Mul(fx, zero))).simplify()==zero)
    assert(unicode(fx.diff(x))=='f`(x)')
    assert(unicode(Mul(gfx,gy).diff(x))==u'g`(f(x))⊗f`(x)⊗g(y)')
    assert(unicode(Mul(gfx,gy).diff(y))==u'g(f(x))⊗g`(y)')

    assert(unicode(Mul(hx,Mul(gx,fx)))==u'h(x)*g(x)*f(x)')
    assert(unicode(Mul(hx,Mul(gx,fx)).diff(x))==u'h`(x)⊗g(x)*f(x)+h(x)⊗{g`(x)⊗f(x)+g(x)⊗f`(x)}')
    assert(unicode(Mul(hx,Mul(gx,fx)).diff(y))=='0.0')

def _SimplePhrase(reset_NodeDict):
    W_left_init  = Var('W_left', [0,0,0,0])
    W_right_init = Var('W_right', [0,0,0,0])
    bias_init = Var('bias', [1,1])
    rnn=RecursiveNN(W_left_init, W_right_init, bias_init)
    merge=rnn.combineTwoNodes

    the=Word('The')
    cat=Word('cat')
    assert(not hasattr(Val(11), 'expression'))
    assert(hasattr(the, 'expression'))
    assert(the.expression()=='w2v(The)')
    assert(unicode(the)=='The')

    the_cat=merge(the,cat)
    assert(unicode(the_cat)=='(The,cat)')
    assert(the_cat.expression()=='tanh(W_left*w2v(The)+W_right*w2v(cat)+bias)')
    assert(unicode(the_cat.diff_no_simplify(Var('W_left')))=='')
    assert(the_cat.diff(Var('W_left')).expression()=='tanh`(W_left*w2v(The)+W_right*w2v(cat)+bias)*w2v(The)')

    the_cat_is=merge(the_cat, Word('is'))
    assert(unicode(the_cat_is)=='((The,cat),is)')
    assert(the_cat_is.expression()=='tanh(W_left*tanh(W_left*w2v(The)+W_right*w2v(cat)+bias)+W_right*w2v(is)+bias)')
    expected='tanh`(W_left*tanh(W_left*w2v(The)+W_right*w2v(cat)+bias)+W_right*w2v(is)+bias)*'+ \
             '(tanh(W_left*w2v(The)+W_right*w2v(cat)+bias)+W_left*tanh`(W_left*w2v(The)+W_right*w2v(cat)+bias)*w2v(The))'
    assert(the_cat_is.diff(Var('W_left')).expression()==expected)

    assert(unicode(merge(the_cat, merge(Word('is'), Word('cute'))))=='((The,cat),(is,cute))')

    the_cat_is_cute=merge(the_cat_is, Word('cute'))
    assert(unicode(the_cat_is_cute)=='(((The,cat),is),cute)')
    expected='tanh(W_left*tanh(W_left*tanh(W_left*w2v(The)+W_right*w2v(cat)+bias)+W_right*w2v(is)+bias)+W_right*w2v(cute)+bias)'
    assert(the_cat_is_cute.expression()==expected)

def assert_all(x):
    assert(np.all(x))
def test_Evaluation(reset_NodeDict):
    vx=np.array([1.0,2.0,3.0]).reshape(1,3)
    vy=np.array([2.0,3.0,4.0]).reshape(3,1)
    vz=np.array([3.0,5.0,7.0]).reshape(1,3)
    x=Var('x')
    x.val=vx
    y=Var('y', vy)
    z=Var('z',vz)
    with pytest.raises(ValueError):
        Dot(x,Var('t',vy.T)).val
    xy=Mul(x,y)
    assert(unicode(xy)=='x*y')
    assert_all(xy.val==vx.dot(vy))
    x_plus_z=Add(x,z)
    assert(unicode(x_plus_z)=='x+z')
    assert_all(x_plus_z.val==vx+vz)
    assert_all(CTimes(xy,z).val==CTimes(z,xy).val)
    assert_all(CTimes(xy,z).val==vx.dot(vy)*vz)
    s0=1.57
    s=Var('s',s0)
    fs=VSF('cos',s, np.cos)
    assert(unicode(fs)=='cos(s)')
    assert(fs.val==np.cos(s0))

def test_CacheKnownValues(reset_NodeDict):
    x=Var('x')
    fx=VSF('cos', x, np.cos)
    gfx=VSF('exp', fx, np.exp)
    gfx.cache()
    exp_cos=lambda x : np.exp(np.cos(x))
    for v in np.random.random(10):
        x.val=v
        assert(gfx.val==exp_cos(v))
    for i in range(100):
        assert(gfx.val==exp_cos(v))

    y=Var('y')
    hy=VSF('tanh', y, np.tanh)
    hy.cache()
    for v in np.random.random(10):
        y.val=v
        assert(hy.val==np.tanh(v))
    gfx_hy = CTimes(gfx, hy)
    gfx_hy.cache()
    exp_cos_x_times_tanh_y = lambda x, y : exp_cos(x)*np.tanh(y)
    vx=5.7
    vy=np.array([1.1,2.1,0.5])
    x.val=vx
    y.val=vy
    assert_all(gfx_hy.val==exp_cos_x_times_tanh_y(vx,vy))
    print "Change x only:"
    #TODO: verify hy will not be evaluated, but use cache, instead.
    vx=1.0
    x.val=vx
    assert_all(gfx_hy.val==exp_cos_x_times_tanh_y(vx,vy))
    print "Change y only:"
    #TODO: verify gfx will not be evaluated, but use cache, instead.
    vy=1.0
    y.val=vy
    assert_all(gfx_hy.val==exp_cos_x_times_tanh_y(vx,vy))

    #Instance of Var must be single-assigned,
    #but it is not yet enforced by code.
    a=Var('a')
    b=Var('b')
    ab=Mul(a,b)
    ab.cache()
    assert(np.isnan(ab.val))
    a.val=1.0
    assert(np.isnan(ab.val))
    b.val=2.0
    assert(ab.val==2.0)

def test_ParentsSettingOfCache():
    x=Var('x')
    fx=VSF('sin',x)
    dfx=fx.diff(x)
    gfx=VSF('exp',Mul(Val(3), fx))
    dgfx=gfx.diff(x)
    dgfx.cache()
    assert fx in x.parents and dfx in x.parents
    for expr in dgfx.children:
        assert dgfx in expr.parents

def test_ExpressionMutations(reset_NodeDict):
    x=Var('x')
    fx=VSF('sin',x, np.sin)
    gx=VSF('exp',x, np.exp)
    #Mutable expression need to be cached.
    fx.cache()
    gx.cache()
    v=1.0
    for v in [1.0,0.5,0.1]:
        x.val=v
        assert(fx.val==np.sin(v))
        assert(gx.val==np.exp(v))

def test_DiffKnownFunctions(reset_NodeDict):
    x=Var('x')
    fx=VSF('sin',x)
    dfx=fx.diff(x)
    assert(dfx.expression()=='cos(x)')
    gfx=VSF('exp',Mul(Val(3), fx))
    dgfx=gfx.diff(x)
    assert(dgfx.expression()==u'exp(3*sin(x))⊗3⊗cos(x)')
    #Caching top expressions only is enough
    gfx.cache()
    dgfx.cache()
    for v in [1.0, 2.0, 14.2, 5.1, 5.72341] :
        x.val=v
        assert(dfx.val==np.cos(v))
        assert(gfx.val==np.exp(3*np.sin(v)))
        #Allow slight difference for complex numpy expressions.
        np.testing.assert_allclose(dgfx.val,np.exp(3*np.sin(v))*3*np.cos(v), rtol=1e-10, atol=0)

    hfx=VSF('log',fx)
    dhfx=hfx.diff(x)
    assert(dhfx.expression()==u'1/(sin(x))⊗cos(x)')
    hx=VSF('log',Add(fx, VSF('exp', x)))
    dhx=hx.diff(x)
    assert(dhx.expression()==u'1/(sin(x)+exp(x))⊗{cos(x)+exp(x)}')

def test_matrix_circle_times_operations(reset_NodeDict):
    va=np.matrix([[1,2,3],[2,3,4]])
    vb=np.matrix([[2,3,4],[3,2,1]])
    vx=np.matrix([5,1,2])
    vy=np.matrix([[5],[1],[2]])
    a,b,x,y=Var('a',va),Var('b',vb),Var('x',vx),Var('y',vy)
    with pytest.raises(ValueError):
        va*vx
    with pytest.raises(ValueError):
        va.dot(vx)
    with pytest.raises(ValueError):
        Mul(a,x).val
    assert_all(va*vy==va.dot(vy))
    assert_all(va*vy==Mul(a,y).val)
    assert_all(va*vy==[[13],[21]])

    assert_all(CTimes(a,a).val== np.array([[ 1,4,9], [4,9,16]]))
    assert_all(CTimes(a,b).val== np.array([[ 2,6,12], [6,6,4]]))
    assert_all(CTimes(b,a).val== np.array([[ 2,6,12], [6,6,4]]))

    vd=np.matrix([1,3])
    ve=np.matrix([[1],[2],[4]])
    vf=np.matrix([1,2,4])
    d,e,f=Var('d',vd),Var('e',ve),Var('f',vf)
    with pytest.raises(ValueError):
        Mul(d,e).val
    assert(np.all(CTimes(d,e).val==np.array([[ 1,  3], [2,  6], [ 4, 12]])))
    assert(np.all(CTimes(e,d).val==np.array([[ 1,  3], [2,  6], [ 4, 12]])))
    with pytest.raises(ValueError):
        CTimes(d,f).val

def test_IdentifyZeroOrIdentityMatrix(reset_NodeDict):
    assert(np.all(np.identity(3, dtype=np.float32)==np.identity(3,dtype=np.float64)))
    zero=Val(np.zeros((3,5)))
    assert(IsZero( zero ))
    zero=Val(np.zeros((3,3)))
    i=Val(np.identity(3))
    assert(IsZero( zero ))
    assert(not IsZero( i ))
    assert(IsIdentity( i ))
    assert(not IsIdentity( zero ))
    assert(not IsIdentity( Val(np.array([[1,0,0],[0,1,0]])) ))

def test_SimplifyZeroAndIdentityMatrix(reset_NodeDict):
    i=np.identity(5)
    z=np.zeros((5,5))
    assert(IsIdentity(Val(i)))
    assert(Mul(Val(i),Val(i)).simplify()==Val(i))
    assert(Mul(Mul(Val(i),Val(i)),Mul(Val(i),Mul(Val(i),Mul(Val(i),Val(i))))).simplify()==Val(i))
    assert(Mul(Mul(Val(i),Add(Add(Val(z),Val(z)),Val(i))),Mul(Val(i),Mul(Val(i),Mul(Val(i),Val(i))))).simplify()==Val(i))
    assert(Mul(Mul(Val(i),Val(i)),Mul(Val(i),Mul(Val(i),Mul(Val(i),Val(z))))).simplify()==Val(z))

def test_Transpose(reset_NodeDict):
    vx=np.matrix([5,1,2])
    vy=np.matrix([1,3,2]).T
    x=Var('x',vx)
    y=Var('y',vy)

    fy=VSF('f',y)
    gx=VSF('g',x)
    fygx=Mul(fy,gx)
    assert(unicode(Transpose(fygx).simplify())==u'[f(y)*g(x)]ᵀ')
    assert(unicode(Transpose(Var('z',2)).simplify())=='z')
    assert(Transpose(Var('z',2)).val==2)
    assert_all(Transpose(x).val==vx.T)
    assert_all(Transpose(Transpose(x)).val==x.val)
    y.val=vy.T
    xyt=Mul(x,Transpose(y))
    assert(unicode(Transpose(xyt))==u'[x*yᵀ]ᵀ')
    assert(xyt.val==12)

def test_Sum0(reset_NodeDict):
    vx=np.array([5,1,2]).reshape(3,1)
    vy=np.array([[1,3],[2,3],[2,3]])
    x=Var('x',vx)
    y=Var('y',vy)
    assert(unicode(Sum0(y))==u'Σ_0(y)')
    assert(unicode(Sum0(CTimes(x,y)))==u'Σ_0(x⊗y)')
    assert(CTimes(x,y).val.shape==(3, 2))
    assert(Sum0(CTimes(x,y)).val.shape==(1, 2))
    assert_all(Sum0(CTimes(x,y)).val==[11,24])
    assert_all(Sum0(CTimes(x,y)).val==Dot(Transpose(x),y).val)



def test_FeedForwardNNEvaluation(reset_NodeDict):
    pass
