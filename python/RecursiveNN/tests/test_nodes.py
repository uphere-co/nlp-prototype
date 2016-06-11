# -*- coding: utf-8 -*-

import os
import sys
myPath = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, myPath + '/../')

import pytest
import numpy as np
from recursiveNN.nodes import Word,Phrase, Val,Var,Fun, Add,Mul,CTimes, IsZero, IsIdentity, Transpose
from recursiveNN.models import RecursiveNN

def test_ElementaryTypes():
    val0 = Val(0)
    val2= Val(2)
    val10= Val(10)
    val=Mul(Val(2),Val(10))
    assert(str(val)=='2*10')
    assert(str(val0)=='0')
    assert(str(val10)=='10')
       
def test_DiffOperations():
    x =Var('x')    
    y =Var('y') 
    fx=Fun('f',x)
    fy=Fun('f',y)
    gx=Fun('g',x)
    gy=Fun('g',y)
    gfx=Fun('g',fx)
    zero=Val(0)
    one=Val(1)
    assert(zero==Val(0))
    assert(zero!=one)
    assert(x !=Var('x'))
    assert(fx==Fun('f',x))
    assert(x ==Var('x'))
    assert(fx!=Fun('f',y))
    assert(fx!=gx)
    assert(str(Var('x'))=='x')
    assert(str(fx)== 'f(x)')    
    assert(str(gfx)== 'g(f(x))')
    assert(str(Mul(fx,gx)) == 'f(x)*g(x)')
    assert(str(Mul(fx,gy)) == 'f(x)*g(y)')
    assert(str(Mul(gfx,gy)) == 'g(f(x))*g(y)')
    z='0.0'
    i='1.0'
    assert(str(Mul(fx,gx).diff_no_simplify(x)) == "f`(x)⊗%s⊗g(x)+f(x)⊗g`(x)⊗%s"%(i,i))
    assert(str(Mul(fx,gy).diff_no_simplify(x)) == "f`(x)⊗%s⊗g(y)+f(x)⊗g`(y)⊗%s"%(i,z))
    assert(str(fx.diff_no_simplify(x))=="f`(x)⊗%s"%i)
    assert(str(gfx.diff_no_simplify(x))=="g`(f(x))⊗f`(x)⊗%s"%i)
    assert(str(gfx.diff_no_simplify(y))=="g`(f(x))⊗f`(x)⊗%s"%z)
    assert(str(Mul(gfx,gy).diff_no_simplify(x)) ==\
           'g`(f(x))⊗f`(x)⊗%s⊗g(y)+g(f(x))⊗g`(y)⊗%s'%(i,z))
    assert(str(Mul(gfx,gy).diff_no_simplify(y)) ==\
           'g`(f(x))⊗f`(x)⊗%s⊗g(y)+g(f(x))⊗g`(y)⊗%s'%(z,i))
    
def test_SimplifyZeroAndOne():
    assert(Mul(Val(1),Val(1)).simplify()==Val(1))
    assert(Mul(Mul(Val(1),Val(1)),Mul(Val(1),Mul(Val(1),Mul(Val(1),Val(1))))).simplify()==Val(1))
    assert(Mul(Mul(Val(1),Val(1)),Mul(Val(1),Mul(Val(1),Mul(Val(1),Val(0))))).simplify()==Val(0))
    x =Var('x')    
    y =Var('y') 
    fx=Fun('f',x)
    fy=Fun('f',y)
    gx=Fun('g',x)
    gy=Fun('g',y)
    hx=Fun('h',x)
    gfx=Fun('g',fx)
    zero=Val(0)    
    one=Val(1)
    assert(str(Mul(one,Add(x,y)))=='1*(x+y)')
    assert(Mul(one,Add(x,y)).expression()=='1*(x+y)')
    assert(str(Mul(one,Add(x,y)).simplify())=='x+y')
    assert(Add(fx, zero).simplify()==fx)
    assert(Mul(fx, zero).simplify()==zero)
    assert(Mul(fx, one).simplify()==Fun('f',Var('x')))
    assert(Mul(zero, gy).simplify()==zero)
    assert(Mul(gx, Mul(fx, zero)).simplify()==zero)
    assert(Mul(gy, Mul(gx, Mul(fx, zero))).simplify()==zero)
    assert(str(fx.diff(x))=='f`(x)')   
    assert(str(Mul(gfx,gy).diff(x))=='g`(f(x))⊗f`(x)⊗g(y)')
    assert(str(Mul(gfx,gy).diff(y))=='g(f(x))⊗g`(y)')
    
    assert(str(Mul(hx,Mul(gx,fx)))=='h(x)*g(x)*f(x)')
    assert(str(Mul(hx,Mul(gx,fx)).diff(x))=='h`(x)⊗g(x)*f(x)+h(x)⊗(g`(x)⊗f(x)+g(x)⊗f`(x))')
    assert(str(Mul(hx,Mul(gx,fx)).diff(y))=='0.0')    
    
def _SimplePhrase():
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
    assert(str(the)=='The')
    
    the_cat=merge(the,cat)
    assert(str(the_cat)=='(The,cat)')    
    assert(the_cat.expression()=='tanh(W_left*w2v(The)+W_right*w2v(cat)+bias)')
    assert(str(the_cat.diff_no_simplify(Var('W_left')))=='')
    assert(the_cat.diff(Var('W_left')).expression()=='tanh`(W_left*w2v(The)+W_right*w2v(cat)+bias)*w2v(The)')    
    
    the_cat_is=merge(the_cat, Word('is'))
    assert(str(the_cat_is)=='((The,cat),is)')
    assert(the_cat_is.expression()=='tanh(W_left*tanh(W_left*w2v(The)+W_right*w2v(cat)+bias)+W_right*w2v(is)+bias)')
    expected='tanh`(W_left*tanh(W_left*w2v(The)+W_right*w2v(cat)+bias)+W_right*w2v(is)+bias)*'+ \
             '(tanh(W_left*w2v(The)+W_right*w2v(cat)+bias)+W_left*tanh`(W_left*w2v(The)+W_right*w2v(cat)+bias)*w2v(The))'
    assert(the_cat_is.diff(Var('W_left')).expression()==expected)
    
    assert(str(merge(the_cat, merge(Word('is'), Word('cute'))))=='((The,cat),(is,cute))')
    
    the_cat_is_cute=merge(the_cat_is, Word('cute'))
    assert(str(the_cat_is_cute)=='(((The,cat),is),cute)')
    expected='tanh(W_left*tanh(W_left*tanh(W_left*w2v(The)+W_right*w2v(cat)+bias)+W_right*w2v(is)+bias)+W_right*w2v(cute)+bias)'
    assert(the_cat_is_cute.expression()==expected)  

def assert_all(x):
    assert(np.all(x))
def test_Evaluation():
    vx=np.matrix([1.0,2.0,3.0])
    vy=np.matrix([2.0,3.0,4.0]).T
    vz=np.matrix([3.0,5.0,7.0])
    x=Var('x')
    x.val=vx
    y=Var('y', vy)
    z=Var('z',vz)
    with pytest.raises(ValueError):
        Mul(x,Var('y',vy.T)).val
    xy=Mul(x,y)
    assert(str(xy)=='x*y')
    assert_all(xy.val==vx.dot(vy))
    x_plus_z=Add(x,z)
    assert(str(x_plus_z)=='x+z')
    assert_all(x_plus_z.val==vx+vz)
    assert_all(Mul(xy,z).val==Mul(z,xy).val)
    assert_all(Mul(xy,z).val==vx.dot(vy)*vz)
    s0=1.57
    s=Var('s',s0)
    fs=Fun('cos',s, np.cos)
    assert(str(fs)=='cos(s)')
    assert(fs.val==np.cos(s0))

def test_CacheKnownValues():
    x=Var('x')    
    fx=Fun('cos', x, np.cos)
    gfx=Fun('exp', fx, np.exp)
    exp_cos=lambda x : np.exp(np.cos(x))
    for v in np.random.random(10):        
        x.val=v
        assert(gfx.val==exp_cos(v))    
    for i in range(100):
        assert(gfx.val==exp_cos(v))
        
    y=Var('y')
    hy=Fun('tanh', y, np.tanh)
    gfx_hy = Mul(gfx, hy)
    exp_cos_x_times_tanh_y = lambda x, y : exp_cos(x)*np.tanh(y)
    vx=5.7
    vy=np.array([1.1,2.1])
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
    x=Var('x')
    y=Var('y')
    assert(np.isnan(Mul(x,y).val))
    x.val=1.0
    assert(np.isnan(Mul(x,y).val))
    y.val=2.0
    assert(Mul(x,y).val==2.0)
    
def test_ParentRelationships():
    x=Var('x')
    fx=Fun('sin',x, np.sin)
    gx=Fun('exp',x, np.exp)
    dfx=fx.diff(x)
    v=1.0
    for v in [1.0,0.5,0.1]:
        x.val=v
        assert(fx.val==np.sin(v))
        assert(gx.val==np.exp(v))    
    
def test_DiffKnownFunctions():
    x=Var('x')
    fx=Fun('sin',x)
    dfx=fx.diff(x)
    print '%r'%dfx
    assert(dfx.expression()=='cos(x)')
    gfx=Fun('exp',Mul(Val(3), fx))
    dgfx=gfx.diff(x)
    assert(dgfx.expression()=='exp(3*sin(x))⊗3⊗cos(x)')
    for v in [1.0, 2.0, 14.2, 5.1, 5.72341] :
        x.val=v
        assert(dfx.val==np.cos(v))
        assert(gfx.val==np.exp(3*np.sin(v)))
        #Allow slight difference for complex numpy expressions.
        np.testing.assert_allclose(dgfx.val,np.exp(3*np.sin(v))*3*np.cos(v), rtol=1e-10, atol=0)
        
    hfx=Fun('log',fx)
    dhfx=hfx.diff(x)
    assert(dhfx.expression()=='1/(sin(x))⊗cos(x)')
    hx=Fun('log',Add(fx, Fun('exp', x)))
    dhx=hx.diff(x)
    assert(dhx.expression()=='1/(sin(x)+exp(x))⊗(cos(x)+exp(x))')
               
def test_matrix_circle_times_operations():
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
        
def test_IdentifyZeroOrIdentityMatrix():
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
    
def test_SimplifyZeroAndIdentityMatrix():
    i=np.identity(5)
    z=np.zeros((5,5))
    assert(IsIdentity(Val(i)))
    assert(Mul(Val(i),Val(i)).simplify()==Val(i))
    assert(Mul(Mul(Val(i),Val(i)),Mul(Val(i),Mul(Val(i),Mul(Val(i),Val(i))))).simplify()==Val(i))
    assert(Mul(Mul(Val(i),Add(Add(Val(z),Val(z)),Val(i))),Mul(Val(i),Mul(Val(i),Mul(Val(i),Val(i))))).simplify()==Val(i))
    assert(Mul(Mul(Val(i),Val(i)),Mul(Val(i),Mul(Val(i),Mul(Val(i),Val(z))))).simplify()==Val(z))

def test_Transpose():
        
    vx=np.matrix([5,1,2])
    vy=np.matrix([1,3,2])
    x=Var('x',vx)
    y=Var('y',vy)
    
    fy=Fun('f',y)
    gx=Fun('g',x)
    fygx=Mul(fy,gx)
    assert(str(Transpose(fygx).simplify())=='(f(y)*g(x)).T')
    assert(str(Transpose(Var('z',2)).simplify())=='z')
    assert(Transpose(Var('z',2)).val==2)
    assert_all(Transpose(x).val==vx.T)
    assert_all(Transpose(Transpose(x)).val==x.val)
    xyt=Mul(x,Transpose(y))
    assert(str(Transpose(xyt))=='(x*(y).T).T')
    assert(xyt.val==12) 
    
def test_MatrixDifferentiation():
    mat=np.matrix
    vw,vx,vb=mat([[1,2,3],[2,3,4]]), mat([[3],[2],[1]]), mat([0.07,0.08])
    w,x,b=Var('W',vw),Var('x',vx),Var('b',vb)
    
    assert_all(w.diff(w).val==np.ones((2,3)))
    wx=Mul(w,x)
    dwx=wx.diff(w)
    xT=Transpose(x)
    assert_all(dwx.val==[[3,2,1],[3,2,1]])
    
    dw=Var('dW',[[0.1,0.2,0.01],[0.2,0.1,0.05]])
    #f(w+dw) = f(w) + dw*dfdw
    #f(w+dw) = Mul(Add(w,dw),x)
    #f(w)=wx
    #dw*dfdw = Mul(CTimes(dw,dwx),x.diff(x))
    assert_all(Mul(Add(w,dw),x).val == Add(wx,Mul(CTimes(dw,dwx),x.diff(x) )).val)
    
    dx=Var('dx',mat([[0.1],[0.2],[0.3]]))
    #f(x+dx) = f(x) + dx*dfdx
    #f(x+dx) = Mul(w,Add(x,dx))
    #f(x)=wx
    #dx*dfdx = Mul(wx.diff(x), dx)
    #print Add(wx, Mul(CTimes(wx.diff(x), Transpose(dx)), x.diff(x)) ).val
    assert_all(Mul(w,Add(x,dx)).val==Add(wx, Mul(wx.diff(x), dx) ).val)
    
    f=Fun('sin', wx)
    g=Fun('cos', wx)
    assert_all(f.diff(w).val==CTimes(Transpose(x), g).val)
    assert_all(f.diff(x).val==CTimes(w, g).val)
    assert_all(CTimes(w, g).val==CTimes(g, w).val)
    
    bw=Mul(b,w)
    h=Fun('sin', bw)
    k=Fun('cos', bw)
    assert_all(h.diff(b).val==CTimes(k, w).val)
    assert_all(h.diff(w).val==CTimes(k, Transpose(b)).val)

def test_FeedForwardNNEvaluation():
    pass
