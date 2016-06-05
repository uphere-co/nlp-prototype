import os
import sys
myPath = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, myPath + '/../')

from recursiveNN.nodes import Word,Phrase, Val,Var,Fun, Add,Mul
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
    assert(str(Mul(fx,gx).diff_no_simplify(x)) == "f`(x)*1*g(x)+f(x)*g`(x)*1")
    assert(str(Mul(fx,gy).diff_no_simplify(x)) == "f`(x)*1*g(y)+f(x)*g`(y)*0")
    assert(str(fx.diff_no_simplify(x))=="f`(x)*1")
    assert(str(gfx.diff_no_simplify(x))=="g`(f(x))*f`(x)*1")
    assert(str(gfx.diff_no_simplify(y))=="g`(f(x))*f`(x)*0")
    assert(str(Mul(gfx,gy).diff_no_simplify(x)) == 'g`(f(x))*f`(x)*1*g(y)+g(f(x))*g`(y)*0')
    assert(str(Mul(gfx,gy).diff_no_simplify(y)) == 'g`(f(x))*f`(x)*0*g(y)+g(f(x))*g`(y)*1')
    
def test_SimplifyZeroAndOne():
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
    assert(str(Mul(one,Add(x,y)).simplify())=='(x+y)')
    assert(Add(fx, zero).simplify()==fx)
    assert(Mul(fx, zero).simplify()==zero)
    assert(Mul(fx, one).simplify()==Fun('f',Var('x')))
    assert(Mul(zero, gy).simplify()==zero)
    assert(Mul(gx, Mul(fx, zero)).simplify()==zero)
    assert(Mul(gy, Mul(gx, Mul(fx, zero))).simplify()==zero)
    assert(str(Mul(gfx,gy).diff(x))=='g`(f(x))*f`(x)*g(y)')
    assert(str(Mul(gfx,gy).diff(y))=='g(f(x))*g`(y)')
    
    assert(str(Mul(hx,Mul(gx,fx)))=='h(x)*g(x)*f(x)')
    assert(str(Mul(hx,Mul(gx,fx)).diff(x))=='h`(x)*g(x)*f(x)+h(x)*(g`(x)*f(x)+g(x)*f`(x))')
    assert(str(Mul(hx,Mul(gx,fx)).diff(y))=='0')
    
def test_SimplePhrase():
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
    
def test_CacheKnownValues():
    pass