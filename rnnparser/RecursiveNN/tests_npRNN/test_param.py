# -*- coding: utf-8 -*-
import numpy as np
from npRNN.param import *

def test_random_parameter():
    n_words = 15
    n_phrases = n_words-1
    dim=200
    param=Param.random(dim)
    assert (param.W.shape ==(dim, dim*2))

def _vectorized_parameter():
    ran=lambda x : np.random.random(x).astype(np.float32)-0.5
    n_words = 5
    n_phrases = n_words-1
    dim=10

    W=ran((dim,dim*2))
    bias=ran(dim)
    u_score=ran(dim)

    param = Param(np.tile(W, (n_phrases,1,1)),np.tile(bias,(n_phrases,1,1)),u_score)
    assert (param.W.shape ==(dim, dim*2))
    assert (param.W_vec.shape ==(n_phrases, dim, dim*2))
    param = Param(W,bias,u_score, n_words)
    assert (param.W.shape ==(dim, dim*2))
    assert (param.W_vec.shape ==(n_phrases, dim, dim*2))

    param2=param.copy()
    assert np.all(param2.W==param.W)
    param.W=10
    assert np.all(param2.W!=param.W)

    assert param.bias.shape == (dim,)

def test_modification():
    n_words = 15
    dim=200
    param=Param.random(dim, n_words)
    param2=param.copy()
    param2.W += 1
    assert np.all(param.W +1 == param2.W)
    param.bias *= 2
    param2.bias = param2.bias*2
    assert np.all(param.bias == param2.bias)


def _repeat():
    n_words = 15
    dim=200
    param=Param.random(dim, n_words)
    param2=param.repeat(10)
    assert np.all(param.W==param2.W)
    assert param.W_vec.shape[0]==14
    assert param2.W_vec.shape[0]==9

def test_normalized_ran_param():
    n_words = 15
    dim=200
    param=Param.random(dim, n_words)
    assert np.abs(param.u_score).sum()==1

def test_math_ops():
    n_words = 15
    dim=200
    param =Param.random(dim, n_words)
    param2=Param.random(dim, n_words)
    param3=param.copy()
    param3+= param2
    assert np.all(param3.W==(param.W+param2.W))
    assert np.all(param3.bias==(param.bias+param2.bias))
    assert np.all(param3.u_score==(param.u_score+param2.u_score))

    param =Param.random(dim, n_words)
    param4=param.copy()
    param4 *= 0.1
    assert np.all(param.W*0.1==param4.W)
