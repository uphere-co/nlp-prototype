# -*- coding: utf-8 -*-
import numpy as np
from npRNN.param import *

def test_random_parameter():
    n_words = 15
    n_phrases = n_words-1
    dim=200
    param=Param.random(n_words, dim)
    assert (param.W.shape ==(dim, dim*2))
    assert (param.W_vec.shape ==(n_phrases, dim, dim*2))

def test_vectorized_parameter():
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

    assert param.bias_vec.shape == (n_phrases,dim)
    assert param.bias.shape == (dim,)

def test_modification():
    n_words = 15
    dim=200
    param=Param.random(n_words, dim)
    param2=param.copy()
    param2.W += 1
    param.W_vec += 1
    assert np.all(param.W == param2.W)
    param.bias *= 2
    param2.bias = param2.bias*2
    assert np.all(param.bias == param2.bias)


def test_repeat():
    n_words = 15
    dim=200
    param=Param.random(n_words, dim)
    param2=param.repeat(10)
    assert np.all(param.W==param2.W)
    assert param.W_vec.shape[0]==14
    assert param2.W_vec.shape[0]==9
