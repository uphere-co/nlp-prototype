# -*- coding: utf-8 -*-
import numpy as np
from npRNN.params import *

def test_random_parameter():
    n_words = 15
    n_phrases = n_words-1
    dim=200
    params=Params.random(n_words, dim)
    assert (params.W.shape ==(n_phrases, dim, dim*2))
    assert np.all(params.W[0]==params.W[1])

def test_vectorized_parameter():
    ran=lambda x : np.random.random(x).astype(np.float32)-0.5
    n_words = 15
    n_phrases = n_words-1
    dim=200

    W=ran((dim,dim*2))
    bias=ran(dim)
    u_score=ran(dim)

    params = Params(np.tile(W, (n_phrases,1,1)),np.tile(bias,(n_phrases,1,1)),u_score)
    print params.W.shape
    assert (params.W.shape ==(n_phrases, dim, dim*2))
    params = Params(W,bias,u_score, n_words)
    assert (params.W.shape ==(n_phrases, dim, dim*2))

    params2=params.copy()
    assert np.all(params2.W==params.W)
    params.W[:]=10
    assert np.all(params2.W!=params.W)
