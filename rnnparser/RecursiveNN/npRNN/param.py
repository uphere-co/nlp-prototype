# -*- coding: utf-8 -*-
import numpy as np

normalize_L1=lambda x : x/np.abs(x).sum()
class Param(object):
    '''
    W_vec, bias_vec:
        RNN combine two word to a new word(phrase).
        Each new word has its own RNN paramter although the values are identical.
        It is for make back-propagation easier.
        Also, it is convinent to calculate partial derivatives.

    W, bias :
        For not separating out values and use one value for all,
        it should be in first elements of vectors.
    '''
    @classmethod
    def random(cls, dim,n_words=2, ran=lambda x : (np.random.random(x).astype(np.float32)-0.5),
               norm = normalize_L1):
        W=ran((dim,dim*2))
        bias=ran(dim)
        u_score=norm(ran(dim))
        return cls(W,bias,u_score, n_words)
    @classmethod
    def from_arr(cls, arr):
        return cls(arr[:-2].T, arr[-2], arr[-1])

    def __init__(self, W,bias,u_score, n_words=None):
        self.bias = bias
        self.W = W
        self.u_score = normalize_L1(u_score)
    def __iadd__(self, val):
        self.W += val.W
        self.bias += val.bias
        self.u_score += val.u_score
        return self
    def __imul__(self, val):
        self.W *= val
        self.bias *= val
        self.u_score *= val
        return self
    def copy(self):
        return Param(self.W.copy(), self.bias.copy(), self.u_score.copy())
    def zero(self):
        return Param(np.zeros(self.W.shape),np.zeros(self.bias.shape), np.zeros(self.u_score.shape))
    def iter_params(self):
        return [self.W, self.bias, self.u_score]
    def to_arr(self):
        arr = np.vstack([self.W.T, self.bias, self.u_score])
        return arr
