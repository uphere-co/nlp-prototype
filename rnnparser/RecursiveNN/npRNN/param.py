# -*- coding: utf-8 -*-
import numpy as np
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
    def random(cls, n_words, dim, ran=lambda x : (np.random.random(x).astype(np.float32)-0.5)):
        W=ran((dim,dim*2))
        bias=ran(dim)
        u_score=ran(dim)
        return Param(W,bias,u_score, n_words)

    def __init__(self, W,bias,u_score, n_words=None):
        if n_words:
            repeat= lambda x : np.tile(x,(n_words-1,1,1))
            self._W=np.tile(W,(n_words-1,1,1))
            self._bias=np.tile(bias,(n_words-1,1))
            self._u_score=u_score
        else:
            self._bias = bias
            self._W = W
            self._u_score = u_score
    @property
    def W_vec(self):
        return self._W
    @property
    def bias_vec(self):
        return self._bias
    @property
    def W(self):
        return self._W[0]
    @property
    def bias(self):
        return self._bias[0]
    @property
    def u_score(self):
        return self._u_score
    @W_vec.setter
    def W_vec(self,val):
        self._W[:] = val
    @bias_vec.setter
    def bias_vec(self,val):
        self._bias[:]=val
    @W.setter
    def W(self,val):
        self._W[:] = val
    @bias.setter
    def bias(self,val):
        self._bias[:]=val
    @u_score.setter
    def u_score(self,val):
        self._u_score[:]=val
    def copy(self):
        return Param(self._W.copy(), self._bias.copy(), self._u_score.copy())
    def iter_params(self):
        return [self.W, self.bias, self.u_score]
    def repeat(self,n_words):
        return Param(self.W,self.bias,self.u_score, n_words)
