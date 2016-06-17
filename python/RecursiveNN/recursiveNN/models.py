
# -*- coding: utf-8 -*-
from nodes import Word,Phrase, Val,Var,VSF, Add,Mul

class RecursiveNN:
    def __init__(self, W_left_init, W_right_init, bias_init):
        assert isinstance(W_left_init, Var), "W_left should be instance of `Var`"
        assert isinstance(W_right_init, Var), "W_right should be instance of `Var`"
        assert isinstance(bias_init, Var), "bias should be instance of `Var`"
        self.W_left=W_left_init
        self.W_right=W_right_init
        bias = Word('!'+bias_init.name+'_vec!')
        bias.expr='bias'
        bias.vec=bias_init        
        self.bias=bias
    def combineTwoNodes(self, left,right):        
        Wxh_left=Mul(self.W_left, left)
        Wxh_right=Mul(self.W_right, right)
        x=Add(Add(Wxh_left,Wxh_right), self.bias)
        vec=VSF('tanh', x)
        phrase=Phrase(left,right, vec)        
        return phrase
        
class Word2VecFactory:
    def __init__(self):
        pass

