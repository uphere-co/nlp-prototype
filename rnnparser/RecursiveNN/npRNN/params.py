import numpy as np
class Params(object):
    '''
    RNN combine two word to a new word(phrase).
    Each new word has its own RNN paramter although the values are identical.
    It is for make back-propagation easier.
    Also, it is convinent to calculate partial derivatives.
    '''
    @classmethod
    def random(cls, n_words, dim, ran=lambda x : (np.random.random(x).astype(np.float32)-0.5)):
        n_phrases = n_words-1
        W=ran((dim,dim*2))
        bias=ran(dim)
        u_score=ran(dim)
        return Params(W,bias,u_score, n_words)

    def __init__(self, W,bias,u_score, n_words=None):
        if n_words:
            repeat= lambda x : np.tile(x,(n_words-1,1,1))
            self.W=repeat(W)
            self.bias=repeat(bias)
            self.u_score=u_score
        else:
            self.W = W
            self.biass = bias
            self.u_score = u_score
    def copy(self):
        return Params(self.W.copy(), self.bias.copy(), self.u_score.copy())
