import numpy as np
from plain import *

float_type = np.float32
int_type = np.int64

class Params(object):
    def __init__(self, W,bias,u_score):
        self.W = W
        self.bias = bias
        self.u_score = u_score

class Parser(object):
    def __init__(self, params, activation_f, activation_df):
        self.params       = params
        self.activation_f = activation_f
        self.activation_df= activation_df

    def forward(self, words_vec, Ws=None):
        n_iter= len(words_vec)-1
        if Ws is None:
            Ws=[self.params.W]*n_iter
        merge_history=[]
        phrases=np.empty((words_vec.shape[0]-1,words_vec.shape[1]))
        words = words_vec
        scores = np.empty(phrases.shape[0])
        for i in range(n_iter):
            wordLRs = np.concatenate([words[:-1],words[1:]], axis=1)
            xs = hidden_vectorized(Ws[i],self.params.bias,wordLRs)
            hs = self.activation_f(xs)
            hs_scores = scoring(self.params.u_score, hs)
            loc=np.argmax(hs_scores)
            words = np.concatenate([words[:loc+1], words[loc+2:]],axis=0)
            words[loc]=hs[loc]
            merge_history.append(loc)
            phrases[i]=hs[loc]
            hs_scores = scoring(self.params.u_score, hs)
            scores[i]=hs_scores[loc]

        whole_words = np.concatenate([words_vec, phrases],axis=0)
        return merge_history, scores, whole_words
