import numpy as np
from plain import *
from param import Param

float_type = np.float32
int_type = np.int64

class Parser(object):
    def __init__(self, activation_f, activation_df):
        self.activation_f = activation_f
        self.activation_df= activation_df

    def forward(self, words_vec, param):
        W,bias, u_score = param.W_vec, param.bias_vec, param.u_score
        n_iter= len(words_vec)-1
        merge_history=[]
        phrases=np.empty((words_vec.shape[0]-1,words_vec.shape[1]))
        words = words_vec
        scores = np.empty(phrases.shape[0])
        for i in range(n_iter):
            wordLRs = np.concatenate([words[:-1],words[1:]], axis=1)
            xs = hidden_vectorized(W[i],bias[i],wordLRs)
            hs = self.activation_f(xs)
            hs_scores = scoring(u_score, hs)
            loc=np.argmax(hs_scores)
            words = np.concatenate([words[:loc+1], words[loc+2:]],axis=0)
            words[loc]=hs[loc]
            merge_history.append(loc)
            phrases[i]=hs[loc]
            scores[i]=hs_scores[loc]

        whole_words = np.concatenate([words_vec, phrases],axis=0)
        return merge_history, scores, whole_words

    def backward_W_partial(self, node, param):
        left_factor,W,b, =param.u_score, param.W, param.bias
        grads=np.zeros(param.W_vec.shape)
        back_propagation(node, left_factor,W,b, grads, is_W=True)
        return np.sum(grads,0)
    def backward_W(self, phrases, param):
        grad = np.zeros(param.W.shape)
        for node in phrases:
            grad +=self.backward_W_partial(node, param)
        return grad

    def backward_b_partial(self, node, param):
        left_factor,W,b, =param.u_score, param.W, param.bias
        grads=np.zeros(param.bias_vec.shape)
        back_propagation(node, left_factor,W,b, grads, is_W=False)
        return np.sum(grads,0)
    def backward_b(self, phrases, param):
        grad = np.zeros(param.bias.shape)
        for node in phrases:
            grad +=self.backward_b_partial(node, param)
        return grad
    def backward_u(self, phrases, param):
        grad = np.zeros(param.u_score.shape)
        for node in phrases:
            grad += node.vec
        return grad
