# -*- coding: utf-8 -*-
import numpy as np
from npRNN.plain import *

def relativeError(base):
    return lambda x : x/base - 1

def show_summary(error, grad,dW):
    v=np.sum(grad*dW)
    grad_wrong=grad.copy()
    np.random.shuffle(grad_wrong)
    v_wrong=np.sum(grad_wrong*dW)
    return np.array([v, error(v), error(v_wrong)])

def rnn1_score(u,Ws,b, word1,word2):
    word12=merge_word([word1,word2], True)
    x0=np.add(np.dot(Ws[0], word12),b)
    h0=np.tanh(x0)
    score= np.dot(u,h0)
    return score

def rnn2_score(u,Ws,b, words, merge_left):
    word12=merge_word(words[:2], merge_left[0])
    x0,h0=activation(Ws[0],b,word12)
    wordLR=merge_word([h0,words[2]], merge_left[1])
    x1,h1=activation(Ws[1],b,wordLR)
    score = np.dot(u,h1)
    return score

def rnn3_score(u,Ws,b, words, merge_left):
    word12=merge_word(words[:2], merge_left[0])
    x0,h0=activation(Ws[0],b,word12)
    wordLR1=merge_word([h0,words[2]], merge_left[1])
    x1,h1=activation(Ws[1],b,wordLR1)
    wordLR2=merge_word([h1,words[3]], merge_left[2])
    x2,h2=activation(Ws[2],b,wordLR2)
    return np.dot(u,h2)
