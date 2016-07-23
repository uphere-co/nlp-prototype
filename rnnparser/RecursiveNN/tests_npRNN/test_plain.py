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

def check_summary(f,grad,delta, rtol):
    v, error, error_ran = show_summary(f,grad,delta)
    print v, error, error_ran
    assert np.abs(error) < rtol
def test_run():
    np.random.seed(132)
    ran=lambda x : np.random.random(x).astype(np.float64)-0.5
    vecs=ran((5,200))
    delta= lambda x ,scale: scale*ran(x.shape)
    W=ran((200,400))
    b=ran((200,))
    u=ran((200,))
    scale=0.0001
    dW=delta(W,scale)
    db=delta(b,scale)
    du=delta(u,scale)

    rtol= scale*100
    word1,word2,word3,word4=vecs[0],vecs[1],vecs[2],vecs[3]
    print 'RNN2'
    rnn2_merge_left=[True,False]
    score,grad_Ws = rnn_single_path(u,[W,W],b, vecs[:3], rnn2_merge_left)
    gradW0,gradW1=grad_Ws
    diff_rnn2= lambda x : x - score
    f=relativeError(diff_rnn2(rnn2_score(u,[W+dW,W],b, vecs[:3], rnn2_merge_left)))
    print show_summary(f,gradW0,dW)
    f=relativeError(diff_rnn2(rnn2_score(u,[W,W+dW],b, vecs[:3], rnn2_merge_left)))
    print show_summary(f,gradW1,dW)
    print score, rnn2_score(u,[W,W],b, vecs[:3], rnn2_merge_left)
    print 'RNN3:'
    rnn3_merge_left=[True,True,False]
    score,grad_Ws = rnn_single_path(u,[W,W,W],b, vecs[:4], rnn3_merge_left)
    gradW0,gradW1,gradW2 = grad_Ws
    diff_rnn3= lambda x : x - score
    f=relativeError(diff_rnn3(rnn3_score(u,[W+dW,W,W],b, vecs[:4], rnn3_merge_left)))
    check_summary(f,gradW0,dW, rtol)
    f=relativeError(diff_rnn3(rnn3_score(u,[W,W+dW,W],b, vecs[:4], rnn3_merge_left)))
    check_summary(f,gradW1,dW, rtol)
    f=relativeError(diff_rnn3(rnn3_score(u,[W,W,W+dW],b, vecs[:4], rnn3_merge_left)))
    check_summary(f,gradW2,dW, rtol)

    print score/rnn3_score(u,[W,W,W],b, vecs[:4], rnn3_merge_left)
