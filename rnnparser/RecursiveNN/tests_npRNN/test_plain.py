# -*- coding: utf-8 -*-
import numpy as np
from npRNN.plain import *
from npRNN.testing_helper import *

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
    print show_summary(f,gradW0,dW)
    f=relativeError(diff_rnn3(rnn3_score(u,[W,W+dW,W],b, vecs[:4], rnn3_merge_left)))
    print show_summary(f,gradW1,dW)
    f=relativeError(diff_rnn3(rnn3_score(u,[W,W,W+dW],b, vecs[:4], rnn3_merge_left)))
    print show_summary(f,gradW2,dW)

    print score/rnn3_score(u,[W,W,W],b, vecs[:4], rnn3_merge_left)
    assert 0
