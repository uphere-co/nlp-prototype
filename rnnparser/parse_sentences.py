import os
import sys
import numpy as np
import pandas as pd
from datetime import datetime

myPath = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, myPath + '/RecursiveNN/')

from recursiveNN.models import Word,Phrase, RecursiveNN,Word2VecFactory
from recursiveNN.nodes import Val,Var,VSF, Add,Mul,Dot,CTimes,Transpose, reset_NodeDict
from recursiveNN.differentiation import Differentiation

input_path=sys.argv[1]
output_path=sys.argv[2]
w2v_path = os.environ.get('HOME')+'/word2vec/1b.training.15epochs.model'
print 'Read and parse: ', input_path
print 'Write to: ', output_path

with open(input_path, 'r') as f:
    short_sents=[line.strip().decode('utf-8') for line in f.readlines()]
w2v_factory=Word2VecFactory(w2v_path)

sents=[w2v_factory.getWord2Vec(line.split()) for line in short_sents]
sents = [x for x in sents if len(x)>0]

def InitialRNN():
    ran=lambda x : np.random.random(x)-0.5
    n=200
    h0,w0,b0,u0     =Var('h0'),Var('w0'),Var('b0'),Var('u0')
    vh0,vw0,vb0,vu0 =ran((n,1)),ran((n,2*n)),ran((n,1)),ran((n,1))
    h0.val,w0.val,b0.val,u0.val=vh0,vw0,vb0,vu0
    u0.val /=  np.abs(u0.val).sum()  #L1 normalization.
    return RecursiveNN(w0,b0,u0)

rnn=InitialRNN()

t_start = datetime.now()
eval_test = [rnn.combineToSentence(sent) for sent in sents]
#eval_test = [rnn.combineToSentence(sent) for sent in sents[:100]]
t_end = datetime.now()
print t_end-t_start

with open(output_path, 'w') as f:
    for sent,_ in eval_test:
        f.write((sent.toBinaryTreeRep()+u'\n').encode('utf-8'))
