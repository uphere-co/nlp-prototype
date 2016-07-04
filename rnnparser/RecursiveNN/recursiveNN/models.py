
# -*- coding: utf-8 -*-
from nodes import Val,Var,VSF, Add,Dot,Transpose
import numpy as np
#import zmq
from gensim.models import Word2Vec

class Word(object):
#class Word():
    __slots__= ["vec"]
    def __init__(self, word, vec=np.random.random((5,1)) ):

        self.vec = Var(word, vec)#
    def __unicode__(self):
        return self.vec.name
    def toBinaryTreeRep(self):
        return self.vec.name
    def __repr__(self):
        return u"Word(%r)"%(self.vec.name)
    #def __eq__(self)
    def expression(self):
        return 'w2v(%s)'%self.vec.name

class Phrase(object):
#class Phrase():
    __slots__= ["left","right","vec"]
    def __init__(self, left, right, vec):
        self.left = left
        self.right = right
        self.vec = vec
    def __unicode__(self):
        return u'(%s,%s)'%(self.left,self.right)
    def toBinaryTreeRep(self):
        return u'(%s %s)'%(self.left.toBinaryTreeRep(),self.right.toBinaryTreeRep())
    def __repr__(self):
        return "Phrase(%r,%r)"%(self.left,self.right)
    def expression(self):
        return self.vec.expression()

class RecursiveNN(object):
#class RecursiveNN():
    __slots__= ["W","bias","u_score"]
    def __init__(self, W_init, bias_init, u_score_init):
        assert isinstance(W_init, Var), "W should be instance of `Var`"
        assert isinstance(bias_init, Var), "bias should be instance of `Var`"
        self.W=W_init
        self.bias=bias_init
        self.u_score=u_score_init
    def combineTwoNodes(self, left,right):
        #TODO: there can be too many vec... replace Var to Val?
        vec = Var(u'(%s⊕%s)'%(left,right))
        vec.val=np.concatenate([left.vec.val,right.vec.val],0)
        Wxh=Dot(self.W, vec)
        x=Add(Wxh, self.bias)
        vec=VSF('tanh', x)
        phrase=Phrase(left,right, vec)
        return phrase
    def score(self, phrase):
        return Dot(Transpose(self.u_score),phrase.vec)
    def combineToSentence(self, sentence_words):
        nodes=list(sentence_words)
        assert nodes[0] is sentence_words[0]
        def mergeHighestPair(nodes,score):
            if len(nodes)00==1:
                return nodes[0], score
            phrases=[self.combineTwoNodes(x,y) for x,y in zip(nodes,nodes[1:])]
            scores=[self.score(node).val for node in phrases]
            idx_max=np.argmax(scores)
            #print nodes
            phrase=phrases[idx_max]
            nodes.pop(idx_max)
            nodes.pop(idx_max)
            nodes.insert(idx_max, phrase)
            return mergeHighestPair(nodes,Add(score,self.score(phrase)))
        return mergeHighestPair(nodes, Val(0))

class Word2VecFactory(object):
    __slots__= ["model"]
    def __init__(self, w2v_path):
        self.model = Word2Vec.load(w2v_path)
    def getWord2Vec(self, words):
        try:
            words=[Word(word, self.model[word].reshape(-1,1)) for word in words]
                  #\if word in self.model.vocab.keys()]
        except:
            words=[]
        return  words

"""
class Word2VecFactory2(object):
    def __init__(self):
        self.context = zmq.Context()
        self.socket = self.context.socket(zmq.REQ)
        self.socket.connect ("tcp://localhost:10100" )
        self.dict={}
    def getWord2Vec(self, words):
        for word in words:
            if word in self.dict.keys():
                continue
            self.socket.send(word)
            packet=self.socket.recv()
            idx_split=packet.find(',')
            dtype,bytes = packet[:idx_split],packet[idx_split+1:]
            self.dict[word]=np.fromstring(bytes,dtype=dtype).reshape(-1,1)
        return [Word(word, self.dict[word]) for word in words]
"""

import ipyparallel as ipp
class ParallelRNN():
    def __init__(self):
        self.rc = ipp.Client()
        self.dview=self.rc[:]
        dview=self.dview
        dview.block=True
        dview.scatter('engine_uid',self.rc.ids)
        dview.execute('engine_uid=engine_uid[0]')
        dview.execute("""
import os
import sys
import cProfile
import pandas as pd
import numpy as np
import ipyparallel as ipp
from ipyparallel import interactive

sys.path.insert(0, os.environ.get('HOME')+'/nlp-prototype/rnnparser/RecursiveNN/')
from recursiveNN.models import Word,Phrase, RecursiveNN,Word2VecFactory
from recursiveNN.nodes import Val,Var,VSF, Add,Mul,Dot,CTimes,Transpose, reset_NodeDict, NodeDict
from recursiveNN.differentiation import Differentiation
""")
    def SetWord2VecModel(self,model):
        self.dview['word2vec']=model
    def SetTestSentences(self,sents_test):
        self.dview.scatter('sents_test', sents_test)
        self.dview.execute("sents_vecs_test=[[Word(word, np.expand_dims(word2vec[word].values,1)) for word in line.split()] for line in sents_test]")
    def SetParams(self,W,bias,u_score):
        self.dview['vw0']=W.val
        self.dview['vb0']=bias.val
        self.dview['vu0']=u_score.val
        self.dview.execute('''
W,bias,u_score=Var('W'),Var('b'),Var('u')
W.val,bias.val,u_score.val=vw0,vb0,vu0
rnn=RecursiveNN(W,bias,u_score)
''')
    def EvalTestScores(self):
        self.dview.execute("eval_test=[rnn.combineToSentence(sent) for sent in sents_vecs_test]")
        self.dview.execute("scores_test=[x.val for _,x in eval_test]")
        return np.array(self.dview.gather("scores_test")).reshape(-1,1)
