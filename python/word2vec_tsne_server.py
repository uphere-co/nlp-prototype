#%pylab 
#Following is alternative for %pylab magic.
from matplotlib import pylab, mlab, pyplot
from IPython.core.pylabtools import figsize, getfigs
from pylab import *

import seaborn
import numpy as np
import pandas as pd
from sklearn.manifold import TSNE
from sklearn.decomposition import PCA

from gensim.models import Word2Vec
import os 
import zmq
context = zmq.Context()
socket = context.socket(zmq.REP)
port=10101
socket.bind("tcp://*:%s" % port)

w2v_path = os.environ.get('WORD2VEC_MODEL')
if not w2v_path:
    w2v_path = os.environ.get('HOME')+'/arxiv/abstracts.model'
    print "Set env. variable WORD2VEC_MODEL to path of a Word2Vec model."

print 'Read Word2Vec model from %s'%(w2v_path)
model_w2v = Word2Vec.load(w2v_path)

def WordsToVecs(model, words):
        return np.array([model[word] for word in words], dtype='float')

idx=0
while True:
    query_words = socket.recv().split()
    vecs_query = WordsToVecs(model_w2v, query_words)
    #tsne_query = TSNE(n_components=2, random_state=0).fit_transform(vecs_query) 
    tsne_query = PCA(n_components=2).fit_transform(vecs_query) 

    figsize(8,8)
    df=pd.DataFrame(tsne_query, columns=['x','y'])
    df['word']=query_words
    ax=df.plot(kind='scatter', x='x',y='y')

    for _,elm in df.iterrows():
        ax.annotate(elm.word, xy = (elm.x, elm.y), fontsize=14)
    fig_dir = '/data/groups/uphere/figs/'
    idx = (idx+1)%10
    fig_fullpath = fig_dir+'w2v_tsne_%d.png'%idx
    savefig(fig_fullpath)    
    socket.send_string(fig_fullpath)    