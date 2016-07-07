import os
import sys
import pandas as pd
sys.path.insert(0, os.environ.get('HOME')+'/nlp-prototype/rnnparser/RecursiveNN/')
from recursiveNN.models import Word2VecFactory

nlp_data_path = os.environ.get('HOME')+'/nlp-data/word2vec-dataset/'
w2v_factory=Word2VecFactory(nlp_data_path+'1b.training.15epochs.model')
vocab_total=w2v_factory.model.index2word
with open('/home/jihuni/word2vec/1b.training.voca', 'w') as f:
     for word in vocab_total:
        f.write((word+u'\n').encode('utf-8'))        
        
df=pd.DataFrame([w2v_factory.model[word] for word in vocab_total], dtype=np.float32)
df.index=vocab_total
#assert df.values.flags.c_contiguous
assert df.values.flags.aligned

for word in vocab_total[:100]:
    assert np.all(df.ix[word].values == w2v_factory.model[word])
    
df.to_pickle('/home/jihuni/word2vec/1b.training.15epochs.pickle')    
