import os
import sys
import ipyparallel as ipp

rc = ipp.Client()
dview=rc[:]
dview.block=True

nlp_data_path = os.environ.get('HOME')+'/nlp-data/word2vec-dataset/'
with open(nlp_data_path+'1b.training.short_sentences', 'r') as f:
    sents=[line.strip().decode('utf-8') for line in f.readlines()]
dview.scatter('sents', sents)

with open('/home/jihuni/word2vec/1b.training.voca', 'r') as f:
    vocab_total=[line.strip().decode('utf-8') for line in f.readlines()]    
dview['vocab_total']=vocab_total

#Following dview.execute takes about an hour!
dview.execute("sents_known = [sent for sent in sents if np.all([word in vocab_total for word in sent.split()])]")
sents_known=dview.gather('sents_known')
with open(nlp_data_path+'1b.training.short_sentences.known', 'w') as f:
     for sent in sents_known:
        f.write((sent+u'\n').encode('utf-8'))
