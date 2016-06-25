import zmq
context = zmq.Context()
socket = context.socket(zmq.REP)
port=10100
socket.bind("tcp://*:%s" % port)

from gensim.models import Word2Vec
import os 
w2v_path = os.environ.get('WORD2VEC_MODEL')
if not w2v_path:
    w2v_path = os.environ.get('HOME')+'/arxiv/abstracts.model'
    print "Set env. variable WORD2VEC_MODEL to path of a Word2Vec model."

print 'Read Word2Vec model from %s'%(w2v_path)
word2vec_arxiv = Word2Vec.load(w2v_path)

while True:
    query_words = socket.recv().split()
    try:
        similar_words_with_scores = word2vec_arxiv.most_similar(query_words)
        similar_words = [word for word,_ in similar_words_with_scores]
        answer_str = ' '.join(similar_words)    
    except:
        answer_str = ''
    socket.send_string(answer_str)
    