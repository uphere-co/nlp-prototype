import nltk
import string
import os
import re

import numpy as np

from nltk.corpus import stopwords
# from nltk.stem.porter import PorterStemmer
from collections import Counter

from sklearn.feature_extraction.text import *

#stemmer = PorterStemmer()
stop_words = [s.encode('utf-8') for s in stopwords.words('english')]

path = '/home/mo/repo/src/topic-modeling-for-US-patents/python/data'
docs = []
tfmap = Counter()
dict = {}

#def stem_tokens(tokens, stemmer):
#    stemmed = []
#    for item in tokens:
#        stemmed.append(stemmer.stem(item))
#    return stemmed

def get_tokens(text):
    tokens = nltk.word_tokenize(text)
    filtered_tokens = [w for w in tokens if not w in stop_words]
    # stems = stem_tokens(filtered_tokens, stemmer)
    return filtered_tokens



#filename = "smalltest.txt"
filename = "parsed_ipg160105.txt"

r = open(filename,'r')

splitter14 = "="*14
splitter38 = "="*38

text = r.read().lower().replace(splitter38,"DOCUMENTSPLITTER").replace(splitter14,"DOCUMENTSPLITTER").replace(":","\n DOCOLLONS \n").translate(None,string.punctuation)

print "Finished Reading!"

tokens = get_tokens(text)

context = []
check0 = True
check1 = False
check2 = False

print "Finished tokenizing!"

n = 0

for word in tokens:
    if ("DOCUMENTSPLITTER" in word and check0):
        nth = word.replace("DOCUMENTSPLITTER","")
        check1 = True
        check0 = False
        continue
    if check1:
        check1 = False
        check2 = True
        continue
    if check2:
        docid = word.replace("DOCUMENTSPLITTER","")
        check0 = True
        check2 = False
        docs.append([nth,docid,context])
        n = n+1
        context = []
        continue

    if n==300:
        break
    
    if (not word == 'DOCOLLONS'):
        context.append(word)

for doc in docs[0:300]:
    tokens = doc[2]
    #count = Counter(tokens)
    dict[(doc[0],doc[1])] = ' '.join(doc[2])
    #print doc[0] + ":" + doc[1]
    #tfmap = tfmap + count # Really Slow Process!!

#print dict
#print tfmap.values()

"""
for subdir, dirs, files in os.walk(path):
    for file in files:
        file_path = subdir + os.path.sep + file
        r = open(file_path,'r')
        text = r.read().lower().translate(None, string.punctuation)
        dict[file] = text
"""


tfidf = TfidfVectorizer(tokenizer=get_tokens, stop_words='english')
tfs = tfidf.fit_transform(dict.values())

m_size = len(tfs.toarray())
res = np.zeros(shape=(m_size,m_size))

r = open('result.txt','w')

for k in range(m_size):
    for j in range(k+1):
        res[k][j] = (tfs.toarray()[k]*tfs.toarray()[j]).sum()
        res[j][k] = res[k][j]

for i in range(m_size):
    for j in range(m_size):
        r.write((str)('{0:.2g}'.format(res[i][j])) + "  ")
    r.write("\n")

r.close()

#print tfs.get_feature_names()


#print dict.values()
#print dict

"""
count_vectorizer = CountVectorizer(stop_words='english')
count_vectorizer.fit_transform(train_set)
print count_vectorizer.vocabulary_

freq_term_matrix = count_vectorizer.transform(test_set)
print freq_term_matrix.todense()

tfidf = TfidfTransformer(norm="l12")
tfidf.fit(freq_term_matrix)
print tfidf.idf_
"""
#print dict
#print tfidf.get_feature_names()
#print tfs.toarray()
