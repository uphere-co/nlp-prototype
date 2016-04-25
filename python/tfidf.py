import nltk
import string
import os
import re

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



filename = "smalltest.txt"
#filename = "parsed_ipg160105.txt"

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
        context = []
        continue

    if (not word == 'DOCOLLONS'):
        context.append(word)

for doc in docs[0:1500]:
    #print doc[0]
    dict[(doc[0],doc[1])] = ' '.join(doc[2])
    #print doc[0] + ":" + doc[1]
    #tfmap = tfmap + count # Really Slow Process!!

#print dict
#print tfmap

"""
for subdir, dirs, files in os.walk(path):
    for file in files:
        file_path = subdir + os.path.sep + file
        r = open(file_path,'r')
        text = r.read().lower().translate(None, string.punctuation)
        dict[file] = text
"""


#tfidf = TfidfVectorizer(tokenizer=get_tokens, stop_words='english')
#tfs = tfidf.fit_transform(dict.values())
#print dict.values()
#print dict

train_set = ("The sky is blue.", "The sun is bright.")
test_set = ("The sun in the sky is bright.", "We can see the shining sun, the bright sun.")

print test_set

count_vectorizer = CountVectorizer(stop_words='english')
count_vectorizer.fit_transform(train_set)
print count_vectorizer.vocabulary_

freq_term_matrix = count_vectorizer.transform(test_set)
print freq_term_matrix.todense()

tfidf = TfidfTransformer(norm="l12")
tfidf.fit(freq_term_matrix)
print tfidf.idf_

#print dict
#print tfidf.get_feature_names()
#print tfs.toarray()

count = Counter(get_tokens(dict[('0','d0746541')]))

#tokens = get_tokens("parsed_ipg160105.txt")
#count = Counter(tokens)

#print count.most_common(100)
