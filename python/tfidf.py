import nltk
import string
import os
import re

from nltk.corpus import stopwords
# from nltk.stem.porter import PorterStemmer
from collections import Counter

from sklearn.feature_extraction.text import TfidfVectorizer

#stemmer = PorterStemmer()
stop_words = [s.encode('utf-8') for s in stopwords.words('english')]

docs = []
tfmap = Counter()

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

for doc in docs:
    tokens = doc[2]
    count = Counter(tokens)
    print doc[0] + ":" + doc[1]
    tfmap = tfmap + count # Really Slow Process!!


print tfmap

#tokens = get_tokens("parsed_ipg160105.txt")
#count = Counter(tokens)

#print count.most_common(100)
