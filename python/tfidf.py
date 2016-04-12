import nltk
import string
import os
import re

from nltk.corpus import stopwords
from nltk.stem.porter import PorterStemmer
from collections import Counter

from sklearn.feature_extraction.text import TfidfVectorizer

stemmer = PorterStemmer()
stop_words = [s.encode('utf-8') for s in stopwords.words('english')]

docs = []
tfmap = Counter()

def stem_tokens(tokens, stemmer):
    stemmed = []
    for item in tokens:
        stemmed.append(stemmer.stem(item))
    return stemmed

def get_tokens(doc):
    tokens = nltk.word_tokenize(doc)
    filtered_tokens = [w.decode('utf-8') for w in tokens if not w in stop_words]
    stems = stem_tokens(filtered_tokens, stemmer)
    return stems

filename = "parsed_ipg160105.txt"

r = open(filename,'r')
text = r.read().lower().split("==============")
length = len(text)

for i in range(length):
    if (i==length):
        break;
    if (i%3) == 0:
        context = text[i].translate(None,string.punctuation)
    if (i%3) == 1:
        nth = text[i].split(':')[0]
        docid = text[i].split(':')[1]
        docs.append([nth,docid,context])

for doc in docs[0:100]:
    tokens = get_tokens(doc[2])
    count = Counter(tokens)
    print doc[0] + ":" + doc[1]
    tfmap = tfmap + count

print tfmap

#tokens = get_tokens("parsed_ipg160105.txt")
#count = Counter(tokens)

#print count.most_common(100)
