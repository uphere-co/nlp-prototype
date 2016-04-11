import nltk
import string
import os

from nltk.corpus import stopwords
from nltk.stem.porter import PorterStemmer
from collections import Counter

from sklearn.feature_extraction.text import TfidfVectorizer

stemmer = PorterStemmer()
stop_words = [s.encode('utf-8') for s in stopwords.words('english')]

def stem_tokens(tokens, stemmer):
    stemmed = []
    for item in tokens:
        stemmed.append(stemmer.stem(item))
    return stemmed

def get_tokens(filename):
    r = open(filename,'r')
    text = r.read().lower().translate(None,string.punctuation)
    tokens = nltk.word_tokenize(text)
    filtered_tokens = [w.decode('utf-8') for w in tokens if not w in stop_words]
    stems = stem_tokens(filtered_tokens, stemmer)
    return stems

tokens = get_tokens("parsed_ipg160105.txt")
count = Counter(tokens)

print count.most_common(100)
