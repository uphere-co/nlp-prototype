import nltk
import string

from nltk.corpus import stopwords
from collections import Counter

def get_tokens(filename):
    r = open(filename,'r')
    text = r.read().lower().translate(None, string.punctuation)
    tokens = nltk.word_tokenize(text)
    return tokens

tokens = get_tokens("parsed_ipg160105.txt")
# tokens = get_tokens("test.txt")
stop_words = [s.encode('utf-8') for s in stopwords.words('english')]
filtered = [w for w in tokens if not w in stop_words]
count = Counter(filtered)
print count.most_common(100)
