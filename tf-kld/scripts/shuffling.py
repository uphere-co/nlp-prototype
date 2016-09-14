import nltk
import random
from nltk.stem.porter import PorterStemmer

stemmer = PorterStemmer()

def stem_tokens(tokens, stemmer):
    stemmed = []
    for item in tokens:
        stemmed.append(stemmer.stem(item))
    return stemmed

# For train file

fin = open("msr_paraphrase_train.txt",'r')
fout = open("Tk_msr_paraphrase_train.txt", 'w')

id1 = []
id2 = []
sen1 = []
sen2 = []

for line in fin:
    items = line.lower().split('\t')
    id1.append(items[1])
    id2.append(items[2])
    sen1.append(' '.join(nltk.word_tokenize(items[3])))
    sen2.append(' '.join(nltk.word_tokenize(items[4])))

fout.write("Quality"+'\t'+"#1 ID"+'\t'+"#2 ID"+'\t'+sen1[0]+'\t'+sen2[0]+'\n')

random.shuffle(sen1)
random.shuffle(sen2)

for i in xrange(len(sen1)-1):
    fout.write("0"+'\t'+id1[i+1]+'\t'+id2[i+1]+'\t'+sen1[i+1]+'\t'+sen2[i+1]+'\n')

# For test file

fin2 = open("msr_paraphrase_test.txt",'r')
fout2 = open("Tk_msr_paraphrase_test.txt", 'w')

id1 = []
id2 = []
sen1 = []
sen2 = []

for line in fin2:
    items = line.lower().split('\t')
    id1.append(items[1])
    id2.append(items[2])
    sen1.append(' '.join(nltk.word_tokenize(items[3])))
    sen2.append(' '.join(nltk.word_tokenize(items[4])))

fout2.write("Quality"+'\t'+"#1 ID"+'\t'+"#2 ID"+'\t'+sen1[0]+'\t'+sen2[0]+'\n')

random.shuffle(sen1)
random.shuffle(sen2)

for i in xrange(len(sen1)-1):
    fout2.write("0"+'\t'+id1[i+1]+'\t'+id2[i+1]+'\t'+sen1[i+1]+'\t'+sen2[i+1]+'\n')
