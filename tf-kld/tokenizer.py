import nltk
from nltk.stem.porter import PorterStemmer

fin = open("msr_paraphrase_train.txt",'r')
fout = open("Tk_msr_paraphrase_train.txt", 'w')

stemmer = PorterStemmer()

def stem_tokens(tokens, stemmer):
    stemmed = []
    for item in tokens:
        stemmed.append(stemmer.stem(item))
    return stemmed

for line in fin:
    items = line.lower().split('\t')
    sen1 = ' '.join(nltk.word_tokenize(items[3]))
    sen2 = ' '.join(nltk.word_tokenize(items[4]))
    fout.write(items[0]+'\t'+items[1]+'\t'+items[2]+'\t'+sen1+'\t'+sen2+'\n')
    #sen1 = ' '.join(stem_tokens(nltk.word_tokenize(unicode(items[3], 'utf-8')), stemmer))
    #sen2 = ' '.join(stem_tokens(nltk.word_tokenize(unicode(items[4], 'utf-8')), stemmer))
    #fout.write(items[0]+'\t'+items[1]+'\t'+items[2]+'\t'+str(sen1.encode('utf-8'))+'\t'+str(sen2.encode('utf-8'))+'\n')



    
fin2 = open("msr_paraphrase_test.txt",'r')
fout2 = open("Tk_msr_paraphrase_test.txt", 'w')

for line in fin2:
    items = line.lower().split('\t')
    sen1 = ' '.join(nltk.word_tokenize(items[3]))
    sen2 = ' '.join(nltk.word_tokenize(items[4]))
    fout2.write(items[0]+'\t'+items[1]+'\t'+items[2]+'\t'+sen1+'\t'+sen2+'\n')
    #sen1 = ' '.join(stem_tokens(nltk.word_tokenize(unicode(items[3], 'utf-8')), stemmer))
    #sen2 = ' '.join(stem_tokens(nltk.word_tokenize(unicode(items[4], 'utf-8')), stemmer))
    #fout2.write(items[0]+'\t'+items[1]+'\t'+items[2]+'\t'+str(sen1.encode('utf-8'))+'\t'+str(sen2.encode('utf-8'))+'\n')
