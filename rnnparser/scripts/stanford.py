import sys
import nltk
from nltk.parse.stanford import StanfordParser
from nltk.parse.stanford import StanfordDependencyParser
from nltk.parse.stanford import StanfordNeuralDependencyParser
from nltk.tag.stanford import StanfordPOSTagger, StanfordNERTagger
from nltk.tokenize.stanford import StanfordTokenizer

#stanford = StanfordParser(model_path="/home/jihuni/word2vec/model/edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz")
#stanford = StanfordParser(stanford = StanfordParser(model_path="edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz"))
stanford_parser_jar = '/data/groups/uphere/parsers/corenlp/stanford-parser-full-2015-12-09/stanford-parser.jar'
stanford_model_jar  = '/data/groups/uphere/parsers/corenlp/stanford-parser-full-2015-12-09/stanford-parser-3.6.0-models.jar'
stanford = StanfordParser(path_to_jar=stanford_parser_jar, 
                          path_to_models_jar=stanford_model_jar)


def IsLeaf(node):
    #From NLTK documentation:
    #    containing no children is 1; the height of a tree
    #    containing only leaves is 2; and the height of any other
    return node.height()==2

def ToASCIIstring(node):
    if IsLeaf(node):
        return node[0]
    return '(%s %s)'%(ToASCIIstring(node[0]), ToASCIIstring(node[1]))

def ToBinaryTreeStr(tree):
    tree.collapse_unary(collapsePOS=True, collapseRoot=True, joinChar="+")
    tree.chomsky_normal_form()
    return ToASCIIstring(tree)

if __name__ == '__main__':
    filename = sys.argv[1]
    sentences=open(filename, 'r').readlines()    
    word_lists=[x.decode('utf-8').split() for x in sentences]
    trees = [list(x)[0] for x in stanford.parse_sents(word_lists)]
    strs = [ToBinaryTreeStr(list(tree)[0]).encode('utf-8') for tree in trees]
    with open(filename+'.stanford', 'w') as f:
        for line in strs:
            f.write(line+'\n')