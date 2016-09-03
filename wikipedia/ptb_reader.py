def IsLeaf(node):
    #From NLTK documentation:
    #    containing no children is 1; the height of a tree
    #    containing only leaves is 2; and the height of any other
    return node.height()==2

def ToString(node):
    if IsLeaf(node):
        return node[0]
    return '(%s %s)'%(ToString(node[0]), ToString(node[1]))

def ToBinaryTreeStr(tree):
    tree.collapse_unary(collapsePOS=True, collapseRoot=True, joinChar="+")
    tree.chomsky_normal_form()
    return ToString(tree)
    

import sys
from nltk.corpus import BracketParseCorpusReader
#corpus_rootr"/home/jihuni/ptb/treebank_3/parsed/mrg/wsj/train/" 
#output="/home/jihuni/ptb/treebank_3/wsj.train"
corpus_root = sys.argv[1]
output = sys.argv[2]
file_pattern = r".*/wsj_.*\.mrg"
ptb = BracketParseCorpusReader(corpus_root, file_pattern)
#ptb.fileids()

sents=[' '.join(words) for words in ptb.sents()]
parsed_sents=ptb.parsed_sents()
binary_trees=[ToBinaryTreeStr(tree) for tree in parsed_sents]

with open(output, 'w') as f:
    for sent in sents:
        f.write(sent+'\n')
with open(output+'.tree', 'w') as f:
    for sent in binary_trees:
        f.write(sent+'\n')