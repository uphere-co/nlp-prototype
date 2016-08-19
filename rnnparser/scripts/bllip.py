import os
import sys
from nltk.parse.bllip import BllipParser
#parser_path='/data/groups/uphere/parsers/bllipparser/SANCL2012-Uniform'
parser_path='/data/groups/uphere/parsers/bllipparser/WSJ+Gigaword-v2'

bllip = BllipParser.from_unified_model_dir(parser_path)

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
    sentences= open(filename, 'r').readlines()
    strs = [ToBinaryTreeStr(list(tree)[0]) for tree in bllip.parse_sents(sentences)]
    #strs=[ToBinaryTreeStr(bllip.parse_one(sent)) for sent in sentences]
    with open(filename+'.bllip', 'w') as f:
        for line in strs:
            f.write(line+'\n')
