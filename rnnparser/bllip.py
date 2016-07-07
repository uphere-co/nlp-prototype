import os
import sys
from nltk.parse.bllip import BllipParser
parser_path=sys.argv[1]

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

def ParseToBinaryTree(sentence):
    top_parse = bllip.parse_one(sentence.split())
    top_parse.collapse_unary(collapsePOS=True, collapseRoot=True, joinChar="+")
    top_parse.chomsky_normal_form()
    return ToASCIIstring(top_parse)

for line in sys.stdin:
    binary_tree = ParseToBinaryTree(line.strip())
    sys.stdout.write(binary_tree+'\n')
