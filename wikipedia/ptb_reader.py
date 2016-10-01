import sys
from nltk.corpus import BracketParseCorpusReader
from nltk.tree import Tree
from copy import deepcopy


def IsLeaf(node):
    #From NLTK documentation:
    #    containing no children is 1; the height of a tree
    #    containing only leaves is 2; and the height of any other
    return node.height()==2

def ToPlainString(node):
    if IsLeaf(node):
        return node[0]
    return '%s %s'%(ToPlainString(node[0]), ToPlainString(node[1]))
def ToString(node):
    if IsLeaf(node):
        return node[0]
    return '(%s %s)'%(ToString(node[0]), ToString(node[1]))
    
def ToBinaryTreeStr(tree):
    tree=deepcopy(tree)
    tree.collapse_unary(collapsePOS=True, collapseRoot=True, joinChar="+")
    tree.chomsky_normal_form()
    return ToString(tree)
    
def RemoveNonePOSnode(tree):
    for node in tree:
        print "Iter", node
        if not isinstance(node, Tree):
            continue;
        if isinstance(node, Tree) and node.label()=='-NONE-':
            print "Remove", node
            tree.remove(node)
            RemoveNonePOSnode(tree)
        elif isinstance(node[0], Tree) and node[0].label()=='-NONE-':
            print "Remove", node
            tree.remove(node)
            RemoveNonePOSnode(tree)
        else :
            RemoveNonePOSnode(node)

def is_none_node(node):
    if isinstance(node, Tree) and node.label()=='-NONE-':
        return True
    return False

def is_empty_node(node):
    if list(node)==[]:
        return True;
    return False

def RemoveNonePOSnode(tree):
    if not isinstance(tree, Tree):
        return
    none_nodes = filter(is_none_node, tree)
    for node in none_nodes:
        tree.remove(node)
    for node in tree:
        RemoveNonePOSnode(node)
        
def RemoveNullNode(tree):
    is_finished=True
    if not isinstance(tree, Tree):
        return is_finished
    empty_nodes = filter(is_empty_node, tree)
    for node in empty_nodes:
        tree.remove(node)
        is_finished=False
    for node in tree:
        is_finished = RemoveNullNode(node) and is_finished 
    return is_finished
    


def ToTrimmedBinaryTreeStr(tree):
    tree=deepcopy(tree)
    RemoveNonePOSnode(tree)
    while(not RemoveNullNode(tree)):
        True
    tree.collapse_unary(collapsePOS=True, collapseRoot=True, joinChar="+")
    tree.chomsky_normal_form()
    return ToString(tree), ToPlainString(tree)
#corpus_root="/home/jihuni/ptb/treebank_3/parsed/mrg/wsj/train/" 
#output="/home/jihuni/ptb/treebank_3/wsj.train"
corpus_root = sys.argv[1]
output = sys.argv[2]
file_pattern = r".*/wsj_.*\.mrg"
if(sys.argv>3):
   file_pattern=sys.argv[3]

ptb = BracketParseCorpusReader(corpus_root, file_pattern)
#ptb.fileids()

sents=[' '.join(words) for words in ptb.sents()]
parsed_sents=ptb.parsed_sents()
#binary_trees=[ToBinaryTreeStr(tree) for tree in parsed_sents]
trimmed_binary_trees=[ToTrimmedBinaryTreeStr(tree) for tree in parsed_sents]

#with open(output, 'w') as f:
#    for sent in sents:
#        f.write(sent+'\n')
#with open(output+'.tree', 'w') as f:
#    for sent in binary_trees:
#        f.write(sent+'\n')
with open(output+'.trim', 'w') as f:
    with open(output+'.trim.tree', 'w') as f2:
        for sent, plain_sent in trimmed_binary_trees:
            f.write(plain_sent.encode('utf-8')+'\n')
            f2.write(sent.encode('utf-8')+'\n')
