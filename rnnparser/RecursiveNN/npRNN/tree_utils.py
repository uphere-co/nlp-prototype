class Node(object):
    @classmethod
    def merge(cls,wordL, wordR, epoch):
        node = cls('(%s %s)'%(wordL.name,wordR.name))
        node.left = wordL
        node.left.parent=node
        node.right = wordR
        node.right.parent=node
        node.depth = max(wordL.depth, wordR.depth)+1
        node.epoch = epoch
        return node
    def __init__(self, word):
        self.depth=0
        self.name=word
        self.left=None
        self.right=None
        self.parent=None
        self.epoch=None
    def __unicode__(self):
        return self.name
    def __repr__(self):
        return self.__unicode__()
    def isPhrase(self):
        return self.epoch is not None
    def other_leaf(self, leaf):
        if leaf is self.left:
            return self.right
        return self.left
    def iter(self):
        if self.depth==0:
            return []
        return self.left.iter() + [self] + self.right.iter()
    def iter2(self):
        if self.left is not None:
            yield self.left.iter2()
        yield self
        if self.right is not None:
            yield self.right.iter2()

class NodeTree(object):
    @classmethod
    def random_merge(cls,word_nodes):
        words=list(words)
        merge_history=[]
        epoch=0
        new_words=[]
        while len(words)>1:
            idx_beg=np.random.choice(range(len(words)-1))
            merge_history.append(idx_beg)
            wordL,wordR=words[idx_beg],words[idx_beg+1]
            new_word = Node.merge(wordL,wordR, epoch)
            words[idx_beg:idx_beg+2]= [new_word]
            new_words.append(new_word)
            epoch+=1
        return new_words, merge_history
    @classmethod
    def directed_merge(cls,words, merge_history):
        merge_history=list(merge_history)
        words=list(words)
        epoch=0
        new_words=[]
        for idx_beg in merge_history:
            wordL,wordR=words[idx_beg],words[idx_beg+1]
            new_word = Node.merge(wordL,wordR, epoch)
            words[idx_beg:idx_beg+2]= [new_word]
            new_words.append(new_word)
            epoch+=1
        return new_words, merge_history
    @classmethod
    def get_merge_direction(cls,composites):
        left_merged=[[] for _ in composites]
        for i,node in enumerate(composites):
            left_merged[i].append(True)
            parent=node.parent
            while parent is not None:
                if parent.left is node:
                    left_merged[i].append(True)
                else:
                    left_merged[i].append(False)
                parent,node=parent.parent,parent
        return left_merged
    @classmethod
    def words_merged(cls, node):
        words = [node.left, node.right]
        while node.parent is not None:
            words.append(node.parent.other_leaf(node))
            node=node.parent
        return words
    def __init__(self, words, history=None):
        if history is None:
            new_words, merge_history = NodeTree.random_merge(words)
        else:
            new_words, merge_history = NodeTree.directed_merge(words, history)
        self.history=merge_history
        self.phrase=new_words[-1]
