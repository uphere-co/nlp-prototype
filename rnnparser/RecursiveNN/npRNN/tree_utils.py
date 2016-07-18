class Vec(object):
    @classmethod
    def merge(cls,wordL, wordR, epoch):
        vec = Vec('(%s %s)'%(wordL.name,wordR.name))
        vec.left = wordL
        vec.right = wordR
        vec.depth = max(wordL.depth, wordR.depth)+1
        vec.epoch = epoch
        return vec
    def __init__(self, word):
        self.depth=0
        self.name=word
        self.left=None
        self.right=None
        self.epoch=None
    def __unicode__(self):
        return self.name
    def __repr__(self):
        return self.__unicode__()
    def isPhrase(self):
        return vec.epoch is not None
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

class VecTree(object):
    @classmethod
    def random_merge(cls,words):
        words=list(words)
        merge_history=[]
        epoch=0
        while len(words)>1:
            idx_beg=np.random.choice(range(len(words)-1))
            merge_history.append(idx_beg)
            idx_end=idx_beg+2
            wordL,wordR=words[idx_beg],words[idx_beg+1]
            words[idx_beg:idx_end]= [Vec.merge(wordL,wordR, epoch)]
            epoch+=1
        return words[0], merge_history
    @classmethod
    def directed_merge(cls,words, merge_history):
        merge_history=list(merge_history)
        words=list(words)
        epoch=0
        for idx_beg in merge_history:
            idx_end=idx_beg+2
            wordL,wordR=words[idx_beg],words[idx_beg+1]
            words[idx_beg:idx_end]= [Vec.merge(wordL,wordR, epoch)]
            epoch+=1
        return words[0]
    def __init__(self, words, history=None):
        if history is None:
            self.phrase, self.history = VecTree.random_merge(words)
        else:
            self.history=history
            self.phrase = VecTree.directed_merge(words, self.history)
