
# -*- coding: utf-8 -*-
from nodes import Node, Val,Var,VSF, Add,Mul

class Word(Node):
    def __init__(self, word):
        Node.__init__(self, name=word)
        self.expr = 'w2v(%s)'%word
        self.vec = Val(np.nan)
    def __str__(self):
        return self.name
    def __repr__(self):
        return u"Word(%r)"%(self.name)
    def expression(self):
        return self.expr
    def diff_no_simplify(self, var):
        return self.vec.diff_no_simplify(var)
    def diff(self, var):
        return self.vec.diff(var)
    def send_message(self, mesg):
        print self, ' received %s'%mesg
        out = str(self)+' received %s'%mesg
        if self.parents :
            for parent in self.parents:
                self, ' returns %s to '%mesg, parent
        return
        
class Phrase(Node):
    def __init__(self, left, right, vec):
        Node.__init__(self, name=None)
        self.left = left
        self.left.add_parent(self)
        self.right = right
        self.right.add_parent(self)                
        self.vec = vec 
    def __unicode__(self):
        return u'(%s,%s)'%(self.left,self.right)        
    def __repr__(self):
        return "Phrase(%r,%r)"%(self.left,self.right)
    def expression(self):
        return self.vec.expression()
    def diff_no_simplify(self, var):
        return self.vec.diff_no_simplify(var)
    def diff(self, var):
        return self.vec.diff(var)
    def send_message(self, mesg):
        print self, ' send %s to %s and %s'%(mesg, self.left, self.right)
        self.left.send_message(mesg)
        self.right.send_message(mesg)
        if self.parents :
            for parent in self.parents:
                print self, 'returns %s to'%mesg, parent
        else :
            print "Message is round-toured."
            
class RecursiveNN:
    def __init__(self, W_left_init, W_right_init, bias_init):
        assert isinstance(W_left_init, Var), "W_left should be instance of `Var`"
        assert isinstance(W_right_init, Var), "W_right should be instance of `Var`"
        assert isinstance(bias_init, Var), "bias should be instance of `Var`"
        self.W_left=W_left_init
        self.W_right=W_right_init
        bias = Word('!'+bias_init.name+'_vec!')
        bias.expr='bias'
        bias.vec=bias_init        
        self.bias=bias
    def combineTwoNodes(self, left,right):        
        Wxh_left=Mul(self.W_left, left)
        Wxh_right=Mul(self.W_right, right)
        x=Add(Add(Wxh_left,Wxh_right), self.bias)
        vec=VSF('tanh', x)
        phrase=Phrase(left,right, vec)        
        return phrase
        
class Word2VecFactory:
    def __init__(self):
        pass

