
import numpy as np

class Number(object):
    def __init__(self, name, val):
        self.name = name
        self.parent=None
        self.val = val
    def __str__(self):
        return self.name
    def __eq__(self, other):
        if isinstance(other, self.__class__) and self.name == other.name:# and self.parent==other.parent:
            return True
        return False
    def send_message(self, mesg):
        print self, 'received message: %r'%mesg
    def simplify(self):
        return self
    def diff(self,var):
        return self.diff_no_simplify(var)
        
class Val(Number):
    def __init__(self, val):
        Number.__init__(self, str(val), val)
    def __repr__(self):
        return "Val(%r)"%(self.name)
    def diff_no_simplify(self, var):
        zero = Val(0)
        zero.parent = self.parent
        return zero
    
class Var(Number):
    def __init__(self, name, val=np.nan):
        Number.__init__(self, name, val)
        self._val = val
    def __repr__(self):
        return "Var(%r)"%(self.name)
    def diff_no_simplify(self, var):
        v=1
        if(var.name!= self.name):
            v=0
        val=Val(v)
        val.parent=self.parent
        return val
    @property
    def val(self):
        return self._val
    @val.setter
    def val(self,val):
        self._val=val
        if self.parent:
            self.parent.resetCachedValue()
        
        
class Word(object):
    def __init__(self, word):
        self.name = word
        self.parent=None
        self.expr = 'w2v(%s)'%word
        self.vec = Val(np.nan)
    def __str__(self):
        return self.name
    def __repr__(self):
        return "Word(%r)"%(self.name)
    def expression(self):
        return self.expr
    def diff_no_simplify(self, var):
        return self.vec.diff_no_simplify(var)
    def diff(self, var):
        return self.vec.diff(var)
    def send_message(self, mesg):
        print self, ' received %s'%mesg
        out = str(self)+' received %s'%mesg
        if self.parent :
            print self, ' returns %s to '%mesg, self.parent
        return
    def simplify(self):
        return self
        
class Phrase(object):
    def __init__(self, left, right, vec):
        self.left = left
        self.left.parent = self
        self.right = right
        self.right.parent = self
        self.parent = None
                
        self.vec = vec 
    def __str__(self):
        return '(%s,%s)'%(self.left,self.right)        
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
        if self.parent :
            print self, 'returns %s to'%mesg, self.parent
        else :
            print "Message is round-toured."
    def simplify(self):
        return self
    
class Fun(object):
    def __init__(self, name, var, op=None):
        self.name = name
        self.parent=None
        self.var=var
        self.var.parent=self
        self.op=op
        self._val=None
    def __str__(self):
        return "%s(%s)"%(self.name, self.var)
    def __repr__(self):
        return "Fun(%r)"%(self.name)
    def expression(self):
        if hasattr(self.var, 'expression'):
            return "%s(%s)"%(self.name, self.var.expression())
        return self.__str__()
    def __eq__(self, other):
        if isinstance(other, self.__class__) and self.var == other.var and self.name==other.name:
            return True
        return False
    def simplify(self):
        self.var.simplify()
        return self
    def diff_no_simplify(self, var):
        expr=Mul(Fun(self.name+"`", self.var), self.var.diff_no_simplify(var))
        return expr
    def diff(self, var):        
        return self.diff_no_simplify(var).simplify()
    @property
    def val(self):
        if not np.any(self._val):
            self._val = self.op(self.var.val)
        return self._val
    def resetCachedValue(self):
        self._val=None
        if self.parent:
            self.parent.resetCachedValue()
        
    def send_message(self, mesg):
        print self, 'send message to ', self.var
        self.var.send_message(mesg)

class BinaryOperator(object):
    def __init__(self, x, y):
        self.name = None
        self.x, self.y = x,y
        self.format="%s%s%s"
        self._parent=None
        
        self.x.parent=self
        self.y.parent=self
        
        self._val=None
    @property
    def parent(self):
        return self._parent
    @parent.setter
    def parent(self,value):
        self._parent=value
    def __str__(self):
        return self.format%(self.x, self.name, self.y)
    def expression(self):
        if hasattr(self.x, 'expression'):
            x_expr = self.x.expression()
        else:
            x_expr= self.x.__str__()
        if hasattr(self.y, 'expression'):
            y_expr = self.y.expression()
        else:
            y_expr= self.y.__str__()
        return self.format%(x_expr, self.name, y_expr)
    def send_message(self, mesg):
        print self, 'send %r message to '%mesg, self.x, ' and ', self.y
        self.x.send_message(mesg)
        self.y.send_message(mesg)        
    def simplify(self):
        self.x=self.x.simplify()
        self.y=self.y.simplify()
    def diff_no_simplify(self, var):
        assert(0)
    def diff(self, var):        
        return self.diff_no_simplify(var).simplify()
    @property
    def val(self):
        if not np.any(self._val):
            self._val = self.op(self.x.val, self.y.val)
        return self._val
    def resetCachedValue(self):
        self._val=None
        if self.parent:
            self.parent.resetCachedValue()
    
class Add(BinaryOperator):
    def __init__(self, x, y):
        BinaryOperator.__init__(self,x,y)
        self.name = '+'
        self.op = np.add
    def __repr__(self):
        return "Add(%r,%r)"%(self.x, self.y)
    def simplify(self):
        BinaryOperator.simplify(self)
        if self.x==Val(0):
            self.y.parent = self.parent
            return self.y
        elif self.y==Val(0):
            self.x.parent = self.parent
            return self.x
        return self
    @BinaryOperator.parent.setter
    def parent(self,value):        
        self._parent=value
        if isinstance(self.parent, Mul):
            self.format='(%s%s%s)'
    def diff_no_simplify(self, var):
        expr=Add(self.x.diff_no_simplify(var),self.y.diff_no_simplify(var))
        return expr
        
    
class Mul(BinaryOperator):
    def __init__(self, x, y):
        BinaryOperator.__init__(self,x,y)
        self.name = '*'
        self.op=np.dot
    def __repr__(self):
        return "Mul(%r,%r)"%(self.x, self.y)
    def simplify(self):
        BinaryOperator.simplify(self)
        if self.x==Val(0) or self.y==Val(0):
            zero = Val(0)
            zero.parent = self.parent
            return zero
        elif self.x==Val(1):
            self.y.parent=self.parent
            return self.y
        elif self.y==Val(1):
            self.x.parent=self.parent
            return self.x
        return self
    def diff_no_simplify(self, var):
        expr=Add(Mul(self.x.diff_no_simplify(var),self.y), 
                 Mul(self.x,self.y.diff_no_simplify(var)))
        return expr