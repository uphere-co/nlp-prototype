
import numpy as np

#`self._val is None` indicates that there are no cache.
#'np.isnan(self._val)' indicates that some of its variables are set to NaN.
class Node(object):
    def __init__(self, name):
        self._parents=[]
        self.name=name
        self._val=None
    def __str__(self):
        return self.name     
    @property
    def parents(self):
        return self._parents
    #@parent.setter
    def add_parent(self,parent):
        self._parents.append(parent)
    @property
    def val(self):
        return self._val
    def diff_no_simplify(self, var):
        assert(0)
    def diff(self,var):
        return self.diff_no_simplify(var).simplify()
    def simplify(self):
        return self
    def resetCachedValue(self):
        self._val=None
        if self.parents:
            for parent in self.parents:
                parent.resetCachedValue()     
               
class Val(Node):
    def __init__(self, val):
        Node.__init__(self, str(val))
        self._val=val
    def __repr__(self):
        return "Val(%r)"%(self.name)
    def __eq__(self, other):
        if isinstance(other, self.__class__) and self.name == other.name:# and self.parent==other.parent:
            return True
        return False
    def diff_no_simplify(self, var):
        zero = Val(0)
        return zero
    
class Var(Node):
    def __init__(self, name, val=np.nan):
        Node.__init__(self, name)
        self._val=val
    def __repr__(self):
        return "Var(%r)"%(self.name)
    def __eq__(self, other):
        if isinstance(other, self.__class__) and self.name == other.name:# and self.parent==other.parent:
            return True
        return False
    def diff_no_simplify(self, var):
        v=1
        if(var.name!= self.name):
            v=0
        val=Val(v)
        return val
    @Node.val.setter
    def val(self,val):
        self.resetCachedValue()
        self._val=val                    
        
class Word(Node):
    def __init__(self, word):
        Node.__init__(self, name=word)
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
        if self.parents :
            for parent in self.parents:
                print self, 'returns %s to'%mesg, parent
        else :
            print "Message is round-toured."

            
class Fun(Node):
    known_functions=dict(
    [('cos' ,('cos',np.cos)),
     ('sin' ,('sin',np.sin)),
     ('exp' ,('exp',np.exp)),
     ('log' ,('log',np.log)),
     ('cos`',('-sin',lambda x : -np.sin(x) )),
     ('sin`',('cos',np.cos)),
     ('exp`',('exp',np.exp)),
     ('log`' ,('1/',lambda x : 1.0/x)),
     ('sig' ,('sig', lambda x : 1/(1+np.exp(-x)))),
     ('tanh',('tanh',np.tanh)),
     ('sig`',('sig`',lambda x : np.exp(-x)/(1+np.exp(-x))**2))])
     #'',('',),
    def __init__(self, name, var, op=None):
        if name in Fun.known_functions.keys():
            name, op0 = Fun.known_functions[name]
            if not op:
                op=op0 
        Node.__init__(self,name)
        self.op=op
        self.var=var
        self.var.add_parent(self)
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
        tmp=self.var.simplify()
        if not tmp is self.var:
            self.var=tmp
            self.var.add_parent(self)
        return self
    def diff_no_simplify(self, var):
        expr=Mul(Fun(self.name+"`", self.var), self.var.diff_no_simplify(var))
        return expr
    @property
    def val(self):
        if not np.any(self._val):
            self._val = self.op(self.var.val)
        return self._val

class BinaryOperator(Node):
    def __init__(self, x, y):
        Node.__init__(self,name=None)
        self.op=None
        self.format="%s%s%s"
        self.x, self.y = x,y        
        self.x.add_parent(self)
        self.y.add_parent(self)
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
    def simplify(self):
        self.x=self.x.simplify()
        self.x.add_parent(self)
        self.y=self.y.simplify()
        self.y.add_parent(self)
    @property
    def val(self):
        if not np.any(self._val):
            self._val = self.op(self.x.val, self.y.val)
        return self._val
    
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
            return self.y
        elif self.y==Val(0):
            return self.x
        return self
    def diff_no_simplify(self, var):
        expr=Add(self.x.diff_no_simplify(var),self.y.diff_no_simplify(var))
        return expr
        
    
class Mul(BinaryOperator):
    def __init__(self, x, y):
        BinaryOperator.__init__(self,x,y)
        self.name = '*'
        self.op=np.dot
        self.update_format()
    def __repr__(self):
        return "Mul(%r,%r)"%(self.x, self.y)
    def update_format(self):
        if isinstance(self.x, Add):
            self.format='(%s)%s%s'
        elif isinstance(self.y, Add):
            self.format='%s%s(%s)'
        else:
            self.format='%s%s%s'
    def simplify(self):
        BinaryOperator.simplify(self)
        self.update_format()
        if self.x==Val(0) or self.y==Val(0):
            return Val(0)
        elif self.x==Val(1):
            return self.y
        elif self.y==Val(1):
            return self.x
        return self
    def diff_no_simplify(self, var):
        expr=Add(Mul(self.x.diff_no_simplify(var),self.y), 
                 Mul(self.x,self.y.diff_no_simplify(var)))
        return expr