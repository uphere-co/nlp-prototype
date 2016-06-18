
# -*- coding: utf-8 -*-
import os
import sys
myPath = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, myPath + '/../')

import numpy as np

from recursiveNN.math import ArrayOrScala,SimplifyIfScalar, IsZero,IsAllOne,IsIdentity, IsScalar,IsVector,IsMatrix
#`self._val is None` indicates that there are no cache.
#'np.isnan(self._val)' indicates that some of its variables are set to NaN.
# It is parent's responsibility to set children's parents.

#__str__() : for pretty prints
#expression() : Math formula. If differs from __str__ for Word and Phrase.

def IsIn(iter, x):
    for elm in iter:
        if x is elm:
            return True
    return False
    
class Node(object):
    def __init__(self, name):
        self._parents=[]
        self.name=name
        self._val=None
    def __unicode__(self):
        return self.name
    def __str__(self):
        return unicode(self).encode('utf-8')     
    @property
    def parents(self):
        return self._parents
    #@parent.setter
    def add_parent(self,parent):
        if not IsIn(self._parents, parent):
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
        self._val=ArrayOrScala(val)
    def __repr__(self):
        return "Val(%r)"%(self.name)
    def __eq__(self, other):
        if isinstance(other, self.__class__) and np.all(self._val == other._val):
            return True
        return False
    def diff_no_simplify(self, var):
        v=np.zeros(self._val.shape)
        zero = Val(v)
        return zero
    
class Var(Node):
    def __init__(self, name, val=np.nan):
        Node.__init__(self, name)
        self._val=ArrayOrScala(val)
    def __repr__(self):
        return "Var(%r)"%(self.name)
    def __eq__(self, other):
        if isinstance(other, self.__class__) and self.name == other.name:# and self.parent==other.parent:
            return True
        return False
    def diff_no_simplify(self, var):
        v=np.ones(self._val.shape)
        if(var.name!= self.name):
            v=np.zeros(self._val.shape)
        val=Val(v)
        return val
    @Node.val.setter
    def val(self,val):
        self.resetCachedValue()
        self._val=ArrayOrScala(val)

def softmax(x):
    x=x-np.max(x)
    exp_x=np.exp(x)
    return exp_x/exp_x.sum()

        
class VSF(Node):
    '''VectorizedScalaFunction'''
    known_functions=dict(
    [('cos' ,('cos',np.cos)),
     ('sin' ,('sin',np.sin)),
     ('exp' ,('exp',np.exp)),
     ('log' ,('log',np.log)),
     ('cos`',('-sin',lambda x : -np.sin(x) )),
     ('sin`',('cos',np.cos)),
     ('exp`',('exp',np.exp)),
     ('log`' ,('1/',lambda x : 1.0/x)),
     ('1/'   ,('1/',lambda x : 1.0/x)),
     ('sig' ,('sig', lambda x : 1/(1+np.exp(-x)))),
     ('tanh',('tanh',np.tanh)),
     ('tanh`',('tanh`',lambda x : np.cosh(x)**-2)),
     ('sig`',('sig`',lambda x : np.exp(-x)/(1+np.exp(-x))**2)),
     #('softmax',('softmax',softmax)), softmax is not VSF.
     ])
    def __init__(self, name, var, op=None):
        name, op0 = VSF.known_functions.get(name, (name,op))            
        if op:
            op0=op 
        Node.__init__(self,name)
        self.op=op0
        self.var=var
        self.var.add_parent(self)
    def __unicode__(self):
        return u"%s(%s)"%(self.name, self.var)
    def __repr__(self):
        return "VSF(%r)(%r)"%(self.name, self.var)
    def expression(self):
        if hasattr(self.var, 'expression'):
            return u"%s(%s)"%(self.name, self.var.expression())
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
        expr=CTimes(VSF(self.name+"`", self.var), 
                    self.var.diff_no_simplify(var))
        return expr
    @property
    def val(self):
        if not self.op:
            return None
        elif self._val is None or np.all(np.isnan(self._val)):
            self._val = self.op(self.var.val)            
        if self._val.shape==(1,1):
            self._val=self._val[0,0]
        return self._val

#Sum0 can be replaced by dot with 1s.
class Sum0(Node):
    def __init__(self, x):
        Node.__init__(self,name=None)
        self.var=x
        self.var.add_parent(self)
    def __unicode__(self):
        return u"Σ_0(%s)"%(self.var)
    def __repr__(self):
        return "Sum_0(%r)"%(self.var)        
    def simplify(self):
        self.var=self.var.simplify()
        if IsScalar(self.var):
                return self.var
        self.var.add_parent(self)
        return self
    @property
    def val(self):
        if not np.any(self._val):
            self._val = SimplifyIfScalar(np.sum(self.var.val, axis=0).reshape(1,-1))
        return self._val

class Transpose(Node):
    def __init__(self, x):
        Node.__init__(self,name=None)
        self.op=np.transpose
        self.var=x
        self.var.add_parent(self)
        self._format=u"[%s]ᵀ"
        if isinstance(self.var, Var) or isinstance(self.var, Val):
            self._format=u"%sᵀ"
    def __unicode__(self):
        return self._format%(self.var)
    def __repr__(self):
        return "Transpose(%r)"%(self.var)        
    def simplify(self):
        self.var=self.var.simplify()
        if IsScalar(self.var):
            return self.var
        self.var.add_parent(self)
        return self
    @property
    def val(self):
        if not np.any(self._val):
            self._val = SimplifyIfScalar(np.transpose(self.var.val))
        return self._val
    def diff_no_simplify(self, var):
        expr=Transpose(self.var.diff_no_simplify(var))
        return expr

def TransposeIfVector(var):
    if IsVector(var):
            return Transpose(var)
    return var
        
class BinaryOperator(Node):
    def __init__(self, x, y):
        Node.__init__(self,name=None)
        self.op=None
        self._format=u"%s%s%s"
        self.x, self.y = x,y        
        self.x.add_parent(self)
        self.y.add_parent(self)
    def __unicode__(self):
        return self._format%(self.x, self.name, self.y)
    def expression(self):
        if hasattr(self.x, 'expression'):
            x_expr = self.x.expression()
        else:
            x_expr= self.x.__unicode__()
        if hasattr(self.y, 'expression'):
            y_expr = self.y.expression()
        else:
            y_expr= self.y.__unicode__()
        return self._format%(x_expr, self.name, y_expr)        
    def simplify(self):
        self.x=self.x.simplify()
        self.x.add_parent(self)
        self.y=self.y.simplify()
        self.y.add_parent(self)
    @property
    def val(self):
        if not np.any(self._val):
            tmp=self.op(self.x.val, self.y.val)
            self._val = SimplifyIfScalar(tmp)
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
        if IsZero(self.x):
            return self.y
        elif IsZero(self.y):
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
            self._format=u'{%s}%s%s'
        elif isinstance(self.y, Add):
            self._format=u'%s%s{%s}'
        else:
            self._format=u'%s%s%s'
    def simplify(self):
        BinaryOperator.simplify(self)
        self.update_format()
        if IsZero(self.x) :
            return self.x
        elif IsZero(self.y) :
            return self.y
        elif IsIdentity(self.x):
            return self.y
        elif IsIdentity(self.y):
            return self.x
        return self
    def diff_no_simplify(self, var):
        expr=Add(CTimes(TransposeIfVector(self.x.diff_no_simplify(var)),TransposeIfVector(self.y)), 
                 CTimes(TransposeIfVector(self.x),TransposeIfVector(self.y.diff_no_simplify(var))))
        return expr
class Dot(BinaryOperator):
    def __init__(self, x, y):
        BinaryOperator.__init__(self,x,y)
        self.name = u'⋅'
        self.op=np.dot
        self.update_format()
    def __repr__(self):
        return "Dot(%r,%r)"%(self.x, self.y)
    def update_format(self):
        if isinstance(self.x, Add):
            self._format=u'{%s}%s%s'
        elif isinstance(self.y, Add):
            self._format=u'%s%s{%s}'
        else:
            self._format=u'%s%s%s'
    def simplify(self):
        BinaryOperator.simplify(self)
        self.update_format()
        if IsZero(self.x) :
            return self.x
        elif IsZero(self.y) :
            return self.y
        elif IsIdentity(self.x):
            return self.y
        elif IsIdentity(self.y):
            return self.x
        return self
    def diff_no_simplify(self, var):
        expr=Add(CTimes(TransposeIfVector(self.x.diff_no_simplify(var)),TransposeIfVector(self.y)), 
                 CTimes(TransposeIfVector(self.x),TransposeIfVector(self.y.diff_no_simplify(var))))
        return expr

#CircleTimes : heavily used for differentiation by Matrix.
class CTimes(BinaryOperator):
    def __init__(self, x, y):
        BinaryOperator.__init__(self,x,y)
        self.name = u'⊗'
        self.op = lambda x,y : np.array(x)*np.array(y)
        self.update_format()
    def __repr__(self):
        return "CTimes(%r,%r)"%(self.x, self.y)
    def update_format(self):
        if isinstance(self.x, Add):
            self._format=u'{%s}%s%s'
        elif isinstance(self.y, Add):
            self._format=u'%s%s{%s}'
        else:
            self._format=u'%s%s%s'
    def simplify(self):
        BinaryOperator.simplify(self)
        self.update_format()
        if IsZero(self.x) :
            return self.x
        elif IsZero(self.y) :
            return self.y
        #TODO: Critical. For cases like a⊗1⊗b where a,b have incorrect dimensions,
        #      below simplication may results error.  
        elif IsAllOne(self.x) :
            return self.y
        elif IsAllOne(self.y):
            return self.x
        return self
    def diff_no_simplify(self, var):
        assert(0)
        return expr

