# -*- coding: utf-8 -*-
import os
import sys
import pytest
import numpy as np

myPath = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, myPath + '/../')
from recursiveNN.math import dot,To2Darray,SimplifyIfScalar, IsZero,IsAllOne,IsIdentity, IsScalar,IsVector,IsMatrix

'''
`self._val is None` of Node object indicates that there are no cache.
`np.isnan(self._val)` indicates that some of its variables are set to NaN.
It is parent's responsibility to set children's parents.

Node.__str__() : for pretty prints
Node.expression() : Math formula. If differs from __str__ for Word and Phrase.
'''

@pytest.fixture(scope="function")
def reset_NodeDict():
    NodeDict.reset()
    pass

@pytest.fixture(scope="function")
def deregister(node):
    print 'Deregister an instance :', NodeDict.getKey(instance)
    NodeDict.deregister(node)

def register(node):
    NodeDict.register(node)

class NodeDict(type):
    _dict = {}
    def __call__(cls, *args):
        instance = super(NodeDict, cls).__call__(*args)
        #Do not cache Val instances
        if isinstance(instance, Val):
            return instance
        key = NodeDict.getKey(instance)
        if key in cls._dict.keys():
            #print "Reusing", key
            return cls._dict[key]
        return instance
    @staticmethod
    def reset():
        #print 'Reset NodeDict'
        NodeDict._dict={}
    @staticmethod
    def getKey(node):
        return str(node)
    @staticmethod
    def deregister(node):
        key=NodeDict.getKey(node)
        NodeDict._dict.pop(key)
        NodeDict._dict_instance.pop(node)
    @staticmethod
    def register(node):
        key=NodeDict.getKey(node)
        if not key in NodeDict._dict.keys():
            #print 'Register an instance :', key
            NodeDict._dict[key] = node

class Node(object):
    __slots__= ["_parents", "name","_val", "_uid"] #
    __metaclass__ = NodeDict
    #_uid_count = 0
    def __init__(self, name):
        self._parents=[]
        self.name=name
        self._val=None
        self._uid=None
        #self._uid = Node._uid_count
        #Node._uid_count+=1
    @classmethod
    def Compile(cls, name, expr, name_scope=None):
        funcion_body, variables = expr.code(), expr.variables
        code='''
import numpy as np
import numba

@numba.jit(nogil=True,nopython=True)
def {name}({vars}):
    return {code}'''.format(name=name, vars=','.join(variables), code=funcion_body)
        if name_scope is None:
            #exec(code,globals())
            exec(fullcode,locals())
            return
        exec(code,name_scope)
        return name_scope
    def __eq__(self, other):
        #return self._uid is other._uid
        return self is other
    def __unicode__(self):
        return self.name
    def __str__(self):
        return unicode(self).encode('utf-8')
    def code(self):
        pass
    def __del__(self):
        #print self, 'will be destructed!'
        #deregister(self)
        pass
    @property
    def parents(self):
        return self._parents
    @property
    def children(self):
        return []
    @property
    def variables(self):
        return []
    def add_parent(self,parent):
        pass
        if not parent in self._parents:
            self._parents.append(parent)
    def remove_parent(self,parent):
        #Due to no duplication of parents, .remove, which delete first occurence only, is enough.
        self._parents.remove(parent)
    @property
    def val(self):
        return self._val
    @val.setter
    def val(self, val):
        self.resetCachedValue()
        self._val=val
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
    def isContain(self, expr):
        if self is expr :
            return True
        else :
            return np.any([child.isContain(expr) for child in self.children])
    def cache(self):
        register(self)
        for child in self.children:
            child.add_parent(self)
            child.cache()

class Val(Node):
    __slots__ = []
    def __init__(self, val):
        Node.__init__(self, 'Val')
        super(self.__class__, self.__class__).val.fset(self, To2Darray(val))
    def __unicode__(self):
        return unicode(SimplifyIfScalar(self.val))
    def __repr__(self):
        return "Val(%r)"%(unicode(self.val))
    def __eq__(self, other):
        if isinstance(other, self.__class__) and np.all(self.val == other.val):
            return True
        return False
    def diff_no_simplify(self, var):
        v=np.zeros(self.val.shape)
        zero = Val(v)
        return zero
    @property
    def val(self):
        return super(self.__class__, self).val
    @val.setter
    def val(self, val):
        raise TypeError('Val objects are immutable')

class Var(Node):
    __slots__ = []
    def __init__(self, name, val=np.nan):
        Node.__init__(self, name)
        self.val=To2Darray(val)
    def __repr__(self):
        return "Var(%r)"%(self.name)
    def code(self):
        return self.name
    def diff_no_simplify(self, var):
        v=np.ones(self.val.shape)
        if(var.name!= self.name):
            v=np.zeros(self.val.shape)
        val=Val(v)
        return val
    @property
    def variables(self):
        return set([self.name])
    @property
    def val(self):
        return super(self.__class__, self).val
    @val.setter
    def val(self, val):
        super(self.__class__, self.__class__).val.fset(self, To2Darray(val))

def softmax(x):
    x=x-np.max(x)
    exp_x=np.exp(x)
    return exp_x/exp_x.sum()

identify_func=lambda x : x

tanhprime__=lambda x : np.cosh(x)**-2
cosprime__=lambda x : -np.sin(x)
class VSF(Node):
    '''VectorizedScalaFunction'''
    __slots__ = ["op","var"]
    known_functions=dict(
    [('cos' ,('cos',np.cos)),
     ('sin' ,('sin',np.sin)),
     ('exp' ,('exp',np.exp)),
     ('log' ,('log',np.log)),
     ('cos`',('-sin',cosprime__ )),
     ('sin`',('cos',np.cos)),
     ('exp`',('exp',np.exp)),
     ('log`' ,('1/',np.reciprocal)),
     ('tanh',('tanh',np.tanh)),
     ('tanh`',('tanh`',tanhprime__)),
     #('sig' ,('sig', lambda x : 1/(1+np.exp(-x)))),
     #('sig`',('sig`',lambda x : np.exp(-x)/(1+np.exp(-x))**2)),
     ('f',('f',lambda x : x)),
     ('f`',('f`',lambda x : x)),
     ('g',('g',lambda x : x)),
     ('g`',('g`',lambda x : x)),
     ('h',('h',lambda x : x)),
     ('h`',('h`',lambda x : x))
     #('softmax',('softmax',softmax)), softmax is not VSF.
     ])
    known_functions_code=dict(
     [(np.cos ,'np.cos({var})'),
      (np.sin ,'np.sin({var})'),
      (cosprime__,'-np.sin({var})'),
      (np.exp ,'np.exp({var})'),
      (np.log ,'np.log({var})'),
      (np.reciprocal,'np.reciprocal({var})'),
      (np.tanh,'np.tanh({var})'),
      (tanhprime__,'np.cosh({var})**-2'),
      ])
    expr_to_fun = dict(known_functions.values())
    func_to_expr=dict(zip(expr_to_fun.values(),expr_to_fun.keys()))
    def __init__(self, op_name, var, op=None):
        op_expr, op0 = VSF.known_functions.get(op_name, (op_name,op))
        if op:
            op0=op
        Node.__init__(self,"vsf") #"%s(%s)"%(op_name,var)
        #self.op_expr=op_expr #cos
        self.op=op0
        self.var=var
        #self.var.add_parent(self)
    def __unicode__(self):
        return u"%s(%s)"%(self.op_expr, self.var)
    def __repr__(self):
        return "VSF(%r)(%r)"%(self.op_expr, self.var)
    def code(self):
        return VSF.known_functions_code[self.op].format(var=self.var.code())
    def expression(self):
        if hasattr(self.var, 'expression'):
            return u"%s(%s)"%(self.op_expr, self.var.expression())
        return self.__str__()
    def simplify(self):
        tmp=self.var.simplify()
        if not tmp is self.var:
            self.var=tmp
            #self.var.add_parent(self)
        return self
    def diff_no_simplify(self, var):
        expr=CTimes(VSF(self.op_expr+"`", self.var),
                    self.var.diff_no_simplify(var))
        return expr
    @property
    def children(self):
        return [self.var]
    @property
    def variables(self):
        return self.var.variables
    @property
    def op_expr(self):
        return self.__class__.func_to_expr[self.op]
        #return self.op_expr
    @property
    def op_name(self):
        return self.__class__.func_to_expr[self.op]
        #return self.op_expr
    @property
    def val(self):
        if not self.op:
            return None
        v=super(self.__class__, self).val
        if v is None or np.all(np.isnan(v)):
            v = self.op(self.var.val)
            super(self.__class__, self.__class__).val.fset(self, v)
        return v

class Transpose(Node):
    __slots__ = ["op","var", "_format"]
    def __init__(self, x):
        Node.__init__(self,name='ᵀ')
        self.op=np.transpose
        self.var=x
        #self.var.add_parent(self)
        self._format=u"[%s]ᵀ"
        if isinstance(self.var, Var) or isinstance(self.var, Val):
            self._format=u"%sᵀ"
    def __unicode__(self):
        return self._format%(self.var)
    def __repr__(self):
        return "Transpose(%r)"%(self.var)
    def code(self):
        return "np.transpose({var})".format(var=self.var.code())
    def simplify(self):
        self.var=self.var.simplify()
        if IsScalar(self.var):
            return self.var
        #self.var.add_parent(self)
        return self
    @property
    def children(self):
        return [self.var]
    @property
    def variables(self):
        return self.var.variables
    @property
    def val(self):
        v=super(self.__class__, self).val
        if not np.any(v):
            v = np.transpose(self.var.val)
            super(self.__class__, self.__class__).val.fset(self, v)
        return v
    def diff_no_simplify(self, var):
        expr=Transpose(self.var.diff_no_simplify(var))
        return expr

def TransposeIfVector(var):
    if IsVector(var):
            return Transpose(var)
    return var

class BinaryOperator(Node):
    __slots__ = ["op","x","y","_format"]
    known_operators_code=dict(
     [(np.dot ,'np.dot({x},{y})'),
      (np.multiply ,'np.multiply({x},{y})'),
      (np.add ,'np.add({x},{y})'),
      ])
    def __init__(self, x, y):
        Node.__init__(self,name=None)
        self.op=None
        self._format=u"%s%s%s"
        self.x, self.y = x,y
        #self.x.add_parent(self)
        #self.y.add_parent(self)
    def __unicode__(self):
        return self._format%(self.x, self.name, self.y)
    def code(self):
        return BinaryOperator.known_operators_code[self.op].format(x=self.x.code(),y=self.y.code())
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
        #self.x.add_parent(self)
        self.y=self.y.simplify()
        #self.y.add_parent(self)
    @property
    def children(self):
        return [self.x, self.y]
    @property
    def variables(self):
        return self.x.variables.union(self.y.variables)
    @property
    def val(self):
        v=super(BinaryOperator, self).val
        if not np.any(v):
            v = self.op(self.x.val, self.y.val)
            super(BinaryOperator, self.__class__).val.fset(self, v)
        return v

class Add(BinaryOperator):
    __slots__ = []
    def __init__(self, x, y):
        BinaryOperator.__init__(self,x,y)
        self.name = u'+'
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
    __slots__ = []
    def __init__(self, x, y):
        BinaryOperator.__init__(self,x,y)
        self.name = u'*'
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
    __slots__ = []
    def __init__(self, x, y):
        BinaryOperator.__init__(self,x,y)
        self.name = u'⋅'
        self.op=np.dot
        #self.op=dot
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
        #TODO: cleanup index
        expr=Add(CTimes(TransposeIfVector(self.x.diff_no_simplify(var)),TransposeIfVector(self.y)),
                 CTimes(TransposeIfVector(self.x),TransposeIfVector(self.y.diff_no_simplify(var))))
        return expr

#CircleTimes : heavily used for differentiation by Matrix.
class CTimes(BinaryOperator):
    __slots__ = []
    def __init__(self, x, y):
        BinaryOperator.__init__(self,x,y)
        self.name = u'⊗'
        self.op = np.multiply
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
