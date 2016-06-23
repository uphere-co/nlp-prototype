# -*- coding: utf-8 -*-
import numpy as np
from recursiveNN.nodes import Val,Var,VSF, Add,Dot,CTimes,Transpose, Sum0,TransposeIfVector
from recursiveNN.models import Word,Phrase
from recursiveNN.math import IsZero,IsAllOne,IsIdentity, IsScalar,IsVector,IsMatrix

def _AccumulateSideExpression(left, right):
    if IsMatrix(left) or IsMatrix(right):
        return Dot(left, right).simplify()
    return CTimes(left,right).simplify()
def _Diff(expr_left, expr, expr_right, var):
    if not (IsScalar(expr_left) or IsVector(expr_left)):
        print expr_left.val, expr.val, expr_right.val, var
        raise ValueError('Left-side expression should be a scalar or a vector since we do not process tesnsor.')
    elif not (IsScalar(expr_left) or IsVector(expr_left)):
        print expr_left.val, expr.val, expr_right.val, var
        raise ValueError('Right-side expression should be a scalar or a vector since we do not process tesnsor.')
    
    if isinstance(expr, Val):
        v=np.zeros(expr.val.shape)
        zero = Val(v)
        return zero
    elif isinstance(expr, Var):
        v=np.ones(expr.val.shape)
        if(var.name!= expr.name):
            v=np.zeros(expr.val.shape)
        val=Val(v)
        return CTimes(CTimes(Transpose(expr_left), val), Transpose(expr_right)).simplify()
    elif isinstance(expr, Dot):
        if not expr.x.isContain(var):
            return _Diff(_AccumulateSideExpression(expr_left, expr.x), expr.y, expr_right, var) 
        elif not expr.y.isContain(var):
            return _Diff(expr_left, expr.x, _AccumulateSideExpression(expr.y, expr_right), var)
        else:
            #print 'x: ', expr.x,expr.x.isContain(var), 'y: ', expr.y,expr.y.isContain(var), 'var: ',var
            x_dy= _Diff(_AccumulateSideExpression(expr_left, expr.x), expr.y, expr_right, var)
            dx_y= _Diff(expr_left, expr.x, _AccumulateSideExpression(expr.y, expr_right), var)
            return Add(x_dy, dx_y).simplify()
        assert 0
    elif isinstance(expr, Add):
        if not expr.x.isContain(var):
            return _Diff(expr_left, expr.y, expr_right, var)
        elif not expr.y.isContain(var):
            return _Diff(expr_left, expr.x, expr_right, var)
        else:
            #print 'x: ', expr.x,expr.x.isContain(var), 'y: ', expr.y,expr.y.isContain(var), 'var: ',var
            dx= _Diff(expr_left, expr.x, expr_right, var)
            dy= _Diff(expr_left, expr.y, expr_right, var)
            return Add(dx, dy).simplify()
        assert 0
    elif isinstance(expr, Transpose):        
        return Transpose(_Diff(expr_left, expr.var, expr_right, var))
    elif isinstance(expr, VSF):
        df = VSF(expr.op_name+"`", expr.var) 
        return _Diff(_AccumulateSideExpression(expr_left, Transpose(df)), expr.var, expr_right, var)
    elif isinstance(expr, Word) or isinstance(expr, Phrase):
        _Diff(expr_left, expr.vec, expr_right, var)    
    raise ValueError('Differentiation of unknown expression')
        
def Differentiation(expr, var):
    if not IsScalar(expr):
        raise ValueError('Differentiation on non-scalar value is not yet supported')
    return _Diff(Val(1), expr, Val(1), var)