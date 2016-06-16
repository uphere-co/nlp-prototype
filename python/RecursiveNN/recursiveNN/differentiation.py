# -*- coding: utf-8 -*-
import numpy as np
from recursiveNN.nodes import Val,Var,VSF, Add,Dot,CTimes,Transpose, Sum0,TransposeIfVector
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
        x_dy= _Diff(_AccumulateSideExpression(expr_left, expr.x), expr.y, expr_right, var)
        dx_y= _Diff(expr_left, expr.x, _AccumulateSideExpression(expr.y, expr_right), var)
        return Add(x_dy, dx_y).simplify()
    elif isinstance(expr, Add):
        dx= _Diff(expr_left, expr.x, expr_right, var)
        dy= _Diff(expr_left, expr.y, expr_right, var)
        return Add(dx, dy).simplify()
    elif isinstance(expr, VSF):
        df = VSF(expr.name+"`", expr.var) 
        return _Diff(_AccumulateSideExpression(expr_left, Transpose(df)), expr.var, expr_right, var)        
    raise ValueError('Differentiation of unknown expression')
        
        #if IsVexpr.x
def Differentiation(expr, var):
    if not IsScalar(expr):
        raise ValueError('Differentiation on non-scalar value is not yet supported')
    return _Diff(Val(1), expr, Val(1), var)