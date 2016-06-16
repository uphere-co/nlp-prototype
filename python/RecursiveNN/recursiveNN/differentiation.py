# -*- coding: utf-8 -*-
import numpy as np
from recursiveNN.nodes import Val,Var,VSF, Add,Dot,CTimes,Transpose, Sum0,TransposeIfVector
from recursiveNN.math import IsZero,IsAllOne,IsIdentity, IsScalar,IsVector,IsMatrix

def Sum0thAxisIfMatrix(expr):
    if IsMatrix(expr):
        return Sum0(expr)
    return expr
def _Diff(expr_left, expr, expr_right, var):
    if isinstance(expr, Val):
        v=np.zeros(expr.val.shape)
        zero = Val(v)
        return zero
    elif isinstance(expr, Var):
        v=np.ones(expr.val.shape)
        if(var.name!= expr.name):
            v=np.zeros(expr.val.shape)
        val=Val(v)
        #print expr_left.val, TransposeVectorsOnly(expr_left).val
        return CTimes(CTimes(expr_left, val), expr_right).simplify()
    elif isinstance(expr, Dot):
        expr_x,expr_y = expr.x, expr.y
        x_dy=_Diff(TransposeIfVector(CTimes(expr_left, expr_x).simplify()), expr_y, expr_right, var)
        dx_y= CTimes(_Diff(expr_left, expr_x, expr_right, var), TransposeIfVector(expr_y))
        return Add(x_dy, dx_y).simplify()
    raise ValueError('Differentiation of unknown expression')
        
        #if IsVexpr.x
def Differentiation(expr, var):
    if not IsScalar(expr):
        raise ValueError('Differentiation on non-scalar value is not yet supported')
    return _Diff(Val(1), expr, Val(1), var)