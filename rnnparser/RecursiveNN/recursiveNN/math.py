# -*- coding: utf-8 -*-
import numpy as np
from numba.decorators import jit, autojit
from numba import int32,uint64, float32, void,int_,float64, char

#@autojit
@jit(nopython=True,nogil=True)
def dot(a, b):
    l,m = a.shape
    m2,n = b.shape
    if m!=m2:
        raise ValueError('Incorrect dimensions')
    d = np.empty((l, n), dtype=a.dtype)
    for i in range(l):
        for j in range(n):
            tmp = 0.0
            for k in range(m):
                tmp += a[i, k]*b[k,j]
            d[i, j] = tmp
    return d
#dot = autojit(InnerProduct)
#Let dot compiled
bb=dot(np.ones((2,2)),np.ones((2,2)))

def NormalizedMatrix(mat, l1norm):
    return l1norm*mat/np.abs(mat).sum()

def SimplifyIfScalar(arr):
    if np.product(arr.shape) ==1 :
        return arr.dtype.type(arr)
    return arr

def To2Darray(x):
    if IsScalarValue(x):
        return np.array(x).reshape(1,1)
    v=np.array(x)
    return v
    if len(v.shape)==1:
        return v.reshape(-1,1)
    return v
    raise ValueError('x is not convertable to matrix')

def IsScalarValue(x):
    try:
        m=float(x)
        return True
    except:
        pass
    return False
def IsScalar(x):
    return IsScalarValue(x.val)
def IsVectorValue(x):
    try:
        if len(x.shape)==1:
            return True
        m,n=x.shape
        #m,n=x.val.shape[0],x.val.shape[1]
        return (m==1 and n>1) or (m>1 and n==1)
    except:
        pass
    return False
def IsVector(x):
    return IsVectorValue(x.val)
def IsMatrixValue(x):
    try:
        #m,n=x.val.shape[0],x.val.shape[1]
        m,n=x.shape
        return m>1 and n>1
    except:
        pass
    return False
def IsMatrix(x):
    return IsMatrixValue(x.val)

def IsZero(var):
    try :
        return np.all(var.val==0)
    except:
        pass
    return False
def IsAllOne(var):
    try :
        return np.all(var.val==1.0)
    except:
        pass
    return False
def IsIdentity(var):
    try :
        if not isinstance(var.val, np.ndarray) and var.val==1.0:
            return True
        else:
            d1=np.min(var.val.shape)
            d2=np.max(var.val.shape)
            if d1 != d2:
                return False
            return np.all(var.val==np.identity(d1))
    except:
        pass
    return False
