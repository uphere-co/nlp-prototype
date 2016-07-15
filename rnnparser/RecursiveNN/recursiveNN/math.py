# -*- coding: utf-8 -*-
import numpy as np
from numba.decorators import jit, autojit
from numba import int32,uint64, float32, void,int_,float64, char

#@autojit
#@jit(nopython=True,nogil=True)
def dot(a, b):
#    return np.dot(a,b)
    l,m = a.shape[-2],a.shape[-1]
    m2,n = b.shape[-2],b.shape[-1]
    if m!=m2:
        raise ValueError('Incorrect dimensions')
    a2=np.expand_dims(a,-1)
    b2=np.expand_dims(b,-3)
    return np.sum(a2*b2, 1)
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
        if len(var.val.shape)==2:
            d1=var.val.shape[0]
            d2=var.val.shape[1]
            if d1 != d2:
                return False
            return np.all(var.val==np.identity(d1))
        elif len(var.val.shape)==0:
            return var.val==1
    except:
        pass
    return False
