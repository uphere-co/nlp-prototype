# -*- coding: utf-8 -*-
import numpy as np

def NormalizedMatrix(mat, l1norm):
    return l1norm*mat/np.abs(mat).sum()

def SimplifyIfScalar(arr):
    if np.product(arr.shape) ==1 :
        return arr.dtype.type(arr)
    return arr

def ScalarToMatrix(x):
    if IsScalarValue(x):
        return np.array(x).reshape(1,1)
    return x
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
