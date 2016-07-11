# -*- coding: utf-8 -*-
import numpy as np
from numba import jitclass
from numba.decorators import jit, autojit
from numba import int32,uint64, float32, void,int_,float64, char

class VariableNames(object):
    def __init__(self, n_names, len_max):
        self._names = np.array(['_v%d'%i for i in range(n_names)], dtype='|S%s'%len_max)
    def __getitem__(self, i):
        return self._names[i] # = self.val[i,:,:]
    def __setitem__(self,i,name):
        self._names[i]=name
    def __unicode__(self):
        return unicode(self._names)
    def __str__(self):
        return self.__unicode__()
    def __repr__(self):
        return self.__unicode__()

class MatrixValues(object):
    #def __init__(self, m_row, n_col, n_vals):
    def __init__(self, shape, max_name_len=100):
        '''
        shape=(n_values, m, n) for n_values of m x n matrix values
        '''
        #self._val = np.zeros((n_vals, m_row, n_col))
        self._val = np.empty(shape)
        #self._names = VariableNames(n_vals, 100)
        self._names = VariableNames(shape[0], max_name_len)
        self._idx=0
    def __getitem__(self, i):
        return self.vals[i] # = self.val[i,:,:]
    def __setitem__(self,i,val):
        if isinstance(val, tuple):
            self.vals[i]=val[1]
            self.names[i]=val[0]
        else:
            self.vals[i]=val
    @property
    def n_vals(self):
        return self._idx
    @property
    def vals(self):
        return self._val
    @property
    def names(self):
        return self._names
    @property
    def n_max(self):
        return self._val.shape[0]
    def allocate(self):
        assert self._idx < self.n_max
        idx=self._idx
        self._idx+=1
        return idx
    def save(self, val):
        i=self.allocate()
        self[i]=val
        return i
    def reset(self):
        self._idx=0

class ValueHolder(object):
    def __init__(self, dim, n_val_max):
        self.dim = dim
        self.n_val_max=n_val_max
        self._dict = {}
        #TODO: generalize this for non-RNN use cases.
        self._dict[(dim,1)]=MatrixValues((n_val_max,dim,1))
        self._dict[(2*dim,1)]=MatrixValues((n_val_max,2*dim,1))
        self._dict[(dim,2*dim)]=MatrixValues((1000,dim,dim*2))
        self._dict[(1,1)]=MatrixValues((n_val_max,1,1))
    def __getitem__(self, value):
        shape=value.shape
        if not shape in self._dict.keys():
            self._dict[shape]=MatrixValues((self.n_val_max,)+shape)
        return self._dict[shape]
    def __call__(self,value):
        return self.__getitem__(value)
    def reset(self):
        for mem in self._dict.values():
            mem.reset()


class vVal(object):
    __slots__ = ["_mem", "_uid"]
    #TODO: factory should hold mem_table.
    mem_table = None
    @staticmethod
    def Init():
        vVal.mem_table = ValueHolder(200, 1000000)
    def __init__(self, val):
        self._mem = vVal.mem_table(val)
        self._uid=self._mem.save(val)
    @property
    def val(self):
        return self._mem[self._uid]
    @val.setter
    def val(self, val):
        raise TypeError('Val objects are immutable')
