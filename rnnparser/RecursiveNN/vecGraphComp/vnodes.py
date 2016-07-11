# -*- coding: utf-8 -*-
import numpy as np
from enum import Enum, unique

class MatrixValues(object):
    def __init__(self, shape):
        '''
        shape=(max_vals, m, n) for max_vals values of m x n matrix value
        '''
        self._vals = np.empty(shape)
        self._idx=0
    #@property
    #def vals(self):
    #    return self._vals
    def __getitem__(self, i):
        return self._vals[i]
    def __setitem__(self,i,val):
        if not self._idx > i:
            raise IndexError('Index %d is not allocated yet'%i)
        self._vals[i]=val
    @property
    def n_max_vals(self):
        return self._vals.shape[0]
    def allocate(self):
        assert self._idx < self.n_max_vals
        idx=self._idx
        self._idx+=1
        return idx
    def save(self, val):
        i=self.allocate()
        self[i]=val
        return i
    def reset(self):
        self._idx=0

def is_integer(val):
    try:
        i=int(val)
        return True
    except:
        pass
    return False
class ValueHolder(object):
    def __init__(self, n_val_max):
        self._n_val_max=n_val_max
        self._holders = []
        self._shapes = []
    @property
    def n_val_max(self):
        return self._n_val_max
    def shape_index(self, shape):
        return self._shapes.index(shape)
    def holder(self, shape_index):
        return self._holders[shape_index]
    def __getitem__(self, sidx):
        if isinstance(sidx, tuple):
            shape=sidx
            try:
                shape_index=self.shape_index(shape)
            except ValueError:
                self._shapes.append(shape)
                self._holders.append(MatrixValues((self.n_val_max,)+shape))
                shape_index=self.shape_index(shape)
            return self.holder(shape_index)
        elif is_integer(sidx):
            shape_index=sidx
            return self.holder(shape_index)
        raise TypeError("idx should be integer or the numpy.array.shape")
    def __call__(self,idx):
        return self.__getitem__(idx)
    def reset(self):
        for mem in self._holders:
            mem.reset()
