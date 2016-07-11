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

@unique
class NodeType(Enum):
    Var = 1
    F = 2
    Add = 3
    Dot = 4
    CTimes = 5

class Block(object):
    '''
    _names : ASCII string.
    '''
    def __init__(self, size):
        len_max=100
        self._names = np.empty((size), dtype='|S%s'%len_max)
        self._idx=0
        self._values_holder=ValueHolder(1000000)
        self._values_info = np.empty((1000000, 5), dtype=np.int64)
    @property
    def n_values(self):
        return self._idx
    def is_declared(self, name):
        idxs=np.where(self._names[:self._idx]==name)[0]
        if len(idxs)>0:
            return True
        return False
    def declare(self, name, val=None):
        '''
        name : unicode string. Internally, stored as ASCII string
        '''
        name=name.encode('utf-8')
        if self.is_declared(name):
            raise ValueError('%s is already declared'%name)
        uid=self.n_values
        self._names[uid]=name
        self._idx +=1
        if val is not None:
            vals=self._values_holder[val.shape]
            sidx=self._values_holder.shape_index(val.shape)
            vidx=vals.save(val)
            self._values_info[uid]=[NodeType.Var.value,sidx,vidx,-1,-1]
        return uid
    def node_type(self, uid):
        return self._values_info[uid]
    def get_value(self, key):
        if isinstance(key, unicode):
            name=key
            uid=self.uid(name)
        elif isinstance(key, int):
            uid=key
        else :
            raise TypeError("key should be either name or uid")
        _,sidx,vidx,_,_ = self.node_type(uid)
        vals=self._values_holder[sidx]
        return vals[vidx]
    def uid(self, name):
        name=name.encode('utf-8')
        if not self.is_declared(name):
            raise ValueError('%s is not declared'%name)
        idxs=np.where(self._names[:self._idx]==name)[0]
        return idxs[0]
    #def __getitem__(self, uid):
    def name(self, uid):
        if not self._idx > uid:
            raise IndexError('%d is not declared'%uid)
        return self._names[uid].decode('utf-8')
