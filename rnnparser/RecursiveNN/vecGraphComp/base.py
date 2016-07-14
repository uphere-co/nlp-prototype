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
    Var = 0
    VSF = 1000
    Transpose = 1001
    Add = 2000
    Dot = 2001
    CTimes = 2002

class Block(object):
    '''
    _names : ASCII string.
    '''
    def __init__(self, size):
        len_max=100
        self._names = np.empty((size), dtype='|S%s'%len_max)
        self._idx=0
        self._values_holder=ValueHolder(size)
        self._values_info = np.empty((size, 5), dtype=np.int64)
    @property
    def n_values(self):
        return self._idx
    def is_declared(self, name):
        return name.encode('utf-8') in self._names[:self.n_values]
    def node_type(self, uid):
        return self._values_info[uid]
    def get_value(self, key):
        if isinstance(key, unicode):
            name=key
            uid=self.uid(name)
        elif is_integer(key):
            uid=key
        else :
            raise TypeError("key should be either name(unicode) or uid")
        _,sidx,vidx,_,_ = self.node_type(uid)
        vals=self._values_holder[sidx]
        return vals[vidx]
    def uid(self, name):
        if not self.is_declared(name):
            raise ValueError('%s is not declared'%name)
        return np.argmax(self._names[:self._idx]==name.encode('utf-8'))
    #def __getitem__(self, uid):
    def name(self, uid):
        #TODO: this assumes uid start from 1.
        if not self._idx > uid:
            raise IndexError('%d is not declared'%uid)
        return self._names[uid].decode('utf-8')

class ExpressionWriter(object):
    def __init__(self, block):
        self._block=block
    def Var(self, name, val):
        '''
        name : unicode string. Internally, stored as ASCII string
        '''
        block=self._block
        if block.is_declared(name):
            return block.uid(name)
        uid=block.n_values
        block._names[uid]=name.encode('utf-8')
        block._idx +=1
        vals=block._values_holder[val.shape]
        sidx=block._values_holder.shape_index(val.shape)
        vidx=vals.save(val)
        block._values_info[uid]=[NodeType.Var.value,sidx,vidx,-1,-1]
        return uid
    def Dot(self, uid1,uid2):
        pass
