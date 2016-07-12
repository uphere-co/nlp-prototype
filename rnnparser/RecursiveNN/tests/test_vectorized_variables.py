# -*- coding: utf-8 -*-
import sys
import os
sys.path.insert(0, os.environ.get('HOME')+'/nlp-prototype/rnnparser/RecursiveNN/')

import numpy as np
import pytest

from vecGraphComp.base import MatrixValues, ValueHolder, Block, NodeType, ExpressionWriter

def test_numpy_structure_of_arrays_with_expand_dims():
    m,n = 200,100
    x1=np.random.random((n,1))
    x2=np.random.random((n,1))
    A=np.random.random((m,n))
    xs = np.concatenate([np.expand_dims(x1,0),np.expand_dims(x2,0)],axis=0)

    Av=np.expand_dims(A,0)
    Axs = np.expand_dims(np.sum(Av*xs.transpose(0,2,1), axis=2),2)

    assert Axs.shape == (2, m,1)
    np.testing.assert_allclose(Axs[0],A.dot(x1), rtol=1e-7, atol=0.0)
    np.testing.assert_allclose(Axs[1],A.dot(x2), rtol=1e-7, atol=0.0)

def test_MatrixValues():
    vals=MatrixValues((3,5,5))
    val=np.ones((5,5))
    with pytest.raises(IndexError):
        vals[0]=val
    i=vals.save(val)
    assert np.all(vals[i]==val)
    val[1,:]=3
    #The `val` is not shared, but copied:
    assert not np.all(vals[i]==val)

    #Assignment uses numpy's broadcasting
    i=vals.save(2)
    assert np.all(vals[i]==2)
    #After the save, the value can be modified.
    vals[i]=3
    assert np.all(vals[i]==3)

    #np.nan should not be tested by `==np.nan' comparison. Use np.isnan
    i=vals.save(np.nan)
    assert np.all(np.isnan(vals[i]))

def test_ValueHolder():
    vs=ValueHolder(1000000)
    vals=vs[(5,1)]
    vals=vs[(5,5)]
    vidx=vals.save(np.array([2]*25).reshape((5,5)))
    vals2=vs[(5,5)]
    assert np.all(vals2[vidx]==2)
    sidx=vs.shape_index((5,5))
    vals3=vs[sidx]
    assert np.all(vals3[vidx]==2)

    #Indexing unknown shape raises ValueError exception.
    with pytest.raises(ValueError):
        vs.shape_index([500,500])

def test_NodeType():
    #Python Enum supports
    i=NodeType.Var.value
    assert isinstance(i, int)
    assert NodeType(i) == NodeType.Var

def test_Block():
    a=Block(1000)
    b=ExpressionWriter(a)
    name='abcdefghijklmn'
    b.Var(name, np.zeros((10,1)))
    a.uid(name)
    name=name[:2]+name[5:]
    #`name` is copied, not shared.
    with pytest.raises(ValueError):
        a.uid(name)

    b.Var(u'가', np.zeros((10,1)))
    #Declaring same name multiple times does nothing
    b.Var(u'가', np.zeros((10,1)))
    uid=b.Var(u'나', np.zeros((10,1)))
    assert a.uid(u'나')==uid
    assert a.name(uid)==u'나'
    with pytest.raises(ValueError):
        a.uid(u'다')
    assert a.name(a.uid(u'abcdefghijklmn'))==u'abcdefghijklmn'

    uid1=b.Var(u'foo', np.array(range(9)).reshape(3,3))
    uid2=b.Var(u'bar', np.array(range(9,18)).reshape(3,3))
    uid3=b.Var(u'x', np.array(range(6)).reshape(3,2))
    uid4=b.Var(u'y', np.array(range(6,12)).reshape(3,2))
    assert np.all(a.get_value(uid1)==np.array(range(9)).reshape(3,3))
    assert np.all(a.get_value(u'bar')==np.array(range(9,18)).reshape(3,3))
    assert np.all(a.get_value(uid3)==np.array(range(6)).reshape(3,2))
    assert np.all(a.get_value(u'y')==np.array(range(6,12)).reshape(3,2))

    word=u'\xa3'
    b.Var(word, np.zeros((10,1)))
