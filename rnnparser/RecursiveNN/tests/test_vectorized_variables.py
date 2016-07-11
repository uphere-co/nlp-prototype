import sys
import os
sys.path.insert(0, os.environ.get('HOME')+'/nlp-prototype/rnnparser/RecursiveNN/')

import numpy as np
import pytest

from vecGraphComp.vnodes import MatrixValues, ValueHolder

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
