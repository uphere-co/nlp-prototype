import sys
import os
sys.path.insert(0, os.environ.get('HOME')+'/nlp-prototype/rnnparser/RecursiveNN/')
import numpy as np
import pytest

from recursiveNN.math import dot

def test_dot():
    for i in range(100):
        a=np.random.random((200,200)).astype(np.float32)
        b=np.random.random((200,1)).astype(np.float32)
        d=dot(a,b)
        d2=np.dot(a,b)
        #1e-6 seems to be best rtol for float32
        np.testing.assert_allclose(d2,d, rtol=1e-6, atol=0)

def test_numpy_inner_product_functions():
    ran=np.random.random

    a=np.ones((4,5))
    b=ran(5,)
    c=ran(4,)
    for i in range(4):
        assert np.all((a*b)[i]==b)
    assert np.all(a.dot(b)==a.dot(np.expand_dims(b,-1)))

    assert np.all(c*a.T ==a.T*c)
    with pytest.raises(ValueError):
        assert a*c
    assert np.all(c.dot(a)==np.expand_dims(c,0).dot(a))
    assert np.expand_dims(c,0).dot(a).shape == (1,5)
    with pytest.raises(ValueError):
        ran((4,2))*ran((2,2))

    np.testing.assert_allclose(c.dot(a).dot(b),c.dot(a.dot(b)), rtol=1e-06)

    #1-D index array is invariant to transpose operation.
    assert np.all(c.T==c)

def test_ctimes_identity():
    ran=np.random.random
    one=np.ones((5,5))
    v=ran(5,)
    v2=ran(5,)
    t1=v.reshape(5,1)*one*v2.reshape(1,5)
    t2=v2.reshape(1,5)*v.reshape(5,1)
    t3=v.reshape(5,1)*v2.reshape(1,5)
    np.testing.assert_allclose(t1,t2)
    np.testing.assert_allclose(t1,t3)
