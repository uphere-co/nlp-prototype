import sys
import os
sys.path.insert(0, os.environ.get('HOME')+'/nlp-prototype/rnnparser/RecursiveNN/')

import numpy as np
import pytest

from recursiveNN.vnodes import ValueHolder
from recursiveNN.nodes import reset_NodeDict

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

def test_ValueHolder():
    dim=200
    vals=ValueHolder(dim, 100000)

    for shape in [(dim,1),(2*dim,1), (dim,dim*2), (1,1)]:
        v=np.random.random(shape)
        mem=vals(v)
        assert mem.n_vals==0
        i=mem.save(v)
        assert i==0
        assert mem.n_vals==1
    vals.reset()
    for shape in [(dim,1),(2*dim,1), (dim,dim*2), (1,1)]:
        v=np.random.random(shape)
        mem=vals(v)
        assert mem.n_vals==0
