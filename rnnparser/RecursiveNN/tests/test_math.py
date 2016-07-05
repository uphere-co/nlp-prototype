import sys
import os
sys.path.insert(0, os.environ.get('HOME')+'/nlp-prototype/rnnparser/RecursiveNN/')
import numpy as np
from recursiveNN.math import dot


def test_dot():
    for i in range(100):
        a=np.random.random((200,200)).astype(np.float32)
        b=np.random.random((200,1)).astype(np.float32)
        d=dot(a,b)
        d2=np.dot(a,b)
        #1e-6 seems to be best rtol for float32
        np.testing.assert_allclose(d2,d, rtol=1e-6, atol=0)
