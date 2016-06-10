import numpy as np

"""
>>> import numpy as np
>>> a=np.array([[25,5,1],[10,5,2]])
>>> b=np.array([1,3,9])
>>> c=np.array([[1],[2],[4]])
>>> assert(a.shape==(2,3))
>>> assert(b.shape==(3,))
>>> assert(c.shape==(3,1))
>>> assert(a.dot(b).shape==(2,))
>>> assert(a.dot(c).shape==(2,1))
>>> assert(np.all(a*b==b*a))
>>> a*b
array([[25, 15,  9],
       [10, 15, 18]])
>>> assert(np.all(c*b==b*c))
>>> b*c
array([[ 1,  3,  9],
       [ 2,  6, 18],
       [ 4, 12, 36]])
>>> d=np.array([1,3])
>>> e=np.array([[1],[2],[4]])
>>> f=np.array([1,2,4])

#Note: d*f is forbidden
>>> assert(np.all(d*e==e*d))
>>> d*e
array([[ 1,  3],
       [ 2,  6],
       [ 4, 12]])


"""
