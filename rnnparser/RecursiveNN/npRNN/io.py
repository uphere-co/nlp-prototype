# -*- coding: utf-8 -*-
import numpy as np
import pandas as pd

class Voca(object):
    def __init__(self, df):
        self._voca = df
    def word2vecs(self, words):
        #words_vec=self._voca.ix[words].values.astype(np.float32)
        words_vec=np.vstack([self._voca.ix[word].values.astype(np.float32) for word in words])
        return words_vec
    @property
    def dim(self):
        return self._voca.shape[-1]
