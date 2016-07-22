# -*- coding: utf-8 -*-
import numpy as np
from param import Param

L1_normalize = lambda x : x/np.abs(x).sum()
class GradientDescent(object):
    def __init__(self, stepsize_W, stepsize_b, stepsize_u):
        self.W_step = stepsize_W
        self.b_step = stepsize_b
        self.u_step = stepsize_u
    def one_step(self, param, grad):
        param1         = param.copy()
        param1.W      += grad.W*self.W_step
        param1.bias   += grad.bias*self.b_step
        param1.u_score+= grad.u_score*self.u_step
        param1.u_score = L1_normalize(param.u_score)
        return param1
