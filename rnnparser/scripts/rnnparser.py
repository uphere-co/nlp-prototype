import pytest
import numpy as np
import pandas as pd
import logging
np.random.seed(132)

from npRNN.rnn import Parser
from npRNN.optimizer import GradientDescent
from npRNN.param import Param
from npRNN.io import Voca
from npRNN.util import get_short_uuid, write_to_disk

L1norm = lambda x : np.abs(x).sum()

class TaskRunner(object):
    @classmethod
    def _set_logger(cls, logger):
        log_path = "/data/groups/uphere/log/app.log"
        logger.setLevel(logging.DEBUG)
        fh = logging.FileHandler(log_path)
        formatter = logging.Formatter('%(asctime)s\t%(name)s\t%(levelname)s\t%(message)s')
        fh.setFormatter(formatter)
        logger.addHandler(fh)
        logger.info("Task launched")
        
    def __init__(self, uid, voca):
        self.uid      = uid
        self.name     = "rnn.Parser#%s"%self.uid
        self.logger   = logging.getLogger(self.name)
        self._set_logger(self.logger)
        self.rnn      = Parser(np.tanh, lambda x : np.cosh(x)**-2)
        self.voca     = voca
        dim=self.voca.dim
        self.param    = Param.random(dim)
        self.optimizer= GradientDescent(05.0/(dim*dim), 10.0/dim, 10.0/dim)

    def training(self, n_epoch, trainset,testset):
        logger       = self.logger
        voca         = self.voca
        rnn          = self.rnn
        param        = self.param
        grad_descent = self.optimizer        
        size_minibatch = 100
        
        i_minibatch=0
        grad_sum=param.zero()
        logger.info("Training started.")           
        write_to_disk('{name}.{idx}'.format(name=self.name,idx=i_minibatch), param.to_arr())
        for epoch in range(n_epoch):
            logger.info("Epoch %d started"%epoch)
            for i,sentence in enumerate(trainset):
                sentence=sentence.decode('utf-8').strip()
                input_words=sentence.split()
                if len(input_words)<2:
                    continue
                try:
                    words_vec=voca.word2vecs(input_words)
                    phrases=rnn.merge_words(input_words,words_vec, param)
                    grad =rnn.backward(phrases, param)
                    grad_sum += grad
                except:
                    #logger.debug("Exception : %s"%sentence)
                    pass

                if i>0 and i%size_minibatch == 0:
                    i_minibatch+=1
                    logger.info("Minibatch       : %d  L1 norm:W,b,u  %f %f %f"%(i_minibatch,L1norm(param.W),L1norm(param.bias),L1norm(param.u_score)))
                    grad_sum *= (1.0/size_minibatch)
                    param=grad_descent.one_step(param, grad_sum)
                    grad_sum=param.zero()
                    if i_minibatch%100==0:
                        logger.info("Progress : %d  W L1 norm: %f"%(i,np.abs(param.W).sum()))
                        write_to_disk('{name}.{idx}'.format(name=self.name,idx=i_minibatch), param.to_arr())
                        scoring(voca, rnn,param,testset, self.logger)
            logger.info("Epoch %d finished"%epoch)
        logger.info("Training finished.")
        
def scoring(voca,rnn,param, dataset, logger):
    score=0
    n=0
    logger.info("Evaluating testset")
    for i,sentence in enumerate(dataset):
        try:
            sentence=sentence.strip().decode('utf-8')
            input_words=sentence.split()
            words_vec=voca.word2vecs(input_words)
            _, scores, _=rnn.forward(words_vec, param)
            if not np.isnan(np.sum(scores)):
                score+=np.sum(scores)
                n+=1
            else:
                pass
                #logger.debug("Nan: %s"%sentence)
        except:
            #logger.debug("Exception: %s"%sentence)
            pass    
    logger.info("Testset average score : %f"%(score/n))
    return score/n

import json    
import sys
if __name__ == "__main__":
    config_path=sys.argv[1]
    with open(config_path, 'r') as f:
        config=json.load(f)
        
    data_path=config['data_path']
    voca_name=config['voca_name']
    voca_path=config['voca_path']

    store = pd.HDFStore(voca_path)
    assert store.is_open
    with open(data_path, 'r') as f:
        data=f.readlines()
    voca = Voca(store[voca_name])
    n_test=5000
    testset = data[:5000]
    trainset = data[5000:]

    task_uid = get_short_uuid()
    task=TaskRunner(task_uid, voca)
    task.training(10, trainset, testset)

