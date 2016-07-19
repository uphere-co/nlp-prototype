import numpy as np
from plain import *

float_type = np.float32
int_type = np.int64
class Parser(object):
    def __init__(self, u_score, W, bias, activation_f, activation_df):
        self.u_score = u_score
        self.W       = W
        self.bias    = bias
        self.activation_f = activation_f
        self.activation_df= activation_df

        self.words     =None
        self.wpairs    =None
        self.xs_wpair  =None
        self.hs_wpair  =None
        self.scores_wpair=None

        self.idxs_word =None
        self.idxs_wpair=None
        self.it_word   =None
        self.it_wpai   =None
    def reset(self, n_words, dim_word):
        # *_wpair is for storing values applied on all word pairs.
        self.words         =np.empty((n_words*2-1,dim_word),  dtype=float_type)
        self.wpairs        =np.empty((n_words*3,  dim_word*2),dtype=float_type)
        self.xs_wpair      =np.empty((n_words*3,  dim_word),  dtype=float_type)
        self.hs_wpair      =np.empty((n_words*3,  dim_word),  dtype=float_type)
        self.scores_wpair  =np.empty( n_words*3,              dtype=float_type)

    def forward(self, words_vec, Ws=None):
        n_words,dim_word=words_vec.shape
        self.reset(n_words,dim_word)

        scores        = np.empty(n_words-2, dtype=float_type)
        merge_history = np.empty(n_words-2, dtype=int_type)

        idxs_word = range(n_words)
        idxs_wpair =range(n_words-1)
        it_word=n_words
        it_wpair=n_words-1

        if Ws is not None:
            W=Ws[0]
        else:
            W=self.W
        bias=self.bias
        u_score=self.u_score

        self.words       [idxs_word]  = words_vec
        self.wpairs      [idxs_wpair] = np.concatenate([words_vec[:-1],words_vec[1:]], axis=1)
        self.xs_wpair    [idxs_wpair] = hidden_vectorized(W,bias, self.wpairs[:n_words-1])
        self.hs_wpair    [idxs_wpair] = self.activation_f(self.xs_wpair[idxs_wpair])
        self.scores_wpair[idxs_wpair] = scoring(u_score, self.hs_wpair[idxs_wpair])

        for epoch in range(n_words-2):
            if Ws is not None:
                W=Ws[epoch]
            else:
                W=self.W
            words_current        = self.words[idxs_word]
            wpairs_current       = self.wpairs[idxs_wpair]
            hs_wpair_current     = self.hs_wpair[idxs_wpair]
            scores_wpair_current = self.scores_wpair[idxs_wpair]
            #assert np.all(np.concatenate([words_current[:-1],words_current[1:]], axis=1)== wpairs_current)
            loc = np.argmax(scores_wpair_current)
            merge_history[epoch]=loc
            scores[epoch]=scores_wpair_current[loc]
            h=hs_wpair_current[loc]
            it_word =update_current_words(self.words, idxs_word, loc, h, it_word)
            it_wpair_new=update_current_word_pairs(self.words, idxs_word, self.wpairs,idxs_wpair, loc,h, it_wpair)
            self.scores_wpair[it_wpair:it_wpair_new] = scoring(u_score,  \
                   self.activation_f(hidden_vectorized(W,bias, self.wpairs[it_wpair:it_wpair_new])))
            it_wpair=it_wpair_new
        return scores,merge_history
