# -*- coding: utf-8 -*-
import numpy as np
from npRNN.plain import *
from npRNN.testing_helper import update_current_words, update_current_word_pairs

def test_toymodel():
    def phrase(word_pair):
        half_len=word_pair.shape[0]/2
        return word_pair[:half_len]+word_pair[half_len:]
    def merge_word(word1,word2):
        return np.concatenate([word1,word2])

    #Define data layout
    #Start from 8 words.
    n_words=8
    dim_word=5
    words=np.empty((n_words*2-1,dim_word))
    wpairs=np.empty((n_words*3,dim_word*2))
    words[:n_words]=np.tile([2**i for i in range(n_words)],(dim_word,1)).T
    wpairs[:n_words-1]= np.concatenate([words[:n_words-1],words[1:n_words]], axis=1)
    idxs_word = range(n_words)
    idxs_wpair = range(n_words-1)
    it_word=n_words
    it_wpair=n_words-1

    merge_history=[3,1,1,0,2,1,0]

    expected_it_word  = [9,10,11,12,13,14,15]
    expected_it_wpair = [9,11,13,14,15,16,17]
    expected_idxs_word = [[0,1,2,8,5,6,7], [0,9,8,5,6,7], [0,10,5,6,7],\
                          [11,5,6,7], [11,5,12],[11,13],[14]]
    expected_idxs_wpair = [[0,1,7,8,5,6], [9,10,8,5,6],\
                           [11,12,5,6], [13,5,6], [13,14], [15],[]]

    for epoch in range(7):
        loc=merge_history[epoch]
        words_current =words[idxs_word]
        wpairs_current=wpairs[idxs_wpair]
        h=phrase(wpairs_current[loc])
        it_word=update_current_words(words, idxs_word, loc, h, it_word)
        it_wpair=update_current_word_pairs(words,idxs_word, wpairs,idxs_wpair, loc,h, it_wpair)
        assert it_word == expected_it_word[epoch]
        assert it_wpair== expected_it_wpair[epoch]
        assert idxs_word  == expected_idxs_word[epoch]
        assert idxs_wpair == expected_idxs_wpair[epoch]
        words_current =words[idxs_word]
        wpairs_current=wpairs[idxs_wpair]
        #check consistency between words and word pairs
        assert np.all(np.concatenate([words_current[:-1],words_current[1:]], axis=1)== wpairs_current)

    assert np.all(words[it_word-1] == np.array([[255]*5]))
    assert np.all(wpairs_current == np.array([[31]*5+[224]*5]))
