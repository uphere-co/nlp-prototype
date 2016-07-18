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
    words=np.empty((15,5))
    wpairs=np.empty((24,10))
    words[:8]=np.tile([2**i for i in range(8)],(5,1)).T
    wpairs[:7]= np.concatenate([words[:7],words[1:8]], axis=1)
    #Start from 8 words.
    idxs_word = range(8)
    idxs_wpair = range(7)
    it_word=8
    it_wpair=7
    words_current =words[idxs_word]
    wpairs_current=wpairs[idxs_wpair]
    loc=3
    h=phrase(wpairs_current[loc])
    it_word=update_current_words(words, idxs_word, loc, h, it_word)
    it_wpair=update_current_word_pairs(words,idxs_word, wpairs,idxs_wpair, loc,h, it_wpair)
    assert it_word == 9
    assert it_wpair== 9
    assert idxs_word  == [0,1,2,8,5,6,7]
    assert idxs_wpair == [0,1,7,8,5,6]
    words_current =words[idxs_word]
    wpairs_current=wpairs[idxs_wpair]
    assert np.all(np.concatenate([words_current[:-1],words_current[1:]], axis=1)== wpairs_current)

    loc=1
    h=phrase(wpairs_current[loc])
    it_word=update_current_words(words, idxs_word, loc, h, it_word)
    it_wpair=update_current_word_pairs(words,idxs_word, wpairs,idxs_wpair, loc,h, it_wpair)
    assert it_word == 10
    assert it_wpair== 11
    assert idxs_word  == [0,9,8,5,6,7]
    assert idxs_wpair == [9,10,8,5,6]
    words_current =words[idxs_word]
    wpairs_current=wpairs[idxs_wpair]
    assert np.all(np.concatenate([words_current[:-1],words_current[1:]], axis=1)== wpairs_current)

    loc=1
    h=phrase(wpairs_current[loc])
    it_word=update_current_words(words, idxs_word, loc, h, it_word)
    it_wpair=update_current_word_pairs(words,idxs_word, wpairs,idxs_wpair, loc,h, it_wpair)
    assert it_word == 11
    assert it_wpair== 13
    assert idxs_word  == [0,10,5,6,7]
    assert idxs_wpair == [11,12,5,6]
    words_current =words[idxs_word]
    wpairs_current=wpairs[idxs_wpair]
    assert np.all(np.concatenate([words_current[:-1],words_current[1:]], axis=1)== wpairs_current)

    loc=0
    h=phrase(wpairs_current[loc])
    it_word=update_current_words(words, idxs_word, loc, h, it_word)
    it_wpair=update_current_word_pairs(words,idxs_word, wpairs,idxs_wpair, loc,h, it_wpair)
    assert it_word == 12
    assert it_wpair== 14
    assert idxs_word  == [11,5,6,7]
    assert idxs_wpair == [13,5,6]
    words_current =words[idxs_word]
    wpairs_current=wpairs[idxs_wpair]
    assert np.all(np.concatenate([words_current[:-1],words_current[1:]], axis=1)== wpairs_current)

    loc=2
    h=phrase(wpairs_current[loc])
    it_word=update_current_words(words, idxs_word, loc, h, it_word)
    it_wpair=update_current_word_pairs(words,idxs_word, wpairs,idxs_wpair, loc,h, it_wpair)
    assert it_word == 13
    assert it_wpair== 15
    assert idxs_word  == [11,5,12]
    assert idxs_wpair == [13,14]
    words_current =words[idxs_word]
    wpairs_current=wpairs[idxs_wpair]
    assert np.all(np.concatenate([words_current[:-1],words_current[1:]], axis=1)== wpairs_current)

    loc=1
    h=phrase(wpairs_current[loc])
    it_word=update_current_words(words, idxs_word, loc, h, it_word)
    it_wpair=update_current_word_pairs(words,idxs_word, wpairs,idxs_wpair, loc,h, it_wpair)
    assert it_word == 14
    assert it_wpair== 16
    assert idxs_word  == [11,13]
    assert idxs_wpair == [15]
    words_current =words[idxs_word]
    wpairs_current=wpairs[idxs_wpair]
    assert np.all(np.concatenate([words_current[:-1],words_current[1:]], axis=1)== wpairs_current)
    assert np.all(wpairs_current == np.array([[31]*5+[224]*5]))
