import numpy as np

activation=np.tanh
dActi = lambda x : np.cosh(x)**-2

def merge_word(words, merge_left):
    wordL,wordR=words
    if merge_left:
        return np.concatenate(words)
    return np.concatenate(words[::-1])
def half(arr, left_half):
    assert(len(arr.shape)==1)
    halt_len=arr.shape[0]/2
    if left_half:
        return arr[:halt_len]
    return arr[halt_len:]
def activation(W,b,wordLR):
    x=np.add(np.dot(W,wordLR),b)
    h=np.tanh(x)
    return x,h
def productLeftFactors(left_factor, x, W):
    return left_factor.reshape(1,-1).dot(dActi(x).reshape(-1,1)*W).reshape(-1)

def rnn(depth, u,Ws,b, words, merge_left):
    merge_left=[True]+merge_left
    xs=np.empty((depth, b.shape[0]))
    hs=np.empty((depth, b.shape[0]))
    wordLRs=np.empty((depth,b.shape[0]*2))

    word_iter=iter(words)
    wordL=word_iter.next()
    for i in range(depth):
        wordR=word_iter.next()
        wordLRs[i]=merge_word([wordL,wordR], merge_left[i])
        xs[i],hs[i]=activation(Ws[i],b,wordLRs[i])
        wordL=hs[i]
    score=np.dot(u,hs[-1])

    grad_Ws = [np.empty(Ws[0].shape)]*depth
    grad_Ws2 = [np.empty(Ws[0].shape)]*depth
    left=u
    for i in range(1, depth, 1):
        grad_Ws[-i] =  np.outer(left*dActi(xs[-i]), wordLRs[-i])
        left = productLeftFactors(left, xs[-i], Ws[-i])
        left = half(left,merge_left[-i])
    grad_Ws[-depth] = np.outer(left*dActi(xs[-depth]), wordLRs[-depth])

    return score,grad_Ws


def update_current_words(words, idxs, loc, new_word, it_word):
    words[it_word]=new_word
    idxs[loc:loc+2]=[it_word]
    return it_word+1

def update_current_word_pairs(words,idxs_word, wpairs,idxs_wpair, loc,new_word, it_wpair ):
    assert np.all(words[idxs_word[loc]]==new_word)
    if loc == 0:
        idxs_wpair[loc:loc+2] = [it_wpair]
        new_wpairR = merge_word([new_word, words[idxs_word[loc+1]]], True)
        wpairs[it_wpair] = new_wpairR
        return it_wpair+1
    elif loc == len(idxs_wpair)-1:
        idxs_wpair[loc-1:loc+1] = [it_wpair]
        new_wpairL = merge_word([words[idxs_word[loc-1]],new_word], True)
        wpairs[it_wpair] = new_wpairL
        return it_wpair+1
    else:
        idxs_wpair[loc-1:loc+2] = [it_wpair,it_wpair+1]
        new_wpairL = merge_word([words[idxs_word[loc-1]],new_word], True)
        new_wpairR = merge_word([new_word, words[idxs_word[loc+1]]], True)
        wpairs[it_wpair:it_wpair+2] = [new_wpairL,new_wpairR]
        return it_wpair+2
