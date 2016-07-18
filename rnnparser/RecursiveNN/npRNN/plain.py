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
