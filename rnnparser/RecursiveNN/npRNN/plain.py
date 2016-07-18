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
def relativeError(base):
    return lambda x : x/base - 1
def activation(W,b,wordLR):
    x=np.add(np.dot(W,wordLR),b)
    h=np.tanh(x)
    return x,h
def productLeftFactors(left_factor, x, W):
    return left_factor.reshape(1,-1).dot(dActi(x).reshape(-1,1)*W).reshape(-1)
def show_summary(error, grad,dW):
    v=np.sum(grad*dW)
    grad_wrong=grad.copy()
    np.random.shuffle(grad_wrong)
    v_wrong=np.sum(grad_wrong*dW)
    return np.array([v, error(v), error(v_wrong)])

def rnn1_score(u,W0,b, word1,word2):
    word12=merge_word([word1,word2], True)
    x0=np.add(np.dot(W0, word12),b)
    h0=np.tanh(x0)
    score= np.dot(u,h0)
    return score

def rnn2_score(u,W1,W0,b, words, merge_left):
    word12=merge_word(words[:2], True)
    x0,h0=activation(W0,b,word12)
    wordLR=merge_word([h0,words[2]], merge_left[0])
    x1,h1=activation(W1,b,wordLR)
    score = np.dot(u,h1)
    return score

def rnn3_score(u,W2,W1,W0,b, words, merge_left):
    word12=merge_word(words[:2], True)
    x0,h0=activation(W0,b,word12)
    wordLR1=merge_word([h0,words[2]], merge_left[0])
    x1,h1=activation(W1,b,wordLR1)
    wordLR2=merge_word([h1,words[3]], merge_left[0])
    x2,h2=activation(W2,b,wordLR2)
    return np.dot(u,h2)

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
