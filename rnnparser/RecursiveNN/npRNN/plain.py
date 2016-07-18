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

def rnn1(u,W0,b, word1,word2):
    x0=np.add(np.dot(W0, np.concatenate([word1,word2])),b)
    h0=np.tanh(x0)
    return np.dot(u,h0)

def rnn2(u,W1,W0,b, word1,word2,word3, merge_left):
    word12=merge_word([word1,word2], True)
    x0,h0=activation(W0,b,word12)
    wordLR=merge_word([h0,word3], merge_left[0])
    x1,h1=activation(W1,b,wordLR)
    return np.dot(u,h1)
def drnn2dW1(u,W1,W0,b, word1,word2,word3, merge_left):
    word12=merge_word([word1,word2], True)
    x0,h0=activation(W0,b,word12)
    wordLR=merge_word([h0,word3], merge_left[0])
    x1,h1=activation(W1,b,wordLR)
    left=u
    return np.outer(left*dActi(x1), wordLR)


def drnn2dW0(u,W1,W0,b, word1,word2,word3, merge_left):
    word12=merge_word([word1,word2], True)
    x0,h0=activation(W0,b,word12)
    wordLR=merge_word([h0,word3], merge_left[0])
    x1,h1=activation(W1,b,wordLR)
    left=u
    left=productLeftFactors(left, x1, W1)
    left=half(left,merge_left[0])
    return np.outer(left*dActi(x0), word12)

def rnn3(u,W2,W1,W0,b, word1,word2,word3,word4, merge_left):
    word12=merge_word([word1,word2], True)
    x0,h0=activation(W0,b,word12)
    wordLR1=merge_word([h0,word3], merge_left[0])
    x1,h1=activation(W1,b,wordLR1)
    wordLR2=merge_word([h1,word4], merge_left[0])
    x2,h2=activation(W2,b,wordLR2)
    h2=np.tanh(x2)
    return np.dot(u,h2)
def drnn3dW0(u,W2,W1,W0,b, word1,word2,word3,word4, merge_left):
    word12=merge_word([word1,word2], True)
    x0,h0=activation(W0,b,word12)
    wordLR1=merge_word([h0,word3], merge_left[0])
    x1,h1=activation(W1,b,wordLR1)
    wordLR2=merge_word([h1,word4], merge_left[0])
    x2,h2=activation(W2,b,wordLR2)
    left=u
    left=productLeftFactors(left, x2, W2)
    left=half(left,merge_left[1])
    left=productLeftFactors(left, x1, W1)
    left=half(left,merge_left[0])
    return np.outer(left*dActi(x0), word12)
def drnn3dW1(u,W2,W1,W0,b, word1,word2,word3,word4, merge_left):
    word12=merge_word([word1,word2], True)
    x0,h0=activation(W0,b,word12)
    wordLR1=merge_word([h0,word3], merge_left[0])
    x1,h1=activation(W1,b,wordLR1)
    wordLR2=merge_word([h1,word4], merge_left[0])
    x2,h2=activation(W2,b,wordLR2)
    left=u
    left=productLeftFactors(left, x2, W2)
    left=half(left,merge_left[1])
    return np.outer(left*dActi(x1), wordLR1)
def drnn3dW2(u,W2,W1,W0,b, word1,word2,word3,word4, merge_left):
    word12=merge_word([word1,word2], True)
    x0,h0=activation(W0,b,word12)
    wordLR1=merge_word([h0,word3], merge_left[0])
    x1,h1=activation(W1,b,wordLR1)
    wordLR2=merge_word([h1,word4], merge_left[0])
    x2,h2=activation(W2,b,wordLR2)
    left=u
    return np.outer(left*dActi(x2), wordLR2)
