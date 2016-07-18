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

def rnn1(u,W0,b, word1,word2):
    word12=merge_word([word1,word2], True)
    x0=np.add(np.dot(W0, word12),b)
    h0=np.tanh(x0)
    score = np.dot(u,h0)
    left=u
    gradW0 = np.outer(left*dActi(x0), wordLR)
    return score,gradW0

def rnn2_score(u,W1,W0,b, words, merge_left):
    word12=merge_word(words[:2], True)
    x0,h0=activation(W0,b,word12)
    wordLR=merge_word([h0,words[2]], merge_left[0])
    x1,h1=activation(W1,b,wordLR)
    score = np.dot(u,h1)
    return score

def rnn2(u,W1,W0,b, words, merge_left):
    word12=merge_word(words[:2], True)
    x0,h0=activation(W0,b,word12)
    wordLR=merge_word([h0,words[2]], merge_left[0])
    x1,h1=activation(W1,b,wordLR)
    score = np.dot(u,h1)
    left=u
    gradW1 = np.outer(left*dActi(x1), wordLR)
    left=productLeftFactors(left, x1, W1)
    left=half(left,merge_left[0])
    gradW0 = np.outer(left*dActi(x0), word12)
    return score,gradW1,gradW0

def rnn3_score(u,W2,W1,W0,b, words, merge_left):
    word12=merge_word(words[:2], True)
    x0,h0=activation(W0,b,word12)
    wordLR1=merge_word([h0,words[2]], merge_left[0])
    x1,h1=activation(W1,b,wordLR1)
    wordLR2=merge_word([h1,words[3]], merge_left[0])
    x2,h2=activation(W2,b,wordLR2)
    h2=np.tanh(x2)
    return np.dot(u,h2)

def rnn3(u,W2,W1,W0,b, words, merge_left):
    word12=merge_word(words[:2], True)
    x0,h0=activation(W0,b,word12)
    wordLR1=merge_word([h0,words[2]], merge_left[0])
    x1,h1=activation(W1,b,wordLR1)
    wordLR2=merge_word([h1,words[3]], merge_left[0])
    x2,h2=activation(W2,b,wordLR2)
    h2=np.tanh(x2)

    score=np.dot(u,h2)

    left=u
    gradW2 =  np.outer(left*dActi(x2), wordLR2)
    left = productLeftFactors(left, x2, W2)
    left = half(left,merge_left[1])
    gradW1 = np.outer(left*dActi(x1), wordLR1)

    left=productLeftFactors(left, x1, W1)
    left=half(left,merge_left[0])
    gradW0 = np.outer(left*dActi(x0), word12)

    return score,gradW2,gradW1,gradW0
