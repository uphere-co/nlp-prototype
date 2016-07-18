import numpy as np
from npRNN.plain import *

ran=lambda x : np.random.random(x).astype(np.float64)-0.5
vecs=ran((5,200))
delta= lambda x ,scale: scale*ran(x.shape)
W=ran((200,400))
b=ran((200,))
u=ran((200,))
scale=0.0001
dW=delta(W,scale)
db=delta(b,scale)
du=delta(u,scale)

word1,word2,word3,word4=vecs[0],vecs[1],vecs[2],vecs[3]

print 'RNN2'
rnn2_merge_left=[False]
diff_rnn2= lambda x : x - rnn2(u,W,W,b, word1,word2,word3, rnn2_merge_left)
f=relativeError(diff_rnn2(rnn2(u,W,W+dW,b, word1,word2,word3, rnn2_merge_left)))
grad=drnn2dW0(u,W,W,b, word1,word2,word3, rnn2_merge_left)
print show_summary(f,grad,dW)
f=relativeError(diff_rnn2(rnn2(u,W+dW,W,b, word1,word2,word3, rnn2_merge_left)))
grad=drnn2dW1(u,W,W,b, word1,word2,word3, rnn2_merge_left)
print show_summary(f,grad,dW)

print 'RNN3:'
rnn3_merge_left=[False,False]
diff_rnn3= lambda x : x - rnn3(u,W,W,W,b, word1,word2,word3,word4, rnn3_merge_left)
f=relativeError(diff_rnn3(rnn3(u,W,W,W+dW,b, word1,word2,word3,word4, rnn3_merge_left)))
grad=drnn3dW0(u,W,W,W,b, word1,word2,word3,word4, rnn3_merge_left)
print show_summary(f,grad,dW)
f=relativeError(diff_rnn3(rnn3(u,W,W+dW,W,b, word1,word2,word3,word4, rnn3_merge_left)))
grad=drnn3dW1(u,W,W,W,b, word1,word2,word3,word4, rnn3_merge_left)
print show_summary(f,grad,dW)
f=relativeError(diff_rnn3(rnn3(u,W+dW,W,W,b, word1,word2,word3,word4, rnn3_merge_left)))
grad=drnn3dW2(u,W,W,W,b, word1,word2,word3,word4, rnn3_merge_left)
print show_summary(f,grad,dW)
