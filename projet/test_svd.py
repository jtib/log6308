import numpy as np
import time
from itertools import product
from sklearn.metrics.pairwise import cosine_similarity
from scipy import sparse

import small_script

dic = small_script.dic
locations = [it[0] for it in small_script.items]
nu = len(dic)
ni = len(locations)

donnees = np.zeros((nu,ni))
dic_users = {ind:u for ind,u in enumerate(dic)}
dic_items = {ind:i for ind,i in enumerate(locations)}
dic_items_rev = {i:ind for ind,i in dic_items.items()}

for u in range(nu):
    indices = [dic_items_rev[i] for i in dic[dic_users[u]]]
    donnees[u][indices] = 1

# Normalizing
## User average
u_average = np.mean(donnees, axis=1, keepdims=True)
## Substraction
donnees_norm = donnees - u_average

# SVD decomposition
U,s,V = np.linalg.svd(donnees_norm, full_matrices=False)

# Using 16 dimensions (same as gensim projection)
s_16 = s[:16]
U_16 = U[:,:16]
V_16 = V[:16,:]
donnees_16 = np.dot((np.dot(U_16, np.diag(s_16))), V_16)

# Final matrix
## Denormalizing
C = donnees_16 + u_average
# 5 best items (indexes)
mSVD_pred = (np.argsort(C))[:,:5]
# Corresponding places
dSVD_pred = {}
for i,j in product(range(mSVD_pred.shape[0]), range(mSVD_pred.shape[1])):
    u = dic_users[i]
    if u in dSVD_pred:
        dSVD_pred[u].append(dic_items[mSVD_pred[i][j]])
    else:
        dSVD_pred[u] = [dic_items[mSVD_pred[i][j]]]

# Other approach : top n neighbours
## Distances (cosinus)
Us = np.dot(U_16, np.diag(np.sqrt(s_16)))
u_sim = cosine_similarity(Us) #between users
# 5 closest neighbors (users)
neighbours = np.argsort(u_sim)[:,:5]
# 10 places in which they've checked in most frequently
#ranking = {}
#for u,u_neigh in product(range(neighbours.shape[0]), range(neighbours.shape[1])):

# Display

