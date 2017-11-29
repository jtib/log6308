import numpy as np
import time
import pickle
from sklearn.metrics.pairwise import cosine_similarity
from scipy import sparse

import small_script
from testknn import *

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
## Distances (rapide)
C_sparse = sparse.csr_matrix(C) #TODO: check if useful
u_distances = cosine_similarity(C_sparse) #between users
# Approches : KNI, KNN

sentences = np.asarray(small_script.vec)
tags = sentences[:,-1]
