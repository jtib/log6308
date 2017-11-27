import numpy as np
import time
import pickle

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

# SVD decomposition
U,s,V = np.linalg.svd(donnees, full_matrices=False)

# Using 16 dimensions (same as gensim projection)
s_16 = s[:16]
U_16 = U[:,:16]
V_16 = V[:16,:]
donnees_16 = np.dot((np.dot(U_16, np.diag(s_16)), V_16))
