import numpy as np
import time
from itertools import product, chain
from collections import Counter
from sklearn.metrics.pairwise import cosine_similarity

import small_script


#decorateur pour avoir les performances en temps
def timeit(method):
    def timed(*args, **kw):
        ts = time.time()
        result = method(*args, **kw)
        te = time.time()
        if 'log_time' in kw:
            name = kw.get('log_name', method.__name__.upper())
            kw['log_time'][name] = int((te - ts) * 1000)
        else:
            print('%r  %2.2f ms' % \
                  (method.__name__, (te - ts) * 1000))
        return result
    return timed



@timeit
def svdPrediction(dic,items,nbDim,nbRecommandation):

    locations = [it[0] for it in items]
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
    s_16 = s[:nbDim]
    U_16 = U[:,:nbDim]
    V_16 = V[:nbDim,:nbDim]
    donnees_16 = np.dot((np.dot(U_16, np.diag(s_16))), V_16)

    # Final matrix
    ## Denormalizing
    C = donnees_16 + u_average
    # 5 best items (indexes)
    mSVD_pred = (np.argsort(C))[:,:nbRecommandation]
    # Corresponding places
    dSVD_pred = {}
    for i,j in product(range(mSVD_pred.shape[0]), range(mSVD_pred.shape[1])):
        u = dic_users[i]
        if u in dSVD_pred:
            dSVD_pred[u].append(dic_items[mSVD_pred[i][j]])
        else:
            dSVD_pred[u] = [dic_items[mSVD_pred[i][j]]]

    return dSVD_pred



# Other approach : top n neighbours
## Distances (cosinus)
@timeit
def topNneighborsSvdPrediction(dic,items,nbDim,nbRecommandation):
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
    s_16 = s[:nbDim]
    U_16 = U[:,:nbDim]
    V_16 = V[:16,:nbDim]
    donnees_16 = np.dot((np.dot(U_16, np.diag(s_16))), V_16)
    Us = np.dot(U_16, np.diag(np.sqrt(s_16)))
    u_sim = cosine_similarity(Us) #between users
    # 5 closest neighbors (users)
    neighbours = np.argsort(u_sim)[:,:5]
    # 5 places in which they've (collectively) checked in most frequently
    # for 1 index in one row:
    names = [[dic_users[ne] for ne in users] for users in neighbours]
    locations = [list(chain.from_iterable([dic[uname] for uname in n])) for n in names]
    rec = [Counter(loc).most_common(5) for loc in locations]
    dic_rec = {dic_users[i]:rec[i] for i in range(len(rec))}
    return dic_rec
