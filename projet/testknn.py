import gensim
import pickle
import sklearn.neighbors
import numpy as np
import scipy
import time


import small_script

users = small_script.users
locations = small_script.locations




def userCosine(users):
    return userLocCosine2(users,users)

def userLocCosine(users,locations):
    x = len(users)
    y = len(locations)
    d = np.zeros((x,y),dtype='float32')
    for i in range(x):
        for j in range(y):
            d[i,j]= scipy.spatial.distance.cosine(users[i],locations[j])
            d[j,i] = d[i,j]
    return d


def userLocCosine2(users,locations):
    return scipy.spatial.distance.cdist(users,locations)

def KNIapproach(distanceMatrix,recommendationSize):
    s = np.shape(distanceMatrix)
    resInd = np.zeros((s[1],recommendationSize))
    resVas = np.zeros((s[1],recommendationSize))
    for u in range(s[0]):
        ind = np.argpartition(distanceMatrix[u,:], -recommendationSize)[-recommendationSize:]
        values = np.argsort(distanceMatrix[ind])
        indices = ind[values]
        resInd[u,:] = values
        resVas[u,:] = indices
    return (resInd,resVas)

print("computing user distances...")
t = time.time()
d = userCosine(users)
t2 = time.time() - t
print("done in {0:.2f}s".format(t2))
print("computing user/location distances...")
d2 = userLocCosine2(users,locations)
print("computing KNI approach...")
d3 = KNIapproach(d2,5)

print("done")


print("hello")