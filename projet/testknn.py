import gensim
import numpy as np
import scipy
import time

import metrics
import small_script
import test_svd
import matplotlib.pyplot as plt

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



#calcule la similarité entre utilisateurs sur la base du cosinus
def userCosine(users):
    u = userLocCosine2(users,users)
    for i in range(np.shape(u)[0]):
        u[i,i] = 10
    return u

#calcule la similarité entre utilisateurs et locations sur la base du cosinus (à ne pas utiliser : userLocCosine2 est
#bien plus performant
def userLocCosine(users,locations):
    x = len(users)
    y = len(locations)
    d = np.zeros((x,y),dtype='float32')
    for i in range(x):
        for j in range(y):
            d[i,j]= scipy.spatial.distance.cosine(users[i],locations[j])
            d[j,i] = d[i,j]
    return d


#renvoie la matric des similarités entre utilisateurs et locations
def userLocCosine2(users,locations):
    return scipy.spatial.distance.cdist(users,locations, metric = 'cosine')


#renvoie le top n des recommandations de lieux pour chaque utilisateur
@timeit
def KNIapproach(distanceMatrix,recommendationSize):
    s = np.shape(distanceMatrix)
    resInd = np.zeros((s[0],recommendationSize),dtype='int32')
    resVas = np.zeros((s[0],recommendationSize))
    for u in range(s[0]):
        ind = np.argsort(distanceMatrix[u,:])[:recommendationSize]
        values = distanceMatrix[u,ind]
        resInd[u,:] = ind
        resVas[u,:] = values
    return (resInd,resVas)


#get N nearest neighbors for each neighbor
def getNNeighbors(distanceUsers, nbNeighbors):
    return KNIapproach(distanceUsers,nbNeighbors+1)[0][:,1:]

#compute the nearest neighbors, then sum the recommendations for the nearest neighbors and recommend the highest scores
@timeit
def KNNapproach(distanceUserLocationMatrix, userDistanceMatrix, neighborhoodSize, recommendationSize):
    nearestNeighborsMatrix = getNNeighbors(userDistanceMatrix,neighborhoodSize)
    res = np.zeros(np.shape(distanceUserLocationMatrix))
    resInd = np.zeros((np.shape(res)[0],recommendationSize))
    resVal = np.zeros((np.shape(res)[0],recommendationSize))
    for u in range(np.shape(res)[0]):
        neighborsscores = distanceUserLocationMatrix[nearestNeighborsMatrix[u,:],:]
        res[u,:]= np.sum(neighborsscores,axis = 0)
        ind = np.argsort(res[u,:])[:recommendationSize]
        values = res[u,ind]
        resInd[u,:] = ind
        resVal[u,:] =values
    return (resInd,resVal)


@timeit
def KIUapproach(usersMatrix, locs, userDistanceMatrix, neighborhoodSize,recommendationSize):
    nearestNeighborsMatrix = getNNeighbors(userDistanceMatrix, neighborhoodSize)
    usersRepresentation = np.zeros(np.shape(usersMatrix))
    for u in range(np.shape(usersRepresentation)[0]):
        b= nearestNeighborsMatrix[u,:]
        c = usersMatrix[b,:]
        a =np.sum(c,axis = 0)
        usersRepresentation[u,:] = a
    newDistanceMatrix = userLocCosine2(usersRepresentation,locs)
    return KNIapproach(newDistanceMatrix,recommendationSize)


def getRandomRecommendation(nbUsers,nbLocations, recommandationsSize):
    res = np.zeros((nbUsers,recommandationsSize))
    for i in range(nbUsers):
        res[i,:]=np.random.choice(nbLocations,recommandationsSize)
    return res


def main():
    content,items = small_script.load_data()

    t1,t2 = small_script.fixedSplitSet(content,0.1)
    r = small_script.ratioColdStart(t1,t2)
    t1,t2 = small_script.fixedSplitSet(content,0.2)
    r2 = small_script.ratioColdStart(t1,t2)
    t1,t2 = small_script.fixedSplitSet(content,0.3)
    r3 = small_script.ratioColdStart(t1,t2)
    t1,t2 = small_script.fixedSplitSet(content,0.4)
    r4 = small_script.ratioColdStart(t1,t2)
    t1,t2 = small_script.fixedSplitSet(content,0.5)
    r5 = small_script.ratioColdStart(t1,t2)
    #On constate que même en prenant comme ensemble de test la moitié de nos données, moins de 5% des utilisateurs se retrouvent
    #sans check-in dans l'ensemble d'entraînement. Le problème de 'cold start' sera donc marginal pour les tests.


    trainSet,testSet = small_script.fixedSplitSet(content,0.2)

    model,users,locations,vec,dic = small_script.getDoc2Vec(trainSet,100)

    lengths = np.asarray([len(v) for k,v in dic.items()])
    m=np.mean(lengths)
    s=np.std(lengths)
    med = np.median(lengths)

    print("computing user distances...")
    userDistances = userCosine(users)
    #for i in range(np.shape(userDistances)[0]):
    #    userDistances[i,i] = 10

    print("computing user/location distances...")
    usersLocDistances = userLocCosine2(users,locations)

    #KNI approach
    print("computing KNI approach...")
    dKNI = KNIapproach(usersLocDistances,10)

    #KNN approach
    print("computing KNN approach")
    dKNN = KNNapproach(usersLocDistances,userDistances,30,10)

    #KIU approach
    print("computing KIU approach")
    dKIU = KIUapproach(np.asarray(users),locations,userDistances,30,10)


    print("computing SVD approach")
    dSVD = test_svd.svdPrediction(dic,items,20,10)
    dSVD2 = test_svd.topNneighborsSvdPrediction(dic,items,20,10)
    tags = metrics.getIndexFromTags(vec)
    locTags = metrics.getLocIndexFromTags(model.wv.index2word)
    t = np.unique(dKNI[0][0,:])
    precisionAt10KNI = metrics.precisionAtK(dKNI[0],testSet,tags,locTags)
    precisionAt10KNN = metrics.precisionAtK(dKNN[0],testSet,tags,locTags)
    precisionAt10KIU = metrics.precisionAtK(dKIU[0],testSet,tags,locTags)
    precisionAt10SVD = metrics.precisionAtKSVD(dSVD,testSet)
    precisionAt10SVD2 = metrics.precisionAtKSVD(dSVD2,testSet)
    randomRes = getRandomRecommendation(2060,2804,10)
    precisionAt10Random = metrics.precisionAtK(randomRes,testSet,tags,locTags)

    #comparaison avec gensim functions
    testGensim = model.docvecs.most_similar(positive=[vec[0].tags[0]])
    t2 = KNIapproach(userDistances,11)[0]
    sentences = np.asarray(vec)
    tags = sentences[:,-1]
    testScipy = tags[t2[0,1:]]
    #les résultats sont bien les mêmes.
