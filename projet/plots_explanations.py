import gensim
import numpy as np
import scipy
import time

import matplotlib.pyplot as plt

import testknn
import metrics
import small_script




content,items = small_script.load_data()


def testColdStartRatio(content,nbPoints):
    l = []
    index =np.linspace(0.01,0.99,nbPoints)
    for i in index:
        t1,t2 = small_script.fixedSplitSet(content,i)
        l.append(small_script.ratioColdStart(t1, t2))
    plt.figure(1)
    plt.plot(index,l)
    plt.title("Evolution de la proportion d'utilisateurs du test set \n sans check-in dans le training set en fonction de leur taille relative \n (0 = pas de test set, 1 = pas de train set")
    plt.show()


def testImpactOfWord2VecDimensions(trainSet,testSet,index):
    lKNI = []
    lKNN = []
    lKIU = []
    for i in index:
        print("computing for {} dimensions...".format(i))
        model, users, locations, vec, dic = small_script.getDoc2Vec(trainSet, i)
        userDistances = testknn.userCosine(users)
        usersLocDistances = testknn.userLocCosine2(users, locations)
        dKNI = testknn.KNIapproach(usersLocDistances, 10)
        dKNN = testknn.KNNapproach(usersLocDistances, userDistances, 30, 10)
        dKIU = testknn.KIUapproach(np.asarray(users), locations, userDistances, 30, 10)
        tags = metrics.getIndexFromTags(vec)
        locTags = metrics.getLocIndexFromTags(model.wv.index2word)
        lKNI.append(metrics.precisionAtK(dKNI[0], testSet, tags, locTags))
        lKNN.append(metrics.precisionAtK(dKNN[0], testSet, tags, locTags))
        lKIU.append(metrics.precisionAtK(dKIU[0], testSet, tags, locTags))
    plt.figure(2)
    plt.plot(index,lKNI,label='KNI')
    plt.plot(index,lKNN,label='KNN')
    plt.plot(index,lKIU,label='KIU')
    plt.legend()
    plt.show()
    pass



#testColdStartRatio(content,20)
#la figure 1 montre que la proportion d'usagers du testing set n'ayant aucune entrée dans le train set reste à peu près constante à moins de 5%
#pour des valeurs comprises entre 0.1 et 0.6. On choisit de négliger ces usagers; les performances s'en trouveront légèrement dégradées mais
#vu les chiffres attendus plutôt faibles, cela ne sera pas très influençant.



trainSet, testSet = small_script.fixedSplitSet(content, 0.5)

indices = np.asarray([5,10,20,30,50,80,100,200,500,1000])
testImpactOfWord2VecDimensions(trainSet,testSet,indices)