import gensim
import numpy as np
import scipy
import time

import matplotlib.pyplot as plt

import testknn
import metrics
import small_script


global i = 1

content,items = small_script.load_data()


def histogramOfData(content):
    # formatting
    dic = {}
    for (k, v) in content:
        if k in dic:
            dic[k].append(v)
        else:
            dic[k] = [v]
    lengths = np.asarray([len(v) for k, v in dic.items()])
    lengths2 = lengths[np.where(lengths <= 50)]
    plt.figure(i)
    i += 1
    plt.subplot(211)
    n, bins, patches = plt.hist(lengths, 200, normed=1)
    plt.subplot(212)
    n, bins, patches = plt.hist(lengths2, 200, normed=1)
    plt.draw()
    return (np.mean(lengths),np.std(lengths),np.median(lengths))



def testColdStartRatio(content,nbPoints):
    l = []
    index =np.linspace(0.01,0.99,nbPoints)
    for i in index:
        t1,t2 = small_script.fixedSplitSet(content,i)
        l.append(small_script.ratioColdStart(t1, t2))
    plt.figure(i)
    i += 1
    plt.plot(index,l)
    plt.title("Ratio d'utilisateurs du test set \n avec un moins 1 check-in dans le training set  \n (0 = pas de test set, 1 = pas de train set)")
    plt.xlabel("ratio taille test set  sur taille du data set entier")
    plt.draw()


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
        dKNN = testknn.KNNapproach(usersLocDistances, userDistances, 400, 10)
        dKIU = testknn.KIUapproach(np.asarray(users), locations, userDistances, 400, 10)
        tags = metrics.getIndexFromTags(vec)
        locTags = metrics.getLocIndexFromTags(model.wv.index2word)
        lKNI.append(metrics.precisionAtK(dKNI[0], testSet, tags, locTags))
        lKNN.append(metrics.precisionAtK(dKNN[0], testSet, tags, locTags))
        lKIU.append(metrics.precisionAtK(dKIU[0], testSet, tags, locTags))
    plt.figure(2)
    plt.xscale('log')
    plt.plot(index,lKNI,label='KNI')
    plt.plot(index,lKNN,label='KNN')
    plt.plot(index,lKIU,label='KIU')
    plt.xlabel("nb of dimensions for word2vec model")
    plt.ylabel("precision at k")
    plt.legend()
    plt.show()
    pass

def testImpactOfNeighborhoodSize(trainSet,testSet,index):
    lKNI = []
    lKNN = []
    lKIU = []
    model, users, locations, vec, dic = small_script.getDoc2Vec(trainSet, 500)
    userDistances = testknn.userCosine(users)
    usersLocDistances = testknn.userLocCosine2(users, locations)
    dKNI = testknn.KNIapproach(usersLocDistances, 10)
    for i in index:
        dKNN = testknn.KNNapproach(usersLocDistances, userDistances, i, 10)
        dKIU = testknn.KIUapproach(np.asarray(users), locations, userDistances, i, 10)
        tags = metrics.getIndexFromTags(vec)
        locTags = metrics.getLocIndexFromTags(model.wv.index2word)
        lKNI.append(metrics.precisionAtK(dKNI[0], testSet, tags, locTags))
        lKNN.append(metrics.precisionAtK(dKNN[0], testSet, tags, locTags))
        lKIU.append(metrics.precisionAtK(dKIU[0], testSet, tags, locTags))
    plt.figure(3)
    plt.plot(index,lKNI,label='KNI')
    plt.plot(index,lKNN,label='KNN')
    plt.plot(index,lKIU,label='KIU')
    plt.xlabel("evolution du nombre de voisins")
    plt.ylabel("precision au rang k")
    plt.legend()
    plt.show()
    pass


mean,std,median =histogramOfData(content)
print("mean = {0:.4f}, median = {1:.4f}, std = {2:.4f}".format(mean,median,std))
#Comme attendu, les données sont très creuses (on rappelle qu'il y a plus de 2500 lieux différents.
#la moyenne est de 13 lieux visités, mais bien plus de la moitié des utilisateurs est en dessous de cette moyenne
#(la médiane est à 8 et l'écart type à 17). On a par ailleurs un phénomène de "long tail" : peu d'utilisateurs avec beaucoup de lieux.

testColdStartRatio(content,20)
#la figure 1 montre que la proportion d'usagers du testing set n'ayant aucune entrée dans le train set reste à peu près constante à moins de 5%
#pour des valeurs comprises entre 0.1 et 0.6. On choisit de négliger ces usagers; les performances s'en trouveront légèrement dégradées mais
#vu les chiffres attendus plutôt faibles, cela ne sera pas très influençant.

plt.show()

trainSet, testSet = small_script.fixedSplitSet(content, 0.4)

indices = np.asarray([5,10,20,30,50,80,100,200,500,1000,1500,2000,3500,5000])
testImpactOfWord2VecDimensions(trainSet,testSet,indices)
#La figure deux montre l'évolution de la précision au rang 10 pour les trois modèles de l'article (KNN, KNI, KIU) en fonction de la dimension
#de l'espace dans le lequel on effectue le word embedding. On note que l'approche KNI  semble bénéficier des hautes dimensions davantage que les autres.
#On retrouve le caractère décrit dans l'article : de plus hautes dimensions mènent à un meilleur résultat.
#Cependant ici le phénomène est bien plus marqué (l'article parle d'une limite asymptotique de 50 dimensions, ici l'augmentation est visible au delà de 5000 dimensions.
#Ce nombre de 5000 semble plus en accord avec la littérature que 50; cependant l'auteur n'ayant pas détaillé son utilisation de doc2vec en détail,
#on ne peut être sûr que nous manipulions exactement le même algorithme : l'article date de 2016 et parle de temps d'entraînement de plus de 20 minutes
#quand notre plus gros modèle tourne en moins d'une minute sur un laptop de 2013...
#De plus il nous faut rappeler que le dataset n'est pas le même.



index = np.asarray([2,3,5,10,15,20,25,30,40,50,100,150,200,250,300,400,1000])
testImpactOfNeighborhoodSize(trainSet,testSet,index)
#La figure montre l'évolution de la précision au rang 10 en fonction de la taille du voisinage utilisé dans l'algorithme (l'approche KNI,
#qui n'utilise pas de voisinage, est là en tant que baseline. On constate que le nombre de voisin optimale semble se situer aux alentours
#de 400, ce qui semble très élevé au regard des 2000 utilisateurs que compte notre base. Cependant le fait que la courbe décroit dans
#les valeurs élevées semble indiquer qu'il ne faut tout de même pas "trop" généraliser; reommander les lieux les plus fréquentés à tout le
#monde ne semble pas être la meilleure solution, ce qui est bon signe : le profil de l"utilisateur importe.












