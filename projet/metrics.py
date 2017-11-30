import numpy as np





def getTags(vec):
    l = [u.tags[0] for u in vec]
    return l


def getLocIndexFromTags(tags):
    d = {}
    for i in range(len(tags)):
        d[tags[i]] = i
    return d

def getIndexFromTags(vec):
    d = {}
    i = 0
    for u in vec:
        d[u[1][0][1:]] = i
        i += 1
    return d

#précision au rang k
#On ne calcule cette précision que pour les utilsateurs ayant au moins un check in dans le test set.
#tags is a dic, containing (tag, index) as entries for each user
def precisionAtK(predictionsAtK, testSet,tags,locTags):
    s = np.shape(predictionsAtK)
    correctGuess = 0
    for (u,l) in testSet:
        #try is here to prevent users that are only is test set to crash the algorithm
        #precision is set to 0 for them
        try:
            index = tags[u]
            if locTags[l] in predictionsAtK[index,:]:
                correctGuess += 1
        except:
            pass
    return (correctGuess/s[0]/s[1])


def precisionAtKSVD(predictionsAtK, testSet):
    correctGuess = 0
    for (u,l) in testSet:
        #try is here to prevent users that are only is test set to crash the algorithm
        #precision is set to 0 for them
        try:
            if l in predictionsAtK[u]:
                correctGuess += 1
        except:
            pass
    return (correctGuess/sum(map(len, predictionsAtK.values())))

def hitRate(predictionAtK,testSet,tags):
    s = np.shape(predictionsAtK)
    correctGuess = np.zeros((s[0]))
    for (u, l) in testSet:
        index = tags[u]
        if l in predictionAtK[index, :]:
            correctGuess[u] = 1
    return (np.sum(correctGuess) / s[0])


def ndcg(predictionAtK, testSet):
    s = np.shape(predictionsAtK)






#On attend des valeurs plutôt basses.
#En effet, notre base de données a 27000 entrées au total. Une précision au rang 10 donne 2060 * 10 = 20600 recommandations
#alors que l'ensemble de test fait au maximum 50% des données soit 13500 entrées environ.
#De plus, il est tout à fait possible que notre algorithme recommande des lieux déjà visités par l'utilisateur dans le training set.
#Cela n'est pas dérangeant dans l'absolu, mais pénalise ici notre score de précision.
#Notons que ces limites sont également présentes dans l'article original.