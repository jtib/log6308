import gensim
from gensim.models.doc2vec import LabeledSentence
import numpy as np
import matplotlib.pyplot as plt


#load and preformat data
def load_data(filename = 'dataset_ubicomp2013/dataset_ubicomp2013_checkins.txt', filenameTags = 'dataset_ubicomp2013/dataset_ubicomp2013_tags.txt' ):
# with dataset_ubicomp
    with open(filename) as dataset:
        content = dataset.readlines()
    with open(filenameTags, errors='ignore') as tags :
        items = tags.readlines()
        nb_items = len(items)
# preformatting
    content = [x.strip() for x in content]
    content = [x.replace('\t', ' ') for x in content]
    content = [x.split() for x in content]
    items = [x.strip() for x in items]
    items = [x.replace('\t', ' ') for x in items]
    items = [x.split() for x in items]
    items = [[x[0]] + (x[1].split(sep=',') if len(x)>1 else []) for x in items]
    return (content, items)




#fonction pour récupérer un training et un testing set random
def splitSet(content, ratio):
    totalsize = len(content)
    testSize = int(ratio*totalsize)
    trainSize = totalsize - testSize
    trainSet = np.random.choice(content,trainSize)
    return trainSet

#return training and test set, in that order.
def fixedSplitSet(content, ratio):
    totalsize = len(content)
    testSize = int(ratio * totalsize)
    trainSize = totalsize - testSize
    return (content[:trainSize],content[trainSize:])

#renvoie la proportion de check-in qui ne pourront pas être correctement devinés puisque l'utilisateur n'est présent que dans
#l'ensemble de test et pas dans celui d'entraînement.
def ratioColdStart(trainSet,testSet):
    z = []
    res = 0
    for (u,l) in  trainSet:
        z.append(u)
    for (u,l) in testSet:
        if u in z:
            res += 1
    return res/len(testSet)
#get doc2vec represnetation of content without tags
def getDoc2Vec(content,nbDim):
    # formatting
    dic = {}
    for (k,v) in content:
        if k in dic:
            dic[k].append(v)
        else:
            dic[k] = [v]
    vec = []
    for (key,val) in dic.items():
        vec.append(LabeledSentence(words=val, tags=['u'+key]))

    # Window size
    w = max([len(x) for x in dic.items()])
    # Model (inc. training (CBOW))
    model = gensim.models.Doc2Vec(vec, window=w, min_count=0, size=nbDim, dbow_words=1)
    # Locations
    # Some places are never visited --> don't appear in the vocabulary, so we take directly what's been counted
    locations = model.wv.syn0
    # Users
    users = [model.docvecs[u.tags[0]] for u in vec]
    return (model,users,locations,vec,dic)




#test of gensim with toy data
def test_gensim():
    # Test (article)
    locs_test = ['0', '1', '2', '3', '4', '5', '6', '7']
    us = ['u0', 'u1', 'u2']
    vec_test = [LabeledSentence(words=['0', '1', '2', '3'], tags=['u0']), LabeledSentence(words=['0', '4', '2', '3', '5', '6'], tags=['u1']), LabeledSentence(words=['3', '7', '5', '6'], tags=['u2'])]
    model_test = gensim.models.Doc2Vec(vec_test, window=6, min_count=1, size=2, dbow_words=1)
    users_test = [model_test.docvecs[u.tags[0]] for u in vec_test]
    loca_test = [model_test.wv[l] for l in locs_test]
    x, y = zip(*users_test)
    z, t = zip(*loca_test)
    fig, ax = plt.subplots()
    ax.scatter(*zip(*loca_test))
    for j,txt in enumerate(locs_test):
        ax.annotate(txt, (z[j],t[j]))
    ax.scatter(*zip(*users_test), marker='+')
    for i,txt in enumerate(us):
        ax.annotate(txt, (x[i],y[i]))
    plt.show()
