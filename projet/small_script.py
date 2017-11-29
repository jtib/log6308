import gensim
from gensim.models.doc2vec import LabeledSentence
import numpy as np
import matplotlib.pyplot as plt

# with dataset_ubicomp
with open('dataset_ubicomp2013/dataset_ubicomp2013_checkins.txt') as dataset:
    content = dataset.readlines()
with open('dataset_ubicomp2013/dataset_ubicomp2013_tags.txt', errors='ignore') as tags :
    items = tags.readlines()
    nb_items = len(items)

# preformatting
content = [x.strip() for x in content]
content = [x.replace('\t', ' ') for x in content]
content = [x.split() for x in content]
items = [x.strip() for x in items]
items = [x.replace('\t', ' ') for x in items]
items = [x.split() for x in items]

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
model = gensim.models.Doc2Vec(vec, window=w, min_count=1, size=16, dbow_words=1)

# Locations
# Some places are never visited --> don't appear in the vocabulary, so we take directly what's been counted
locations = model.wv.syn0

# Users
users = [model.docvecs[u.tags[0]] for u in vec]




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
