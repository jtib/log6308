import gensim
import numpy as np

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
locations = [it[0] for it in items]
dic = {}
for (k,v) in content:
    if k in dic:
        dic[k].append(v)
    else:
        dic[k] = [v]

vec = []
for (key,val) in dic.items():
    vec.append(val)


# Window size
w = max([len(x) for x in dic.items()])

# Model (inc. training (CBOW))
model = gensim.models.Word2Vec(vec, window=w, min_count=1, size=16, batch_words=1000)

# Locations
# Some places are never visited --> don't appear in the vocabulary
#locs = [model.wv[l] for l in locations]

# Users
users = [model.wv[u] for u in vec]

# Test (article)
vec_test = [['0', '1', '2', '3'], ['0', '4', '2', '3', '5', '6'], ['3', '7', '5', '6']]
model_test = gensim.models.Word2Vec(vec, window=6, min_count=1, size=2, batch_words=1)
