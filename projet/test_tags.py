import gensim
from gensim.models.doc2vec import LabeledSentence
import numpy as np
import matplotlib.pyplot as plt
from itertools import chain

import small_script

fs_tags = small_script.items
dic = small_script.dic

# idea: represent users with the tags associated to the places they've checked into
# represent places as their tags
# if no tags, won't be placed
# TODO: count number of places visited but without tags

# Dictionary of locations/tags
dic_loc_tags = {l[0]:l[1:] for l in fs_tags}

# Users
vecu = []
for (key,val) in dic.items():
    u_tags = [dic_loc_tags[t] for t in val]
    u_tags = list(chain.from_iterable(u_tags))
    vecu.append(LabeledSentence(words=u_tags, tags=['u'+key]))
# Locations
cuve = []
for (key,val) in dic_loc_tags.items():
    cuve.append(LabeledSentence(words=val, tags=[key]))

vec = vecu + cuve

# Window size
w = max([len(e.words) for e in vec])

# Model (DBOW)
model = gensim.models.Doc2Vec(vec, window=w, min_count=4, size=16, dbow_words=0)

# Locations, users
locations = [model.docvecs[l.tags[0]] for l in cuve]
users = [model.docvecs[u.tags[0]] for u in vecu]

# Nearest neighbours
from testknn import userCosine, userLocCosine2, KNIapproach, KNNapproach, KIUapproach

userDistances = userCosine(users)
usersLocDistances = userLocCosine2(users, locations)
dKNI = KNIapproach(usersLocDistances, 5)
dKNN = KNNapproach(usersLocDistances, userDistances, 5, 10)
dKIU = KIUapproach(np.asarray(users), np.asarray(locations), userDistances, 5, 8)
