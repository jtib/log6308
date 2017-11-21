import gensim

# with dataset_ubicomp
with open('dataset_ubicomp2013/dataset_ubicomp2013_checkins.txt') as dataset:
    content = dataset.readlines()
# preformatting
content = [x.strip() for x in content]
content = [x.replace('\t', ' ') for x in content]
content = [x.split() for x in content]
# formatting
dic = {}
for (k,v) in content:
    if k in dic:
        dic[k].append(v)
    else:
        dic[k] = [v]

# Window size
w = max([len(x) for x in dic.items()])

# Model (inc. training (CBOW))
model = gensim.models.Word2Vec(dic, window=w, min_count=1, size=50)

