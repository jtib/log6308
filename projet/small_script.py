import gensim

# with dataset_ubicomp
with open('dataset_ubicomp2013/dataset_ubicomp2013_checkins.txt') as dataset:
    content = dataset.readlines()
# preformatting
content = [x.strip() for x in content]
content = [x.replace('\t', ' ') for x in content]
content = [x.split() for x in content]
# formatting (takes a looong time)
i = 0
vec = []
while i < len(content):
    user = sorted(content)[i][0]
    v = [user]
    while i < len(content) and sorted(content)[i][0] == user:
        v.append(sorted(content)[i][1])
        i += 1
    vec.append(v)

# Window size
w = max([len(x) for x in vec])

# Model (inc. training (CBOW))
model = gensim.models.Word2Vec(vec, window=w, min_count=1, size=50)

