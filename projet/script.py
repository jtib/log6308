import gensim
import pickle
 
 
def save_object(obj, filename):
     with open(filename, 'wb') as output:
         pickle.dump(obj, output, pickle.HIGHEST_PROTOCOL)
 
def load_object(filename):
     with open(filename, 'rb') as handle:
         b = pickle.load(handle)
         return b
 
 # with dataset_ubicomp
with open('dataset_ubicomp2013/dataset_ubicomp2013_checkins.txt') as dataset:
    content = dataset.readlines()

content = [x.strip() for x in content]
content = [x.replace('\t', ' ') for x in content]
content = [x.split() for x in content]
print("Preformatting done")
 
# formatting (takes a looong time)

# formatting
dic = {}
for (k, v) in content:
    if k in dic:
        dic[k].append(v)
    else:
        dic[k] = [v]

#to load from file
#vec = load_object('data.pkl')

vec = []
# Model (inc. training (CBOW))
for (k,v) in dic.items():
    vec.append([k]+v)


# Window size
w = max([len(x) for x in vec])

model = gensim.models.Word2Vec(vec, window=w, min_count=1, size=100, batch_words=1000)
word_vectors = model.wv
del model
 
pass