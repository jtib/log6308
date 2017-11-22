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
# preformatting
content = [x.strip() for x in content]
content = [x.replace('\t', ' ') for x in content]
content = [x.split() for x in content]
print("Preformatting done")

# formatting (takes a looong time)
i = 0
vec = []
if 0:
    while i < len(content):
        user = sorted(content)[i][0]
        v = [user]
        while i < len(content) and sorted(content)[i][0] == user:
            v.append(sorted(content)[i][1])
            i += 1
        vec.append(v)
        if i%100 == 0:
            print("Avancement : {0:.2f}".format(i/len(content)*100))
            if i % 1000 == 0:
                print("Saving : {} out of {} entries".format(i, len(content)))
                save_object(vec, "data.pkl")


vec = load_object('data.pkl')

# Window size
w = max([len(x) for x in vec])

# Model (inc. training (CBOW))
model = gensim.models.Word2Vec(vec, window=w, min_count=1, size=50)
word_vectors = model.wv
del model

pass
