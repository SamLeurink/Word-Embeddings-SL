import gensim
import gensim.models
import gensim.downloader
from scipy.spatial import distance

google_news_vectors = gensim.downloader.load('word2vec-google-news-300')

with open(r"C:\Users\samle\Documents\NRC-VAD-Lexicon-Aug2018Release\NRC-VAD-Lexicon-Aug2018Release\NRC-VAD-Lexicon.txt") as f:
    contents = f.readlines()

thelist = []
for x in contents:
    pl = x.split()
    thelist.append(pl[0])

seedlist = ['good','bad','active','passive', 'dominant', 'submissive']
with open('resultscs.csv', 'w') as f:
    writer = csv.writer(f)
    writer.writerow(['name','good','bad', 'active', 'passive', 'dominant', 'submissive'])
    for x in thelist:
        therow = []
        therow.append(x)
        for y in seedlist:
            try:
                therow.append(google_news_vectors.similarity(x,y))
            except:
                pass
        writer.writerow(therow)

with open('resultsed.csv', 'w') as f:
    writer = csv.writer(f)
    writer.writerow(['name','good','bad', 'active', 'passive', 'dominant', 'submissive'])
    for x in thelist:
        therow = []
        therow.append(x)
        for y in seedlist:
            try:
                therow.append(distance.euclidean(google_news_vectors[x],google_news_vectors[y]))
            except:
                pass
        writer.writerow(therow)
