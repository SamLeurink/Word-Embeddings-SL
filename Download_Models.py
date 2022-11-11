import gensim
import gensim.models
import gensim.downloader
from scipy.spatial import distance

#Loading the google news model from gensim.
google_news_vectors = gensim.downloader.load('word2vec-google-news-300')

#Open the text file containing the NRC-VAD lexicon.
with open("NRC-VAD-Lexicon.txt") as f:
    contents = f.readlines()

#Create a list containing only the first words in the text file.
thelist = []
for x in contents:
    pl = x.split()
    thelist.append(pl[0])

#Creating a list of the seed words for convenience
seedlist = ['good','bad','active','passive', 'dominant', 'submissive']

#Creating a csv file containing the cosine similarities between the words in thelist and the seedwords, if a specific word doesn't return a value it will
#return an empty line.
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

#Similar to cosine similarity, this creates a csv file containing the euclidean distances between the words in thelist and the seedwords. 
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
