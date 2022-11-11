#install the packages needed
install.packages("dplyr")
install.packages("stringr")
install.packages("udpipe")
install.packages("flextable")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("broom")

#all the libraries used
library(ggplot2)
library(tidyverse)
library(broom)
library(dplyr)
library(stringr)
library(udpipe)
library(flextable)

#Read the files created in python and the NRC file
datacossim <- read.csv("resultscs.csv")
dataeucdis <- read.csv("resultsed.csv")
dataNRC <- read.csv("NRC-VAD-Lexicon.txt", sep = "")

#Add column names to the NRC dataframe for format
colnames(dataNRC) <- c("names","v_NRC","a_NRC","d_NRC") 

#Filtering out the phrases that didn't recieve a score from Word2Vec
datacossim2 <- datacossim %>%
  filter(!is.na(good))

dataeucdis2 <- dataeucdis %>%
  filter(!is.na(good))

#Creating a new dataframe where the sentiment scores are calculated as described in the paper
datalikcossim <- datacossim2 %>%
  group_by(name) %>%
  summarize(V = (good - bad) / (1 - 0.7190051), A = (active - passive) / (1 - 0.43619528) , D = (dominant - submissive) / (0.27168104) )

datalikeucdis <- dataeucdis2 %>%
  group_by(name) %>%
  summarize(V = (bad - good) / (1.802906), A = (passive - active) / (2.900496) , D = (submissive - dominant) / (3.821561) )

#Normalizing the results to be from 0 to 1
datalikerttot <- datalikcossim %>%
  group_by(name) %>%
  summarize(v = (V + 1) / 2, a = (A + 1) / 2, d = (D + 1) / 2)

datalikeuctot <- datalikeucdis %>%
  group_by(name) %>%
  summarize(v = (V + 1) / 2, a = (A + 1) / 2, d = (D + 1) / 2)

#Adding the NRC scores to the dataframe and filtering out the duplicated words and words that don't have a score
datatota <- merge(datalikerttot, dataNRC, by.x = "name", by.y = "names")
datatotaed <- merge(datalikeuctot, dataNRC, by.x = "name", by.y = "names")

datatotaed <- datatotaed %>%
  filter(!(is.na(as.numeric(v_NRC))))

datatota <- datatota %>%
  filter(!(is.na(as.numeric(v_NRC))))

datatota <- datatota[!duplicated(datatota),]
datatotaed <- datatotaed[!duplicated(datatotaed),]

#Correlations and plots for Cosine Similarity (CS)
datatota$v_NRC <- as.numeric(datatota$v_NRC)
cossimplotV <- ggplot(data = datatota, aes(y = v, x = v_NRC)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ylim(0,1.2) +
  theme_bw()

datatota$a_NRC <- as.numeric(datatota$a_NRC)
cossimplotA <- ggplot(data = datatota, aes(y = a, x = a_NRC)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

datatota$d_NRC <- as.numeric(datatota$d_NRC)
cossimplotD <- ggplot(data = datatota, aes(y = d, x = d_NRC)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

cossimspearmanV <- cor.test(datatota$v, datatota$v_NRC, method = "spearman", exact = FALSE)
cossimspearmanA <- cor.test(datatota$a, datatota$a_NRC, method = "spearman", exact = FALSE)
cossimspearmanD <- cor.test(datatota$d, datatota$d_NRC, method = "spearman", exact = FALSE)

#Correlations and plots for Euclidean Distance (ED)
datatotaed$v_NRC <- as.numeric(datatotaed$v_NRC)
eucdisplotV <- ggplot(data = datatotaed, aes(y = v, x = v_NRC)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ylim(0,1.2) +
  theme_bw()

datatotaed$a_NRC <- as.numeric(datatotaed$a_NRC)
eucdisplotA <- ggplot(data = datatotaed, aes(y = a, x = a_NRC)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

datatotaed$d_NRC <- as.numeric(datatotaed$d_NRC)
eucdisplotD <- ggplot(data = datatotaed, aes(y = d, x = d_NRC)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

eucdisspearmanV <- cor.test(datatotaed$v, datatotaed$v_NRC, method = "spearman", exact = FALSE)
eucdisspearmanA <- cor.test(datatotaed$a, datatotaed$a_NRC, method = "spearman", exact = FALSE)
eucdisspearmanD <- cor.test(datatotaed$d, datatotaed$d_NRC, method = "spearman", exact = FALSE)

#Creating the POS-Tagger by downloading the english model from udpipe
m_eng   <- udpipe::udpipe_download_model(language = "english-ewt")
ud_model <- udpipe_load_model(m_eng$file_model)
engword <- as.data.frame(udpipe_annotate(ud_model, x = datatota$name))

#Creating the Adjectives dataframes for Cosine Similarity and Euclidean Distance
datatotapos <- merge(datatota, engword, by.x = "name", by.y = "token")
datatotaedpos <- merge(datatotaed, engword, by.x = "name", by.y = "token")

datatotapos <- datatotapos %>%
  filter(upos == "ADJ")

datatotaedpos <- datatotaedpos %>%
  filter(upos == "ADJ")

#Correlations and plots for Cosine Similarity Adjectives (CSA)
cossimplotVadj <- ggplot(data = datatotapos, aes(y = v, x = v_NRC)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ylim(0,1.2) +
  theme_bw()

cossimplotAadj <- ggplot(data = datatotapos, aes(y = a, x = a_NRC)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

cossimplotDadj <- ggplot(data = datatotapos, aes(y = d, x = d_NRC)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

cossimspearmanVadj <- cor.test(datatotapos$v, datatotapos$v_NRC, method = "spearman", exact = FALSE)
cossimspearmanAadj <- cor.test(datatotapos$a, datatotapos$a_NRC, method = "spearman", exact = FALSE)
cossimspearmanDadj <- cor.test(datatotapos$d, datatotapos$d_NRC, method = "spearman", exact = FALSE)

#Correlations and plots for Euclidean Distance Adjectives (EDA)
eucdisplotVadj <- ggplot(data = datatotaedpos, aes(y = v, x = v_NRC)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ylim(0,1.2) +
  theme_bw()

eucdisplotAadj <- ggplot(data = datatotaedpos, aes(y = a, x = a_NRC)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

eucdisplotDadj <- ggplot(data = datatotaedpos, aes(y = d, x = d_NRC)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

eucdisspearmanVadj <- cor.test(datatotaedpos$v, datatotaedpos$v_NRC, method = "spearman", exact = FALSE)
eucdisspearmanAadj <- cor.test(datatotaedpos$a, datatotaedpos$a_NRC, method = "spearman", exact = FALSE)
eucdisspearmanDadj <- cor.test(datatotaedpos$d, datatotaedpos$d_NRC, method = "spearman", exact = FALSE)

#Calculating the regression lines for:
#Valence
lm(datatota$v ~ datatota$v_NRC)
lm(datatotaed$v ~ datatotaed$v_NRC)
lm(datatotaedpos$v ~ datatotaedpos$v_NRC)
lm(datatotapos$v ~ datatotapos$v_NRC)
valplot <- ggplot()

#Arousal
lm(datatota$a ~ datatota$a_NRC)
lm(datatotaed$a ~ datatotaed$a_NRC)
lm(datatotaedpos$a ~ datatotaedpos$a_NRC)
lm(datatotapos$a ~ datatotapos$a_NRC)

#Dominance
lm(datatota$d ~ datatota$d_NRC)
lm(datatotaed$d ~ datatotaed$d_NRC)
lm(datatotaedpos$d ~ datatotaedpos$d_NRC)
lm(datatotapos$d ~ datatotapos$d_NRC)
