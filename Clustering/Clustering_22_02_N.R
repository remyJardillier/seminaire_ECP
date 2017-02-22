setwd("/Users/lucieraguet/Documents/R/Seminaire_ECP/Clustering_22_02")
rm(list=ls())
# library importations

library(tm) # Framework for text mining. 
library(RColorBrewer) # Generate palette of colours for plots. 
library(qdapDictionaries) 
library(qdap) # Quantitative discourse analysis of transcripts. 
library(dplyr) # Data wrangling, pipe operator %>%(). 
library(ggplot2) # Plot word frequencies. 
library(scales) # Include commas in numbers. 
library(magrittr)
library(Rgraphviz)
library(wordcloud)
library(lsa)
library(parallel)
require(rJava) # needed for stemming function 
require(SnowballC)
library(dplyr)
library(proxy)
library(stats) # hierarchical clustering and kmeans
library(cluster) # to plot kmeans
# Voir si indispensable, pas dispo pour cette version de R
#library(multicore)
#install.packages("multicore")

# Chargement du corpus
load("corpus_lucie_propre.Rdata")

#Exemple de texte
docs[[30]]$content

n_text = length(docs)
#Traitement du corpus

docs = tm_map(docs, removeWords, c("the", "and", stopwords("english")))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, stemDocument, language = "english") 

#Transformation du Corpus en DocumentTermMatrix 

dtm = DocumentTermMatrix(docs) #nom sur les colonnes, texte sur les lignes, fréquences dans la matrice 
inspect(dtm[1,1:7])

mat_dtm = as.matrix(dtm) ## Noms sur les colonnes, textes sur les lignes
View(mat_dtm)
#sparse term
k=0.8

dtms = removeSparseTerms(dtm, k) ##Supprime tous les termes de la matrice dont la fréquence de vide est supérieure à k
inspect(dtms[1:7,1:7])

mat_dtm=as.matrix(dtms)

## On ajoute une étape intermédiaire de normalisation des données. On veut que les fréquences des mots soient normalisées
## au sein des textes pour que la longueur des textes n'influe pas sur le clustering.

mat_dtm = t(scale(t(mat_dtm)))

dist_dtm = daisy(mat_dtm,metric = "euclidean")

##### Hierarchical clustering #####

clust_h= hclust(dist_dtm, method="ward.D")

plot(clust_h, xlab=NULL, sub=NULL, labels = FALSE, main = "Dendogramme pour données normalisées")

group_h=cutree(clust_h,k=2)

## Un exemple
table(group_h)
row.names(mat_dtm[which(group_h==1),])

inertie = sort(clust_h$height, decreasing=TRUE)
plot(inertie, type= "l", main="Inertie en fonction du nombre de clusters", xlab="Nombre de clusters")

## On observe deux pertes d'inertie pour 2 et 4/5 clusters. On part donc sur 5 clusters pour appliquer les kmeans
n_clust=5

group2_h = cutree(clust_h, k=n_clust)
table(group2_h)

##### Kmeans #####

mat_dist_dtm = as.matrix(dist_dtm)
res = kmeans(dist_dtm,centers = n_clust, nstart = 10)
res2 = kmeans(mat_dtm, centers =n_clust, nstart = 10)

prov = matrix(0,n_text,2)
prov[,2]=as.matrix(res2$cluster)
prov[,1]=names(res2$cluster)
clust2=matrix(0,n_text,4)
clust2[,1]=as.vector(order(prov[,2]))
for (i in 1:n_text){
  clust2[i,2]=prov[as.numeric(clust2[i,1]),1]
  clust2[i,3]=prov[as.numeric(clust2[i,1]),2]
}
clust2[,1:2]= clust2[,2:3]
for (i in 1:n_text){
  clust2[i,3]=res$cluster[clust2[i,1]]
}
for (i in 1:n_clust){
  clust2[which(group2_h==i),4]=i
}
clust2

##### Analyse au sein des clusters , Hierarchical clustering #####

table(group2_h)

### Cluster 1 ###
cluster1 = as.vector(which(group2_h==1))
fiche_t1 = row.names(mat_dtm[which(group2_h==1),])
docs1= docs[cluster1]

toSpace = content_transformer(function(x, pattern) gsub(pattern, " ",x))
docs1 = tm_map(docs1, toSpace, "/|@|\\|")
docs1 = tm_map(docs1, content_transformer(tolower))
dtm1 = DocumentTermMatrix(docs1)
dtm1 = removeSparseTerms(dtm1, sparse = 0.2)

plot(dtm1, terms = findFreqTerms(dtm1,lowfreq = 150), corThreshold = 0.6, main = "Graphe de corrélation, cluster 1")


wf1 = colSums(as.matrix(dtm1))
wf_df1 = data.frame(word=names(wf1), freq=wf1) 
term_freq1 = findFreqTerms(dtm1,lowfreq = 100)

subset(wf_df1, freq>100) %>%
  ggplot(aes(word, freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

### Cluster 2 ###
cluster2 = as.vector(which(group2_h==2))
fiche_t2 = row.names(mat_dtm[which(group2_h==2),])
docs2= docs[cluster2]

docs2 = tm_map(docs2, toSpace, "/|@|\\|")
docs2 = tm_map(docs2, content_transformer(tolower))
dtm2 = DocumentTermMatrix(docs2)
dtm2 = removeSparseTerms(dtm2, sparse = 0.2)

plot(dtm2, terms = findFreqTerms(dtm2,lowfreq = 150), corThreshold = 0.6)

wf2 = colSums(as.matrix(dtm2))
wf_df2 = data.frame(word=names(wf2), freq=wf2) 
term_freq2 = findFreqTerms(dtm2,lowfreq = 100)

subset(wf_df2, freq>100) %>%
  ggplot(aes(word, freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

### Cluster 3 ###

cluster3 = as.vector(which(group2_h==3))
fiche_t3 = row.names(mat_dtm[which(group2_h==3),])
docs3= docs[cluster3]

docs3 = tm_map(docs3, toSpace, "/|@|\\|")
docs3 = tm_map(docs3, content_transformer(tolower))
dtm3 = DocumentTermMatrix(docs3)
dtm3 = removeSparseTerms(dtm3, sparse = 0.2)

plot(dtm3, terms = findFreqTerms(dtm3,lowfreq = 150), corThreshold = 0.6)

wf3 = colSums(as.matrix(dtm3))
wf_df3 = data.frame(word=names(wf3), freq=wf3) 
term_freq3 = findFreqTerms(dtm3,lowfreq = 100)

subset(wf_df3, freq>100) %>%
  ggplot(aes(word, freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

### Cluster 4 ###

cluster4 = as.vector(which(group2_h==4))
fiche_t4 = row.names(mat_dtm[which(group2_h==4),])
docs4= docs[cluster4]

docs4 = tm_map(docs4, toSpace, "/|@|\\|")
docs4 = tm_map(docs4, content_transformer(tolower))
dtm4 = DocumentTermMatrix(docs4)
dtm4 = removeSparseTerms(dtm4, sparse = 0.2)

plot(dtm4, terms = findFreqTerms(dtm4,lowfreq = 150), corThreshold = 0.6)

wf4 = colSums(as.matrix(dtm4)) 
wf_df4 = data.frame(word=names(wf4), freq=wf4)
term_freq4 = findFreqTerms(dtm4,lowfreq = 100)

subset(wf_df4, freq>100) %>%
  ggplot(aes(word, freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

### Cluster 5 ###

cluster5 = as.vector(which(group2_h==5))
fiche_t5 = row.names(mat_dtm[which(group2_h==5),])
docs5= docs[cluster5]

docs5 = tm_map(docs5, toSpace, "/|@|\\|")
docs5 = tm_map(docs5, content_transformer(tolower))
dtm5 = DocumentTermMatrix(docs5)
dtm5 = removeSparseTerms(dtm5, sparse = 0.2)

plot(dtm5, terms = findFreqTerms(dtm5,lowfreq = 150), corThreshold = 0.6)

wf5 = colSums(as.matrix(dtm5))
wf_df5 = data.frame(word=names(wf5), freq=wf5) 
term_freq5 = findFreqTerms(dtm5,lowfreq = 100)

subset(wf_df5, freq>100) %>%
  ggplot(aes(word, freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

