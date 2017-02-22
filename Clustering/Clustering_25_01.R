setwd("/Users/lucieraguet/Documents/R/Seminaire_ECP/Fiches_Sinistres")

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
library(multicore)
install.packages("multicore")

# import pdf file names
#file_name =c( "/Users/lucieraguet/Documents/R/Seminaire_ECP/Prov/BancoInvex/15.01.12 - (01) BANCO INVEX SA FIDEICOMIS NO 192 FINAL - 01563814 (ENG) .pdf",
#              "/Users/lucieraguet/Documents/R/Seminaire_ECP/Prov/CaboReal/15.06.03 - CABO REAL (DREAMS & CASA DEL MAR & GANZO IR6 [ENG] .pdf",
#              "/Users/lucieraguet/Documents/R/Seminaire_ECP/Prov/Costco/15.05.14 - COSTCO - 01562914 - FINAL [ENG] .pdf",
#              "/Users/lucieraguet/Documents/R/Seminaire_ECP/Prov/DesarrolloMarinaVallarta/15.03.13 - DESARROLLO MARINA VALLARTA 01561914  Mayan Los Cabos [ENG] .pdf",
#              "/Users/lucieraguet/Documents/R/Seminaire_ECP/Prov/FemsaComerco/15.02.09 - FEMSA COMERCO - 01623214 - ENG.pdf",
#              "/Users/lucieraguet/Documents/R/Seminaire_ECP/Prov/FondoNacional/14.10.24 - FONDO NACIONAL - IP 01671914 - 4353-IR01.pdf",
#              "/Users/lucieraguet/Documents/R/Seminaire_ECP/Prov/PuebloBonito/16.06.14 - PUEBLO BONITO - 01564314 - UPDATE REPORT.pdf",
#              "/Users/lucieraguet/Documents/R/Seminaire_ECP/Prov/VillaSolaris/15.03.23 - VILLA SOLARIS - FINAL REPORT - 01567514 (ENG).PDF")
#Rpdf = readPDF(control = list(text = "-layout"))

#######################################
#          create the corpus          #
#######################################

#toSpace = content_transformer(function(x, pattern) { return (gsub(pattern, "", x))})

## Création Corpus à partir de fichiers PDF

# import pdf file names
#file_name =c( "/Users/lucieraguet/Documents/R/Seminaire_ECP/Prov/BancoInvex/15.01.12 - (01) BANCO INVEX SA FIDEICOMIS NO 192 FINAL - 01563814 (ENG) .pdf",
#              "/Users/lucieraguet/Documents/R/Seminaire_ECP/Prov/CaboReal/15.06.03 - CABO REAL (DREAMS & CASA DEL MAR & GANZO IR6 [ENG] .pdf",
#              "/Users/lucieraguet/Documents/R/Seminaire_ECP/Prov/Costco/15.05.14 - COSTCO - 01562914 - FINAL [ENG] .pdf",
#              "/Users/lucieraguet/Documents/R/Seminaire_ECP/Prov/DesarrolloMarinaVallarta/15.03.13 - DESARROLLO MARINA VALLARTA 01561914  Mayan Los Cabos [ENG] .pdf",
#              "/Users/lucieraguet/Documents/R/Seminaire_ECP/Prov/FemsaComerco/15.02.09 - FEMSA COMERCO - 01623214 - ENG.pdf",
#              "/Users/lucieraguet/Documents/R/Seminaire_ECP/Prov/FondoNacional/14.10.24 - FONDO NACIONAL - IP 01671914 - 4353-IR01.pdf",
#              "/Users/lucieraguet/Documents/R/Seminaire_ECP/Prov/PuebloBonito/16.06.14 - PUEBLO BONITO - 01564314 - UPDATE REPORT.pdf",
#              "/Users/lucieraguet/Documents/R/Seminaire_ECP/Prov/VillaSolaris/15.03.23 - VILLA SOLARIS - FINAL REPORT - 01567514 (ENG).PDF")
#Rpdf = readPDF(control = list(text = "-layout"))

#doc = Corpus(URISource(file_name), readerControl = list(reader = Rpdf))


## Création Corpus à partir de fichiers .txt
#file_name_bis = c("/Users/lucieraguet/Documents/R/Seminaire_ECP/fiches_lucie/BancoInvex/BancoInvex.txt",
#                  "/Users/lucieraguet/Documents/R/Seminaire_ECP/fiches_lucie/CaboReal/CaboReal.txt",
#                  "/Users/lucieraguet/Documents/R/Seminaire_ECP/fiches_lucie/Costco/Costco.txt",
#                  "/Users/lucieraguet/Documents/R/Seminaire_ECP/fiches_lucie/Decameron/Decameron.txt",
#                  "/Users/lucieraguet/Documents/R/Seminaire_ECP/fiches_lucie/DesarrolloMarinaVallarta/DesarrolloMarinaVallarta.txt",
#                  "/Users/lucieraguet/Documents/R/Seminaire_ECP/fiches_lucie/FemsaComerco/FemsaComerco.txt",
#                  "/Users/lucieraguet/Documents/R/Seminaire_ECP/fiches_lucie/FondoNacional/FondoNacional.txt",
#                  "/Users/lucieraguet/Documents/R/Seminaire_ECP/fiches_lucie/PuebloBonito/PuebloBonito.txt",
#                  "/Users/lucieraguet/Documents/R/Seminaire_ECP/fiches_lucie/VillaSolaris/VillaSolaris.txt")
#
#Rdoc = readDOC(control = list(text = "-layout"))
#docbis = Corpus(URISource(file_name_bis), readerControl=Rdoc)

# Chargement du corpus
load("corpus_lucie.Rdata")

#Exemple de texte
docs[[72]]$content

#Traitement du corpus

docs = tm_map(docs, removeWords, c("the", "and", stopwords("english")))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, stemDocument, language = "english") 

#Transformation du Corpus en DocumentTermMatrix ou TermDocumentMatrix

dtm = DocumentTermMatrix(docs) #nom sur les colonnes, texte sur les lignes, fréquences dans la matrice 
inspect(dtm[1,1:7])

tdm = TermDocumentMatrix(docs) #Transposée de tdm
inspect(tdm[1,1:7])

### Pour Lucie
mat_dtm = as.matrix(dtm)
View(mat_dtm) #permet de visualiser les données
View(col.names(mat_dtm))

mat_tdm=as.matrix(tdm)
View(mat_tdm) #permet de visualiser les données
####

#sparse term
k=0.8

dtms = removeSparseTerms(dtm, k)
inspect(dtms[1:7,1:7])

### Pour Lucie
mat_dtm=as.matrix(dtms)
dim(mat_dtm)
View(mat_dtm)

### Question: est ce qu'on fait un PCA ou non? On commence par essayer sans. On utilise daisy qui est 
# plus souple que dist.
dist_dtm = daisy(mat_dtm,metric = "euclidean")

### Pour Lucie
View(as.matrix(dist_dtm))

### Hierarchical clustering
pdf("Clustering_hierarchiques_1.pdf",height=10,width = 10)
clust_h= hclust(dist_dtm, method="ward.D")
dev.off()

plot(clust_h, xlab=NULL, sub=NULL)

### Cut the tree to understand what causes the cuts
group_h=cutree(clust_h,h=750)
table(group_h)

## Looking at the text contained in group#4 we understand that the corpus creation failed in the sense
## that they are now empty. Therefore we delete them from the corpus and reiterate the hierarchical 
## clustering
group1 = which(group_h==1)
group2 = which(group_h==2)
group3 = which(group_h==3)
group4 = which(group_h==4)
write.table(names(group3),"test.txt",sep=";")

docs = docs[-as.numeric(group3)]

## Reiterate the operations
dtm = DocumentTermMatrix(docs)
dtms = removeSparseTerms(dtm, k)
mat_dtm=as.matrix(dtms)
dim(mat_dtm)
dist_dtm = daisy(mat_dtm,metric = "euclidean")

# Avec Ward.2 Critère de min de variance au sein des clusters
clust_h= hclust(dist_dtm, method="ward.D2")
plot(clust_h,labels=FALSE, xlab=NULL, sub=NULL)

inertie = sort(clust_h$height, decreasing=TRUE)
plot(inertie, type="s",xlab="Nombre de Classes")
# On remarque deux sauts d'inertie importants pour 4/5 et 7/8 clusters. On retestera les hypothèses
# avec le kmeans
group_h=cutree(clust_h,h=300)
table(group_h)
# On obtient 6 classes mais on remarque le groupe de 4 fiches sur la branche de gauche est à l'origine de 
# distinctions. On supprime donc ces éléments et on réitère les étapes.

docs = docs[-as.numeric(which(group_h==1))]
docs = docs[-as.numeric(which(group_h==3))]
docs = docs[-as.numeric(which(group_h==6))]

dtm = DocumentTermMatrix(docs)
dtms = removeSparseTerms(dtm, k)
mat_dtm=as.matrix(dtms)
dim(mat_dtm)
dist_dtm = daisy(mat_dtm,metric = "euclidean")
clust_h= hclust(dist_dtm, method="ward.D2")
pdf("Clustering_hierarchiques.pdf",height=10,width = 10)
plot(clust_h,labels=FALSE, xlab=NULL, sub=NULL, main="Clustering hiérarchique des fiches sinistres")
dev.off()   
