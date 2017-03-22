setwd("/Users/lucieraguet/Documents/R/Seminaire_ECP/Clustering_22_02")
rm(list=ls())

##### Introduction #####

# library importations

library(tm) # Framework for text mining. 
library(qdapDictionaries) 
library(qdap) # Quantitative discourse analysis of transcripts. 
library(dplyr) # Data wrangling, pipe operator %>%(). 
library(scales) # Include commas in numbers. 

# Chargement du corpus
load("corpus_lucie_propre.Rdata")

n_text = length(docs) # Nombres de textes contenus dans le corpus

#Traitement du corpus

docs = tm_map(docs, removeWords, c("the", "and", stopwords("english")))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, stemDocument, language = "english") 

#Suppression des mots de moins de 4 caractères
# content of the document as a single character
for (i in 1:n_text){
  doc = docs[[i]]
  s = paste(doc$content, collapse = " ")
  s = as.String(s)
  
  # separate each word of the text in a vector
  s = unlist(strsplit(s, split=" "))
  s_clean = rep(NA,length(s))
  count = 1
  
  for(j in 1:length(s)){
    if(nchar(s[j]) > 3 ){
      s_clean[count] = s[j]
      count = count+1
    }
  }
  # Supprimer les NA
  s_clean = s_clean[!is.na(s_clean)]
  docs[[i]]$content = s_clean  
}

##### Analyse des fréquences et extraction des 100 mots les plus fréquents #####

# On crée une document term matrix uniquement pour les mots de 4 lettres et plus
dtm = DocumentTermMatrix(docs)
dtm_mat = as.matrix(dtm)

# On s'assure que la document term matrix ne comporte pas de stopwords. Si c'est le cas, on supprime les colonnes
# correspondantes

sw = c(stopwords(kind="en"),stopwords(kind="es")) # On cherche les stopwords espagnol et anglais
sw=sw[nchar(sw)>3] #On ne garde les stopwords que de longueur 4 et plus
vect = c() #Vecteur qui va contenir les mots en communs entre la matrice et la liste de stop words
for (i in 1:dim(dtm_mat)[2]){
  if(length(which(sw==colnames(dtm_mat)[i]))==0){}else{vect=c(vect,colnames(dtm_mat)[i])}
}
ind=rep(0,length(vect)) # On détermine les indices correspondants
for (i in 1:length(vect)){
  ind[i]= which(colnames(dtm_mat)==vect[i])
}
dtm_mat = dtm_mat[,-ind] #On supprime les colonnes

# On a besoin de normaliser les fréquences par texte pour que la longueur des textes n'influe pas sur la fréquence
dtm_mat = t(scale(t(dtm_mat)))
write.csv2(colnames(dtm_mat),file = "Noms.csv")

# On veut maintenant garder les 100 fréquences les plus élevées en cumulées. On crée donc un vecteur de fréquences cumulées
dtm_mat_cs = colSums(dtm_mat)
dtm_mat_cs = order(dtm_mat_cs,decreasing=TRUE)[1:100]
dtm_mat_100=dtm_mat[,dtm_mat_cs]
mots_les_plus_frequents = colnames(dtm_mat_100)

save(mots_les_plus_frequents,file='Mots_freq.Rdata')
write.csv2(dtm_mat_100,file="Frequences_100.csv",row.names=TRUE,col.names=TRUE)

##### Analyse des corrélations #####

# On part de la matrice des fréquences calculées ci dessus et on calcule la matrice de corrélation

mat_cor = cor(dtm_mat_100)
ind_lig = which(mat_cor>0.8)%%100
ind_col = (which(mat_cor>0.8)-ind_lig)/100+1
ind_mat = matrix(NA,length(ind_lig),2)
ind_mat[,1] = ind_lig
ind_mat[,2] = ind_col
list_ind = c(dim(ind_mat)[1])
for (i in 1:dim(ind_mat)[1]){
  if (ind_mat[i,1]==ind_mat[i,2]){list_ind=c(list_ind,i)}
}
ind_mat=ind_mat[-list_ind,]
# On veut ordonner la liste
for (i in 1: dim(ind_mat)[1]){
  prov = max(ind_mat[i,1],ind_mat[i,2])
  ind_mat[i,1]=min(ind_mat[i,1],ind_mat[i,2])
  ind_mat[i,2]=prov
}
ind_mat = unique(ind_mat)
tab_corr = matrix(0,dim(ind_mat)[1],3)
for (i in 1: dim(ind_mat)[1]){
  tab_corr[i,1]=row.names(mat_cor)[ind_mat[i,1]]
  tab_corr[i,2]=colnames(mat_cor)[ind_mat[i,2]]
  tab_corr[i,3]=mat_cor[ind_mat[i,1],ind_mat[i,2]]
}

write.csv2(tab_corr,file="Corr_sup_80.csv")


##### Recherche mots comme piscine and co #####

noms  = colnames(dtm_mat)
liste = c(which(noms=="pool") , which(noms=="roof") , which(noms=="wood") , which(noms=="concr"),
        which(noms=="wall") , which(noms=="floor") , which(noms=="water"),
        which(noms=="window"))
dtm_list = dtm_mat[,liste]

write.csv2(dtm_list, file = "freq_manuelle.csv")


##### Analyse des mots les plus fréquents pour chaque texte

mat = matrix(NA,30,100)
row.names(mat)=c(row.names(dtm_mat))
for (i in 1:30){
  mat[i,]=colnames(dtm_mat)[order(dtm_mat[i,],decreasing=TRUE)[1:100]]
}
View(mat)

write.csv2(mat,"freq_100_text.csv")
