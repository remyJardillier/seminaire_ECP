#--------------------------------------------------------------------------------#
#   Algorithme pour extraire l'information des en-têtes des fiches
#--------------------------------------------------------------------------------#

setwd(dir = "D:/Documents/Centrale/3A/seminaire_AXA/Fiches_Sinistres_mex/2_annotations")

####################################
## 1. Install the packages needed ##
####################################

# install packages needed
install.packages(c("tm", "qdap", "qdapDictionaries", "dplyr", "RColorBrewer", 
                   "ggplot2", "scales", "magrittr", "wordcloud"), dependencies = T)

# installation du package Rgraphviz (correlation plot)
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

# librariy importations
library(tm) # Framework for text mining. 
library(qdap) # Quantitative discourse analysis of transcripts. 
library(qdapDictionaries) 
library(dplyr) # Data wrangling, pipe operator %>%(). 
library(RColorBrewer) # Generate palette of colours for plots. 
library(ggplot2) # Plot word frequencies. 
library(scales) # Include commas in numbers. 
library(magrittr)
library(Rgraphviz)
library(wordcloud)

# function to see the beggining of a text
viewDocs <- function(docs, n, l) {docs[[n]]$content[1:l]}

##############################################
## 2. Create a corpus of text form PDF docs ##
##############################################

# import pdf file names
file_names_1 <- list.files(pattern = "pdf$", recursive = T)
file_names_2 <- list.files(pattern = "PDF$", recursive = T)
file_names <- c(file_names_1, file_names_2)

Rpdf <- readPDF(control = list(text = "-layout"))

# create the corpus
docs <- Corpus(URISource(file_names), 
               readerControl = list(reader = Rpdf))

# write(doc[[1]]$content, "D:/Documents/Centrale/3A/seminaire_AXA/Fiches_Sinistres_mex/BancoInvex/15.01.12 - (01) BANCO INVEX SA FIDEICOMIS NO 192 FINAL - 01563814 (ENG) .txt")

# strip White space (remove unusesefull spaces)
docs <- tm_map(docs, stripWhitespace)

viewDocs(docs,3,50)

########################
## IE, first approach ##
########################

# index of the doc:
d <- 2 # on travaille sur la deuxième fiche du corpus
viewDocs(docs,d,100)

# 1. On regarde les lignes sur lesquelles il y a des ":"
# 2. On définit ce qu'il y a à gauche comme l'attribut (nom de l'information, de la colonne du tableau)
# 3. On définit ce qu'il y a à droite comme l'information que l'on place sous l'attribut dans le tableau.

# on ne retient que 1/10 (premières lignes) de la fiche (on considère que ces lignes contiennent l'en-tête)
l_doc <- as.integer(length(docs[[d]]$content)/10) 

#-------------------------------------------------------------------------------------------------#
# POINT D'AMELIORATION:
# idée 1. Faire une étude stat sur la longueur des en-têtes
# idée 2. Déterminer une règle pour trouver la fin de l'en-tête
#-------------------------------------------------------------------------------------------------#

# tableau qui contiendra l'information (on supprimera les colonnes vides à la fin)
table_info_1 <- data.frame(matrix(nrow=2, ncol = l_doc))

# compteur sur les colonnes du tableau
count <- 1

# boucle sur les lignes de la fiche
for(i in 1:l_doc){
  lgn <- docs[[d]]$content[i]
  if(grepl(":", lgn)){
    
    ind <- gregexpr(":", lgn)[[1]][1]

    table_info_1[1,count] <- substr(lgn, 1, ind-1)
    table_info_1[2, count] <- substr(lgn, ind+1, nchar(lgn))
      
    count <- count+1
  }
}

# on ne garde que les colonnes conenat de l'information
table_info_1 <- table_info_1[,1:(count-1)]

#########################
## IE, second approach ##
#########################

# 1. Définition des attributs (nom de l'information que l'on veut extraire)
# 2. création d'un dictionnaire de mots pour chaque attributs
# 3. Recherche de ces mots dans le texte
# 4. Recherche de l'information associée à ce mot


# index of the doc:
d <- 2
viewDocs(docs,2,100)

# dictionnaire de mots
dic_words <- list(date = c("date of loss", "date and time of loss", "sinister date"), 
                  localisation = c("location of event", "place of loss", "address", "insured company", "insured"),
                  cause = c("cause of loss", "direct cause of the event", "cause of the event", "cause"),
                  couts_sinistres = c(" Claims Reserve Estimation", "claim"),
                  valeurs_biens_assures = c("total", "sub total"))

#-----------------------------------------------------------------------------------------------#
# POINT D'AMELIORATION:
# 1. définition claire des attributs
# 2. construction d'un dictionnaire de mot aussi exhaustif que possible
#         -> à la main?
#         -> algorithmiquement?
#         -> packages "qdapDictionaries" (see in "textMiningO")? "wordnet" (see in "JSS_TM_Infrastructure"?
#-----------------------------------------------------------------------------------------------#


# nombre d'attributs (nom des informations à extraire) dans le dictionnaire
len_dic_words <- length(dic_words)

# nombre de ligne dans le doc
len_doc <- as.integer(length(docs[[d]]$content)/10)

#-------------------------------------------------------------------------------------------------#
# POINT D'AMELIORATION (même problème que l'appoche 1)
# idée 1. Faire une étude stat sur la longueur des en-têtes
# idée 2. Déterminer une règle pour trouver la fin de l'en-tête
# idée 3. Avec cette deuxième approche, on pourrait parcourir l'ensemble du document, sans se soucier
#         de ne sélectionner que l'en-tête.
#-------------------------------------------------------------------------------------------------#

# data frame des infos à extraire
table_info_2 <- data.frame(matrix(nrow=1, ncol = len_dic_words))

names <- rep(NA, len_dic_words)
for(i in 1:len_dic_words){
  names[i] <- names(dic_words[i])
}
names(table_info_2) <- names

# boucle sur les attributs du dictionnaire
for(atr in 1:len_dic_words){
  
  # boucle sur les mots associés à l'attribut
  for(w in 1:length(dic_words[[atr]])){
    
    # compteur pour sortir de la boucle sur w (word) si l'info a été trouvé
    br <- 0
    word <- dic_words[[atr]][w]
    
    # boucle sur les lignes du texte: on cherche le mot dans la ligne
    for(l in 6:len_doc){ # 6: pour ne pas prendre en compte les en-tÃªtes
      
      # ligne correspondante
      lgn <- docs[[d]]$content[l]
      
      # test si le mot est dans la ligne
      if(grepl(word, lgn, ignore.case = T)){
        
        # indice du mot
        ind <- gregexpr(word, lgn, ignore.case = T)[[1]][1]
        
        # on met le bout de phrase qui est après le mot dans le tableau
        table_info_2[1,atr] <- substr(lgn, ind+nchar(word)+1, nchar(lgn))
        
        #-------------------------------------------------------------------------------------------------#
        # POINT D'AMELIORATION:
        # idée 1. utiliser les tags (parse tree, voir le doc "JSS_TM_Infrastructure") pour trouver 
        #         l'information liée au mot dans la phrase.
        #-------------------------------------------------------------------------------------------------#
        
        # l'info a été trouvé, on sort dela boucle
        br <- br+1
        break
      }
    }
    # si br==1, l'info a été trouvé donc on sort de la boucle sur w
    if(br==1) break
  }
}

##########################################
## Construction du dictionnaire de mots ##
##########################################

#----------------------------------------#
# packages "wordnet"

#install.packages("wordnet")
library(wordnet)

# download the WordNet here: http://wordnet.princeton.edu/

# path to the wordNet:
setDict("C:/Program Files (x86)/WordNet/2.1/dict")

# different types of filter: allow to search for terms according
# to certain criteria. Explanation here: https://sites.google.com/site/mfwallace/jawbone
getFilterTypes()

# type of filter we want to use
filter <- getTermFilter("ContainsFilter", "loss", ignoreCase = T)
terms <- getIndexTerms("NOUN", 10, filter)
sapply(terms, getLemma)

# synonyms(word, "NOUN" ou "ADJECTIVE" ou "ADVERB" ou "VERB")
synonyms("loss", "NOUN")
synonyms("loss", "ADJECTIVE")
synonyms("loss", "ADVERB")
synonyms("loss", "VERB")

synonyms("date", "NOUN")
synonyms("date", "ADJECTIVE")
synonyms("date", "ADVERB")
synonyms("date", "VERB")

###############################
## IE, approach 3: with tags ##
###############################


# usefull packages:
# install.packages("openNLP")
library(openNLP)
require("NLP")

#-------------------------------#
# fonction pour tager le texte 
# input: un document du corpus
# POS = "Part Of Speech"

tagPOS <-  function(doc, ...) {
  # content of the document as a single character
  s <- paste(doc$content, collapse = " ")
  
  # we have to convert s as a String to use the following function
  s <- as.String(s)
  
  # words annotation:
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- NLP::annotate(s, word_token_annotator, a2)
  
  # words tagging:
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  a3 <- NLP::annotate(s, pos_tag_annotator, a2)
  
  # 
  a3w <- a3[a3$type == "word"]
  POSwords <- s[a3w]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  x=list(POSwords  = POSwords, POStags = POStags, POStagged = POStagged)
}

# document sur lequel on va appliquer la fonction tagPOS
doc <- docs[[3]]

# sorties de la fonction: document 2 tagger
doc_tagged <- tagPOS(doc)

str(doc_tagged$POStagged)
head(doc_tagged$POStags)
head(doc_tagged$POSwords)

#----------------------------------------#
# Recherche de l'informaiton dans le texte

# dictionnaire de mots pour chaque attributs
dic <- list(date = list(level1 = c("date"), level2 = c("loss", "incident")),
            cause = c("hurricane", "floods", "fire", "earthquake"),
            localisation = c("location of event", "place of loss", "address", "insured company", "insured"),
            couts_sinistres = c(" Claims Reserve Estimation", "claim"),
            valeurs_biens_assures = c("total", "sub total"))

# extraction de l'information à partir du dictionnaire de mots.
# table we want to fulfill
table <- data.frame(matrix(nrow=1, ncol = length(dic)))

names <- rep(NA, length(dic))
for(i in 1:length(dic)){
  names[i] <- names(dic[i])
}
names(table) <- names

# function to extract the information
IE <- function(doc, dic){
  
  # convertion to lower case:
  doc <- content_transformer(tolower)(doc)
  
  # tag le doc avec la fonction tagPOS défini précédemment
  doc_tagged <- tagPOS(doc) 
  
  # nombre de mots dans le texte:
  len_txt <- length(doc_tagged$POSwords)
  
  # nombre d'attributs (nom des informations à extraire) dans le dictionnaire
  len_dic <- length(dic)
  
  ####################
  # date extraction: #
  ####################
  
  ## where to look for the date
  
  # indices of the word "date" in the vector containing the words
  ind_date <- which(doc_tagged$POSwords %in% dic$date$level1)[1:4]
  
  # test if the date really refer to the date of the event based on the 2nd level words
  for(ind in ind_date){
    # test if a second level word is close:
    if(length(which(doc_tagged$POSwords[(ind-2):(ind+4)] %in% dic$date$level2))){
      
      
      break
    }
  }
  
  
  # boucle sur les attributs du dictionnaire
  for(j in 1:len_dic){
    
    #
    atr <- dic
    
    # boucle sur les mots associés à l'attribut
    for(wrd in 1:length(dic[[atr]])){
      
      # indexes des mots
      ind <- rep(NA, len_txt)
      
      # test si le mot est dans la ligne
      if(grepl(word, lgn, ignore.case = T)){
        
        #-----------------#
        # TO BE CONTINUED # -> faire le dictionnaire de mot d'abord
        #-----------------#
        }
    
    }
  }
  
}

# recherche des mots du dictionnaire dans "POSwords"

# recherche de l'information associée




