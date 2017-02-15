#--------------------------------------------------------------------------------#
#   Algorithme pour extraire l'information des en-têtes des fiches
#--------------------------------------------------------------------------------#

# functions needed:
setwd(dir = "D:/Documents/Centrale/3A/seminaire_AXA/dossier_robin/code/IE")

source(file = "tagPOS.R")
source(file = "search_date.R")
source(file = "search_policy_id.R")
source(file = "search_claim.R")

####################################
## 1. Install the packages needed ##
####################################

# install packages needed
install.packages(c("tm", "qdap", "qdapDictionaries", "dplyr", 
                   "ggplot2", "scales", "magrittr"), dependencies = T)

# installation du package Rgraphviz (correlation plot)
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

# librariy importations
library(tm) # Framework for text mining. 
library(qdap) # Quantitative discourse analysis of transcripts. 
library(qdapDictionaries) 
library(dplyr) # Data wrangling, pipe operator %>%(). 
library(ggplot2) # Plot word frequencies. 
library(scales) # Include commas in numbers. 
library(magrittr)

# function to see the beggining of a text
viewDocs <- function(docs, n, l) {docs[[n]]$content[1:l]}

##############################################
## 2. Create a corpus of text form PDF docs ##
##############################################

# folder containing the data
setwd(dir = "D:/Documents/Centrale/3A/seminaire_AXA/Fiches_Sinistres_mex/2_annotations")

# import pdf file names
file_names_1 <- list.files(pattern = "pdf$", recursive = T)
file_names_2 <- list.files(pattern = "PDF$", recursive = T)
file_names <- c(file_names_1, file_names_2)

Rpdf <- readPDF(control = list(text = "-layout"))

# create the corpus
docs <- Corpus(URISource("VistaSerena/14.10.08 - VISTA SERENA - IP 01573514 - PRELIM CRAWFORD [ENG] .pdf"), 
               readerControl = list(reader = Rpdf))

# write(doc[[1]]$content, "D:/Documents/Centrale/3A/seminaire_AXA/Fiches_Sinistres_mex/BancoInvex/15.01.12 - (01) BANCO INVEX SA FIDEICOMIS NO 192 FINAL - 01563814 (ENG) .txt")

# strip White space (remove unusesefull spaces) and to lower.
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))

viewDocs(docs,1,50)

###############################
## 3. IE, approach with tags ##
###############################

#-------------------------------------------------------#
# document sur lequel on va appliquer la fonction tagPOS
doc <- docs[[1]]

# sorties de la fonction: document 2 tagger
doc_tagged <- tagPOS(doc)

str(doc_tagged$POStagged)
head(doc_tagged$POStags)
head(doc_tagged$POSwords)

#----------------------------------------#
# dictionnaire de mots pour chaque attributs
dic <- list(date = list(level1 = c("date"), 
                        level2 = c("loss", "incident", "sinister", "event")),
            
            policy_id = list( level1 = c("policy"), 
                              level2 = c("number", "no", "nâº", "nº"), 
                              level3 = c("period")),
            
            claim = list(level1 = c("claim","adjusted loss", "claimed amount", "loss estimate")),
            
            cause = c("hurricane", "floods", "fire", "earthquake"),
            
            localisation = c("location of event", "place of loss", "address", "insured company", "insured"),
            
            couts_sinistres = c(" Claims Reserve Estimation", "claim"),
            
            valeurs_biens_assures = c("total", "sub total"))


#------------------------------------------------------------#
# table we want to fulfill
table <- data.frame(matrix(nrow=1, ncol = length(dic)))

names <- rep(NA, length(dic))
for(i in 1:length(dic)){
  names[i] <- names(dic[i])
}
names(table) <- names

#----------------------------------------------------------------#
# function to extract the information
IE <- function(doc, dic){
  #--------------------------------------------------#
  # data processing #
  
  # strip whitespace:
  doc <- stripWhitespace(doc)
  
  # convertion to lower case:
  doc <- content_transformer(tolower)(doc)
  
  # tag le doc avec la fonction tagPOS défini précédemment
  doc_tagged <- tagPOS(doc) 
  
  #----------------------------------------------------#
  # Information extraction # 
  
  # date: 
  date <- search_date(doc_tagged)
  
  # policy_id:
  policy_id <- search_policy_id(doc_tagged)
}

