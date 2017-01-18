setwd(dir = "")

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

##############################################
## 2. Create a corpus of text form PDF docs ##
##############################################

# import pdf file names
files1 <- list.files(pattern = "pdf$")
files2 <- list.files(pattern = "PDF$")
files <- c(files1, files2)

Rpdf <- readPDF(control = list(text = "-layout"))

# tranform them to text
costco <- Corpus(URISource(files), 
                   readerControl = list(reader = Rpdf))

#############################
## 3. Exploring the corpus ##
#############################

class(costco) # "VCorpus" "Corpus"
str(costco) # list (of 6 text here) 
summary(costco)

class(costco[[1]]) # "PlainTextDocument" "TextDocument" 
str(costco[[1]]) # list (of the 'meta' and the 'content')

class(costco[[1]]$content)
str(costco[[1]]$content) # char vector
costco[[1]]$content[1:10]

str(costco[[1]]$meta) # list
costco[[1]]$meta$author

# export the file as a txt file
writeLines(costco[[1]]$content, paste("G:/seminaire_AXA/Fiches_Sinistres_mex/Costco_text/", files[1], ".txt", sep = ""))

# visualisation
viewDocs <- function(d, n) {d[[n]]$content[1:100]}
viewDocs(costco, 1)

#############################
## 4. Preparing the corpus ##
#############################

## pre processing functions ##
getTransformations() 

## remove special characters ##
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ",x))
    # rq: functions of the package tm, allow the function to be applied on a specified document type
costco <- tm_map(costco, toSpace, "/|@|\\|")
    # rq: tm_map ~ lapply
inspect(costco[1])
viewDocs(costco,1)

## conversion to lower case ##
costco <- tm_map(costco, content_transformer(tolower))
inspect(costco)
viewDocs(costco,1)

## remove numbers ##
costco <- tm_map(costco, removeNumbers) 
viewDocs(costco,1)

## remove punctuation ##
costco <- tm_map(costco, removePunctuation)
viewDocs(costco,1)

## remove stop words ##
# spanish stopwords
stopwords("spanish")[1:15]
length(stopwords("spanish"))

# remove common spanish stopwords
costco <- tm_map(costco, removeWords, stopwords("spanish"))
viewDocs(costco,1)

# remove our own stopwords
# costco <- tm_map(costco, removeWords, c(""))
    # analyse the documents more precisely for it

## strip whitespace ##
costco <- tm_map(costco, stripWhitespace)
viewDocs(costco,1)

# stemming (removes common word endings for Spanish: a, o...)
costco <- tm_map(costco, stemDocument, language = "spanish")
viewDocs(costco,1)

##############################################
## 5. Creating a Document Term Matrix (DTM) ##
##############################################

dtm <- DocumentTermMatrix(costco)
str(dtm)
class(dtm)
dtm
dim(dtm)

inspect(dtm[1:3, 3000:3010])

# rq: count (virginia university site: http://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/) 
costco.tdm <- TermDocumentMatrix(costco, control = 
                                   list(removePunctuation = TRUE,
                                        stopwords = TRUE,
                                        tolower = TRUE,
                                        stemming = TRUE,
                                        removeNumbers = TRUE,
                                        bounds = list(global = c(3, Inf))))

str(costco.tdm)
inspect(costco.tdm[1:10,])

########################
## 6. Explore the DTM ##
########################

freq <- colSums(as.matrix(dtm))
freq[1:5]

# order the frequencies from lower to bigger 
  # (return the indexes here)
ord <- order(freq)
ord[1:5]

# least frequent terms (not interesting)
freq[head(ord)]

# most frequent terms (interesting)
freq[tail(ord)]

#########################################
## 7. Distribution of Term Frequencies ##
#########################################

# number of words with 1,2... occurrences
head(table(freq),15) # 522 terms occur just once.

tail(table(freq),15) # 1 term occur 814 times

#############################################
## 8. Conversion to Matrix and save to CSV ##
#############################################

m <- as.matrix(dtm)
dim(m) # 6 files, 3166 words

# to csv: if needed in another software

##############################
## 9. Removing Sparse Terms ##
##############################

# "sparse terms" = infrequent terms
dim(dtm)

dtms <- removeSparseTerms(dtm,0.35) # possibilité de procéder par itérations pour garder un certain nombre de mots
dim(dtms)

inspect(dtms)

freq <- colSUms(as.matrix(dtms))
freq

table(head(freq,10))
