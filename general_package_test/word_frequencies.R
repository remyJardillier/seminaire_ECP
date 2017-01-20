setwd(dir = "D:/Documents/Centrale/3A/seminaire_AXA/Fiches_Sinistres_mex/1_fiches anglais")

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
file_names <- list.files(recursive = T)
Rpdf <- readPDF(control = list(text = "-layout"))

# create the corpus
docs <- Corpus(URISource(file_names), 
                   readerControl = list(reader = Rpdf))

# remove special characters
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ",x))
docs <- tm_map(docs, toSpace, "/|@|\\|")

# conversion to lower case
docs <- tm_map(docs, content_transformer(tolower))

# remove numbers 
docs <- tm_map(docs, removeNumbers) 

# remove punctuation 
docs <- tm_map(docs, removePunctuation)

# remove common english stopwords
docs <- tm_map(docs, removeWords, c(stopwords("english"), "also"))

# strip whitespace 
docs <- tm_map(docs, stripWhitespace)

# stemming (removes common word endings for English: a, o...)
docs <- tm_map(docs, stemDocument, language = "english")

##############################
## 3. document term matrix
##############################

# creation of the dtm
dtm <- DocumentTermMatrix(docs)

# remove sparse terms
dtm <- removeSparseTerms(dtm, sparse = 0.2)

################
## 4. Results
################

# correlation plots
plot(dtm, 
     terms = findFreqTerms(dtm,lowfreq = 150),
     corThreshold = 0.6)

# word frequencies
wf <- colSums(as.matrix(dtm))
wf_df <- data.frame(word=names(wf), freq=wf) 

# histogramme des mots
subset(wf_df, freq>50) %>%
  ggplot(aes(word, freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

# word cloud
set.seed(142)
wordcloud(names(wf), wf, min.freq=15,colors = brewer.pal(6,"Dark2"),
          scale = c(5,0.1), rot.per = 0.2)

