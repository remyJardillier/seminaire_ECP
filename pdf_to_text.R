setwd(dir = "D:/Documents/Centrale/3A/seminaire_AXA/fiches_lucie")

# import pdf file names
file_names1 <- list.files(pattern = "PDF$", recursive = T)
file_names2 <- list.files(pattern = "pdf$", recursive = T)
file_names <- c(file_names1, file_names2)
Rpdf <- readPDF(control = list(text = "-layout"))

# create the corpus
docs <- Corpus(URISource(file_names), 
               readerControl = list(reader = Rpdf))

docs = tm_map(docs, removeWords, c("the", "and", stopwords("english")))
docs = tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, stemDocument, language = "english")

for(i in 1:9){
  name <- paste(substr(file_names[i],1,nchar(file_names[i])-4), ".txt", sep="")
  write(docs[[i]]$content, paste("D:/Documents/Centrale/3A/seminaire_AXA/fiches_lucie/", name, sep=""))
}
# write(doc[[1]]$content, "D:/Documents/Centrale/3A/seminaire_AXA/Fiches_Sinistres_mex/BancoInvex/15.01.12 - (01) BANCO INVEX SA FIDEICOMIS NO 192 FINAL - 01563814 (ENG) .txt")
