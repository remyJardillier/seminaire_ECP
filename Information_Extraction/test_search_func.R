# functions needed:
setwd(dir = "D:/Documents/Centrale/3A/seminaire_AXA/dossier_robin/code/IE")

source(file = "tagPOS.R")
source(file = "search_date.R")
source(file = "search_policy_id.R")
source(file = "search_date_dummy.R")

library(tm)

# create the corpus
setwd(dir = "D:/Documents/Centrale/3A/seminaire_AXA/dossier_robin/FICHES_TRIEES")

# import pdf file names
file_names_1 <- list.files(pattern = "pdf$", recursive = T)
file_names_2 <- list.files(pattern = "PDF$", recursive = T)
file_names <- c(file_names_1, file_names_2)

# create the corpus
Rpdf <- readPDF(control = list(text = "-layout"))
docs <- Corpus(URISource(file_names), 
               readerControl = list(reader = Rpdf))

# strip White space (remove unusesefull spaces) and to lower.
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))

# diccionary
dic <- list(date = list(level1 = c("date"), level2 = c("loss", "incident", "sinister", "event")),
            
            policy_id = list( level1 = c("policy"), level2 = c("number", "no"), level3 = c("period")),
            
            cause = c("hurricane", "floods", "fire", "earthquake"),
            
            localisation = c("location of event", "place of loss", "address", "insured company", "insured"),
            
            couts_sinistres = c(" Claims Reserve Estimation", "claim"),
            
            valeurs_biens_assures = c("total", "sub total"))

# function to extract the information
IE <- function(doc, dic){
  #--------------------------------------------------#
  
  # tag le doc avec la fonction tagPOS défini précédemment
  doc_tagged <- tagPOS(doc) 
  
  #----------------------------------------------------#
  # Information extraction # 
  
  # date: 
  date <- search_date(doc_tagged, dic)
  
  # policy_id:
  policy_id <- search_policy_id(doc_tagged, dic)
  
  return(list(date = date, policy_id = policy_id))
}

# table to save the results
table_test <- data.frame(matrix(ncol = 3, nrow = length(docs)))
colnames(table_test) = c("name", "date", "policy_id")
table_test[,1] <- names(docs)

# we apply the function IE on each text of the corpus
for(k in 22:length(docs)){
  infos <- IE(docs[[k]], dic) 
  
  table_test[k,2] <- infos$date
  table_test[k,3] <- infos$policy_id
}

# k = 2: pb avec la référence: tagged as "DT"
# k = 4: pas de date dans la fiche

#↨-------------------------------------------------------------------#
# comparaison des deux fonctions "search_date", "search_date_dummy":

# table to save the results
table_test_date <- data.frame(matrix(ncol = 3, nrow = length(docs)))
colnames(table_test_date) = c("name", "date", "date_dummy")
table_test_date[,1] <- names(docs)

for(k in 1:length(docs)){
  doc_tagged <- tagPOS(docs[[k]])
  
  # date
  table_test_date[k,2] <- search_date(doc_tagged, dic)
  
  # date_dummy
  table_test_date[k,3] <- search_date_dummy(doc_tagged, dic)
}
