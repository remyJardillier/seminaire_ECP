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
  
  # words
  a3w <- a3[a3$type == "word"]
  POSwords <- s[a3w]
  
  # tags
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  # POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  
  # list we return
  x=list(POSwords  = POSwords, POStags = POStags) #, POStagged = POStagged)
}
