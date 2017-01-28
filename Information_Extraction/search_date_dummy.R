#########################################################
## function that search the date into the doc (tagged) ##
#########################################################

# prend la première occurence du mot "date" dans le texte et extrait l'information associée.

library(zoo)

search_date_dummy <- function(doc_tagged, dic){
  
  # indices of the word "date" in the vector containing the words
  ind_date <- match(dic$date$level1, doc_tagged$POSwords )
  
  # test if the word date is in the text
  if(!is.na(ind_date)){
    
    # date format
    date_tag_formats <- list(c("NN", "CD", ",", "CD"),         # september 14, 2014
                             c("JJ", "CD", ",", "CD"),
                             c("NN", "JJ", ",", "CD"),         # september 14th, 2014
                             
                             c("JJ", "NN", "CD"),              # 14th september 2014
                             c("CD", "NN", "CD"),              # 14 september 2014
                             
                             c("NN", "CD", "CD"),              # septembre 14 2014
                             c("NN", "JJ", "CD"),              # september 14th 2014
                             
                             c("CD", "NN", "CD", "NN", "CD"),  # 14 / 09 / 2014
                             c("CD", ":", "CD", ":", "CD"),    # 14 - 09 - 2014
                             c("CD", "JJ", "NN", "NN", "CD"),  # 14 / sept / 2014
                             
                             c("CD")                           # 14/09/2014, 14/sept/2014, 14-09-2014...
    )
    
    # sub-sequences in which to search the date 
    sub_words <- doc_tagged$POSwords[(ind_date+1) : (ind_date + 12)]
    sub_tags <- doc_tagged$POStags[(ind_date+1) : (ind_date + 12)]
    
    # index of the beginning of the date
    ind_date_sub <- NA
    
    # length of the date
    l <- NA
    
    # search for the tags corresponding to each date formats
    for(i in 1:length(date_tag_formats)){
      
      # search for the sequence of date tags into the sub-sequence of tags
      ind <- which(rollapply(sub_tags, length(date_tag_formats[[i]]), identical, date_tag_formats[[i]]))
      
      # if we found the date, we save its length and its index
      if(length(ind) != 0){
        l <- length(date_tag_formats[[i]])
        ind_date_sub <- ind
        
        break
      }
    }
    ind_date_sub <- ind_date_sub[1] 
    
    # if we found the date, we return it
    if( ! is.na(l)){
      date <- paste(sub_words[ind_date_sub: (ind_date_sub + l -1)], sep = " ", collapse = " ")
      return(date)
    }else{
      # if not, we return NA
      return(NA)
    }
    
  }else{
    # if the word "date" is not in the text, return NA
    return(NA)
  }
  
  
}
