#########################################################
## function that search the date into the doc (tagged) ##
#########################################################

# amélioration: regarder à quelle point le premier mot date est le bon dans notre recherche
# -> si ok, pas besoin d'utiliser le level 2 sur le dictionnaire de mots

library(zoo)

search_date <- function(doc_tagged, dic){
  
  # indices of the word "date" in the vector containing the words
  inds_date <- which(doc_tagged$POSwords %in% dic$date$level1)
  
  # test if the word date is in the text
  if(length(inds_date) != 0){
    
    # we keep the 5 first occurences of the word "date": there is a big probability that
    # the date we are looking for is near one of those 5 first words.
    inds_date <- inds_date[1:min(length(inds_date),4)]
    
    # index of the date we want in the whole text:
    ind_date_txt <- NA
    
    # test if the date really refer to the date of the event based on the 2nd level words
    for(ind in inds_date){
      
      # test if a second level word is close:
      if(any(doc_tagged$POSwords[max(ind-1,1):min((ind+3), length(doc_tagged$POSwords))] %in% dic$date$level2)){
        ind_date_txt <- ind
        
        break
      }
    }
    
    # if the above test on the second level words failed, we take the first occurence of the word "date"
    if(is.na(ind_date_txt)){
      ind_date_txt <- inds_date[1]
    } 
    
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
    sub_words <- doc_tagged$POSwords[(ind_date_txt+1) : (ind_date_txt + 12)]
    sub_tags <- doc_tagged$POStags[(ind_date_txt+1) : (ind_date_txt + 12)]
    
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
