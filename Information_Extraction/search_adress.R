############################################################
## function that search the adress into the doc (tagged) ##
############################################################

#--- Libraries
require(RgoogleMaps)

search_adress <- function(doc_tagged, dic){
  
  ###################################################
  # search where the information is in the document #
  ###################################################
  
  # look for the indexes of the words in the tagged document for each word in the diccionary
  inds_adress <- list()
  
  for(i in 1:length(dic$adress$level1)){
    # transform the word in the dic (string) into a vector
    word <- unlist(strsplit(dic$adress$level1[[i]], split = " "))
    
    inds_tmp <- v_in_v(word, doc_tagged$POSwords)
    inds_adress[[i]] <- inds_tmp
  }
  
  # test if the adress has been found
  if(all(is.na(inds_adress))){
    return(list(adress = NA, latitude = NA, longitude = NA))
  }
  
  # compute the minimum of the occurence and the word associated
  ind_adress <- min(unlist(inds_adress), na.rm = T) 
  ind_word_dic <- NA
  
  # find the length of the word associated (l_word_dic) to the minimum index
    for(j in 1:length(inds_adress)){
      # test if the word is in the text
      if(! is.na(inds_adress[[j]] [1])){
        # if it is, we test if the index of each occurence is smaller than the one we have
        for(ind in inds_adress[[j]]){
          if(ind == ind_adress){
            ind_adress <- ind
            ind_word_dic <- j
          }
        }
      }
    }
    l_word_dic <- length(unlist(strsplit(dic$adress$level1[[ind_word_dic]], split = " ")))
    
    ######################
    # extract the adress #
    ######################
    
    # démarche: prendre n mots après l'indice trouvé (n à déterminer avec des tests) 
    ind_start <- ind_adress + l_word_dic
    if(doc_tagged$POSwords[ind_start] == ":"){
      ind_start <- ind_start + 1
    }
    
    n_words <- c(8,7,6,5,4,9,10)
    dec <- 0:5
    lat_long <- c(NA,NA)
    
    for(n in n_words){
      for(d in dec){
        # entire adress and tags
        adress <- doc_tagged$POSwords[(ind_start+d):(ind_start+d+n)]
        tags <- doc_tagged$POStags[(ind_start+d):(ind_start+d+n)]
        
        # test if the adress refers to multiple adresses
        bool <- F
        for(i in 1:length(dic$adress$level2)){
          word <- unlist(strsplit(dic$adress$level2[[i]], split = " "))
          if(!is.na(v_in_v(word, doc_tagged$POSwords[(ind_start+d):(ind_start+d+n)])) ){
            bool <- T
          }
        }
        
        if(bool){
          return(list(adress = paste(adress, collapse = " "), latitude = "various", longitude = "various"))
        }else{
          # latitude / longitude
          lat_long <- getGeoCode(paste(adress, collapse = " "))
          
          # if we found the latitude and longitude, we return it
          if( ! is.na(lat_long[1])){
            adress <- paste(adress, collapse = " ")
            return(list(adress = adress, latitude = lat_long[1], longitude = lat_long[2]))
          }
          
        }
        
        
      }
    }
}
