##########################################################
## function that search the claim into the doc (tagged) ##
##########################################################

# function to search the date
search_claim <- function(doc_tagged, dic){
  
  #--------------------------#
  # search where is the claim
  
  # indices of the level1 words in the dictionary
  inds_c <- which(doc_tagged$POSwords %in% dic$claim$level1[[1]])
  inds_al <- v_in_v(dic$claim$level1[[2]], doc_tagged$POSwords)
  inds_ca <- v_in_v(dic$claim$level1[[3]], doc_tagged$POSwords)
  inds_le <- v_in_v(dic$claim$level1[[4]], doc_tagged$POSwords)
  inds_el <- v_in_v(dic$claim$level1[[5]], doc_tagged$POSwords)
  
  # other option to explore:
  # ind_al <- which(rollapply(doc_tagged$POSwords, 2, identical, dic$claim$level1[[4]]))
  
  # min indexes
  min_inds <- c(c = min(inds_c),
                al = min(inds_al),
                ca = min(inds_ca),
                le = min(inds_le),
                el = min(inds_el))
  
  # index where we are going to look for the information:
  ind_min <- min(min_inds, na.rm = T)
  name = names(which(min_inds == ind_min))
  
  
  # test if it's the claim (and not the claim number or the claim reference)
  if(name == "c" ){
    if(doc_tagged$POSwords[ind_min+1] %in% c("no", "reference")){
      inds_c <- inds_c[-1]
      
      # min indexes
      min_inds <- c(c = min(inds_c),
                    al = min(inds_al),
                    ca = min(inds_ca),
                    le = min(inds_le),
                    el = min(inds_el))
      
      # index where we are going to look for the information:
      ind_min <- min(min_inds, na.rm = T)
      name = names(which(min_inds == ind_min))
    }
  }
  
  ind_start <- ind_min + length(name)
  
  #----------------------------------------------------------------------------#
  # test if the claim is in the report or not (level 2 words in the dictionary)
  
  bool <- FALSE
  for(i in 1:length(dic$claim$level2)){
    if(!is.na(v_in_v(dic$claim$level2[[i]], doc_tagged$POSwords[ind_start:(ind_start+6)])) ){
      bool <- T
    }
  }
  
  if(bool){
    return("not yet formally quantified")
    
  }else{
    
    # sequences in which to search the claim:
    sub_words <- doc_tagged$POSwords[(ind_start) : (ind_start + 5)] 
    sub_tags <- doc_tagged$POStags[(ind_start) : (ind_start + 5)]
    
    # searh for the first "CD" tag after the word we found in the previous part
    ind_claim_sub <- min(match(c("CD"), sub_tags))
    claim <- sub_words[c(ind_claim_sub -1 , ind_claim_sub)]
    
    if(is.na(claim)[1]){
      return(NA) 
    }else{
      claim = paste(claim, collapse = " ")
      return(claim)
    }
  }
}




