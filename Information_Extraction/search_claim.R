##########################################################
## function that search the claim into the doc (tagged) ##
##########################################################

#doc <- docs[[10]]
#doc_tagged <- tagPOS(doc = doc)

# function to see if a vector (POSords) countain another vector (w) 

#POSwords <- doc_tagged$POSwords
#w <- dic$claim$level1[[2]]

v_in_v <- function(POSwords, w){
  inds_tmp <- which(POSwords %in% w[1])
  inds <- rep(NA, length(inds_tmp))
  count <- 1
  
  for(i in inds_tmp){
    if(POSwords[i+1] == w[2]){
      inds[count] <- i
      count <- count+1
    }
  }
  
  inds <- inds[1:(count-1)]
}

# function to search the date
search_claim <- function(doc_tagged, dic){
  
  # indices of the word "date" in the vector containing the words
  inds_c <- which(doc_tagged$POSwords %in% dic$claim$level1[[1]])
  inds_al <- v_in_v(doc_tagged$POSwords, dic$claim$level1[[2]])
  inds_ca <- v_in_v(doc_tagged$POSwords, dic$claim$level1[[3]])
  inds_le <- v_in_v(doc_tagged$POSwords, dic$claim$level1[[4]])
  
  # other option to explore:
  # ind_al <- which(rollapply(doc_tagged$POSwords, 2, identical, dic$claim$level1[[4]]))
  
  # min indexes
  min_inds <- c(c = min(inds_c),
                al = min(inds_al),
                ca = min(inds_ca),
                le = min(inds_le))
  
  # index where we are going to look for the information:
  ind_min <- min(min_inds, na.rm = T)
  name = names(which(min_inds == ind_min))
  
  # test if there is "no"
  if(name == "c"){
    if(doc_tagged$POSwords[ind_min+1] %in% c("no", "reference")){
      inds_c <- inds_c[-1]
      
      # min indexes
      min_inds <- c(c = min(inds_c),
                    al = min(inds_al),
                    ca = min(inds_ca),
                    le = min(inds_le))
      
      # index where we are going to look for the information:
      ind_min <- min(min_inds, na.rm = T)
      name = names(which(min_inds == ind_min))
    }
  }
  
  # sequences in which to search the claim:
  sub_words <- doc_tagged$POSwords[(ind_min+1) : (ind_min + 5)] 
  sub_tags <- doc_tagged$POStags[(ind_min+1) : (ind_min + 5)]
  
  # # tag format:
  # claim_tag_formats <- list(c("JJ", "CD"),
  #                           c("CD", "JJ"))   
  
  # searh for the first "CD" tag after the word we found in the previous part
  ind_claim_sub <- min(match(c("CD"), sub_tags))
  claim <- sub_words[c(ind_claim_sub -1 , ind_claim_sub)]
  
  if(is.na(claim)[1]){
    return(NA) 
  }else{
    claim = paste(claim, collapse = " ")
    return(claim)
  }
  
  # # index of the beginning of the claim
  # ind_claim_sub <- NA
  # 
  # # length of the claim
  # l <- NA
  # 
  # # search for the tags corresponding to each claim formats (break if we found it)
  # for(i in 1:length(claim_tag_formats)){
  #   
  #   # search for the sequence of date tags into the sub-sequence of tags
  #   ind <- which(rollapply(sub_tags, length(claim_tag_formats[[i]]), identical, claim_tag_formats[[i]]))
  #   
  #   # if we found the date, we save its length and its index
  #   if(length(ind) != 0){
  #     l <- length(claim_tag_formats[[i]])
  #     ind_claim_sub <- ind
  #     
  #     break
  #   }
  # }
  # ind_claim_sub <- ind_claim_sub[1] 
  # 
  # # if we found the claim, we return it
  # if( ! is.na(l)){
  #   claim <- paste(sub_words[ind_claim_sub: (ind_claim_sub + l -1)], sep = " ", collapse = " ")
  #   return(claim)
  # }else{
  #   # if not, we return NA
  #   return(NA)
  # }

}

