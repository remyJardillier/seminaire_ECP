##############################################################
## function that search the policy id into the doc (tagged) ##
##############################################################

search_policy_id <- function(doc_tagged, dic){
  
  # indices of the word "policy" in the vector containing the words
  inds_policy <- which(doc_tagged$POSwords %in% dic$policy_id$level1)
  
  # we keep the 5 first occurences of the word "policy"
  inds_policy <- inds_policy[1:min(length(inds_policy),3)]
  
  # index of the policy we want in the whole text:
  ind_policy_txt <- NA
  
  # test if there is "no" or "number" (level2 in the diccionary) after policy (if it's the case, 
  # there is a big probability that the information we're looking for is near this place)
  for(ind in inds_policy){
    
    # test if a second level word is close:
    if(any(doc_tagged$POSwords[c(ind+1,ind + 2)] %in% dic$policy_id$level2)){
      ind_policy_txt <- ind
      
      break
    }
  }
  
  # if the above test on the second level words failed, we remove the word policy folowing by "period"
  #  -> it is not the policy number
  if(is.na(ind_policy_txt)){
    
    # indexes to remove:
    inds_policy_tmp <- inds_policy
    
    for(i in 1:length(inds_policy)){
      if(doc_tagged$POSwords[inds_policy[i]+1] == dic$policy_id$level3[1]){
        inds_policy_tmp <- inds_policy[-i]
      }
    }
    inds_policy <- inds_policy_tmp
    
    # then, we take the first occurence of the word "policy"
    ind_policy_txt <- inds_policy[1]
  }
  
  # index where to start the research
  ind_start <- ind_policy_txt + 1
  if(doc_tagged$POSwords[ind_start] == ":"){
    ind_start <- ind_start + 1
  }
  # sequences in which to search the policy
  sub_words <- doc_tagged$POSwords[(ind_start) : (ind_start + 10)] # is 10 a good choice???
  sub_tags <- doc_tagged$POStags[(ind_start) : (ind_start + 10)]
  
  # searh for the first "CD" or "FW" tag after the word we found in the previous part
  ind_policy_sub <- min(match(c("CD", "FW"), sub_tags), na.rm = T)
  policy_id <- sub_words[ind_policy_sub]
  
  # return the policy_id
  return(policy_id)
}
