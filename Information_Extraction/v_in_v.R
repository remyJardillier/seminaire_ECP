#######################################################################
# function to see if a vector (POSwords) countain another vector (w) ## 
#######################################################################

v_in_v <- function(w, POSwords){
  l <- length(w)
  ind <- which(rollapply(POSwords, l, identical, w))
  
  
  if(length(ind) == 0){
    # return NA is there is no index
    return(NA)
  }else{
    return(ind)
  }
}

