###################################################
# -- fonction pour calculer la distance à la cote 
# -- 
# -- à partir de la latitude et de la longitude
###################################################

# libraries
require(sp)
require(ggplot2)

get_distance_to_coast <- function(shape_file,latitude, longitude){
  
  # test if the latitude and longitude are numeric
  if( (!is.numeric(latitude)) || (!is.numeric(longitude))){
    return(NA)
  }
  
  # longitude and latitude of the shape file points
  coord <- ggplot2::fortify(shape_file)[,c("lat", "long")]
  
  # distances to each point of the coast
  coordmat <- as.matrix(coord)
  DistanceCoast <- NULL
  Dist <- spDistsN1(coordmat,c(latitude,longitude),longlat=TRUE)
  
  # distance to the coast
  DistanceCoast <- as.numeric(Dist[which.min(Dist)]) 
  
  return(DistanceCoast)
}


