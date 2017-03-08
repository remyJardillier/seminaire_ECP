##################################################
# -- Function to visualize sinister locations
# -- 
##################################################

# libraries
require(googleVis)

visualize_locations <- function(names, latitudes, longitudes){
  
  # data frame with the names and latitudes and longitudes in the format "lat:long" (for the fucntion gvisMap)
  names_lat_long_tmp <- data.frame(names = names, lat = latitudes, long = longitudes)
  names_lat_long_tmp <- names_lat_long_tmp[complete.cases(names_lat_long_tmp),]# delete rows with NA 
  
  # data frame with the name and latitude / longitude in the format "lat:long" (for the fucntion gvisMap)
  names_lat_long <- data.frame(names = names_lat_long_tmp$names, lat_long = paste(names_lat_long_tmp$lat,
                                                                                  ":", names_lat_long_tmp$long, sep = ""))
  
  # icon for the locations on the map
  marker <- "'http://icons.iconarchive.com/icons/icons-land/vista-map-markers/32/Map-Marker-Marker-Outside-Azure-icon.png'"
  
  # creation of the map
  map <- gvisMap(names_lat_long, locationvar = "lat_long", tipvar =  "names", 
                  options=list(showTip=TRUE, showLine=F, enableScrollWheel=TRUE, 
                               mapType='normal', useMapTypeControl=TRUE, width=800,height=800,
                               icons=paste0("{",
                                            "'default': {'normal': ", marker,
                                            "}}")
                  ))
  # plot the map
  plot(map)
  
}


