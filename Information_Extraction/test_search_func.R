##############################################################
# -- Scrip to import the data and test the functions
# --
##############################################################

main_dir = "D:/Documents/Centrale/3A/seminaire_AXA/dossier_robin"

# functions needed:
setwd(dir = paste(main_dir,"/code/IE", sep = ""))
source(file = "tagPOS.R")
source(file = "v_in_v.R")

source(file = "search_date.R")
source(file = "search_policy_id.R")
source(file = "search_date_dummy.R")
source(file = "search_claim.R")
source(file = "search_adress.R")

source(file = "get_distance_to_coast.R")
source(file = "visualize_locations.R")

# -- create the corpus
setwd(dir = paste(main_dir, "/FICHES_TRIEES", sep = ""))

# import pdf file names
file_names <- list.files(pattern = "\\.pdf$", ignore.case = T, recursive = T)

# -- library to install:
require(tm)
require(rgdal) # to import the shapefile
require(maptools)

# -- create the corpus
Rpdf <- readPDF(control = list(text = "-layout"))
docs <- Corpus(URISource(file_names), 
               readerControl = list(reader = Rpdf))

# strip White space (remove unusesefull spaces) and to lower.
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))

# function to see the beggining of a text
viewDocs <- function(docs, n, l) {docs[[n]]$content[1:l]}

# -- diccionary
dic <- list(date = list(level1 = c("date"), 
                        level2 = c("loss", "incident", "sinister", "event")),
            
            policy_id = list( level1 = c("policy"), 
                              level2 = c("number", "no", "nâº", "nº"), 
                              level3 = c("period")),
            
            claim = list(level1 = list("claim",c("adjusted", "loss"), c("claimed", "amount"), 
                                       c("loss", "estimate"), c("estimated", "loss")),
                         level2 = list(c("not", "formally", "quantified") ,c("not", "formally", "quantified", "yet"),
                                       c("not", "yet", "formally", "quantified" ))
                         ),
            
            adress = list(level1 = c("location of loss", "location of incident", "loss location", "event location", "location name", "location",
                          "located at", "situation of loss", 
                          "place of incident", "place of loss"),
                          level2 = c("various", "different adresses", "several locations", "several adresses", "different locations")
                          )
            )

# -- shape file
shape_file = readOGR(dsn = paste(main_dir, "/MEX borders", sep =""), layer = "MEX_adm0")

# -- table to save the results
table_test <- data.frame(matrix(ncol = 8, nrow = length(docs)))
colnames(table_test) = c("name", "date", "policy_id", "claim", "adress", "latitude", "longitude", "distance_to_coast")
table_test[,1] <- names(docs)

# we apply the function IE on each text of the corpus
for(k in 6:length(docs)){
  
  # tag le doc avec la fonction tagPOS défini précédemment
  doc_tagged <- tagPOS(docs[[k]]) 
  
  # appel des fonctions
  # table_test[k,2] <- search_date(doc_tagged, dic)
  # table_test[k,3] <- search_policy_id(doc_tagged, dic)
  # table_test[k,4] <- search_claim(doc_tagged, dic)
   adress_lat_long <- search_adress(doc_tagged, dic)
   table_test[k,5] <- adress_lat_long[[1]]
   table_test[k,6] <- adress_lat_long[[2]]
   table_test[k,7] <- adress_lat_long[[3]]
   table_test[k,8] <- get_distance_to_coast(shape_file, adress_lat_long$latitude, adress_lat_long$longitude)
}

# export the table
setwd(dir = paste(main_dir,"/code/IE", sep = ""))
write.table(x = table_test, file = "table_results.csv", sep = ",", row.names = F, col.names = T)

#--------------------------------#
# visialize the location on a map

# convert to numeric and remove NAs
names <- table_test$name
latitudes_table <- as.numeric(table_test$latitude)
names <- names[! is.na(latitudes_table)]

latitudes <- as.numeric(table_test$latitude)
latitudes <- latitudes[! is.na(latitudes)]

longitudes <- as.numeric(table_test$longitude)
longitudes <- longitudes[! is.na(longitudes)]

visualize_locations(names, latitudes, longitudes)

#-------------------------------------------------------------------#
# comparaison des deux fonctions "search_date", "search_date_dummy":

# table to save the results
table_test_date <- data.frame(matrix(ncol = 3, nrow = length(docs)))
colnames(table_test_date) = c("name", "date", "date_dummy")
table_test_date[,1] <- names(docs)

for(k in 1:length(docs)){
  doc_tagged <- tagPOS(docs[[k]])
  
  # date
  table_test_date[k,2] <- search_date(doc_tagged, dic)
  
  # date_dummy
  table_test_date[k,3] <- search_date_dummy(doc_tagged, dic)
}
