# install.packages("tidyverse")
# install.packages("pgirmess")


file <- "C:/Users/jason.oconnor/Desktop/spbu_ltm.csv" # change to correct file path for the csv you want to convert
ID_field = "ID" # change to the name of the ID field (e.g. site name) in your csv file
Lat_field = "Lat" # change to name of latitude field in your csv file
Long_field = "Long" # change to name of longitude field in your csv file
output_filename = "test12345"

csv_to_gpx <- function(x, ID = NULL, Long = Longitude, Lat= Latitude, output_filename = NULL) {
  library(tidyverse)
  library(pgirmess)
  if(is.null(ID)) {stop("ID column not specified")}
  if(!(ID %in% names(x))) {stop("ID column not found in csv input file, check id column spelling")}
  if(!(Long %in% names(x))) {stop("ID column not found in csv input file, check id column spelling")}
  if(!(Lat %in% names(x))) {stop("ID column not found in csv input file, check id column spelling")}
  if(is.null(output_filename)) {filename = x} else {filename = output_filename}
  temp <- x %>% mutate(ID = ID, Long = Long, Lat = Lat) %>% select(ID,Long,Lat)
  pgirmess::writeGPX(data.frame(temp), filename=filename, type="w")
  tx <- readLines(paste(filename,".gpx", sep=""))
  tx2 <- gsub(pattern = "<gpx version=\"1.1\" creator=\"pgirbric\">", replace = "<gpx creator=\"Esri\" version=\"1.1\" xalan=\"http://xml.apache.org/xalan\" xmlns=\"http://www.topografix.com/GPX/1/1\" xsi=\"http://www.w3.org/2001/XMLSchema-instance\">", x = tx)
  writeLines(tx2, paste(filename,".gpx", sep=""))
}


csv_to_gpx(data.frame(read.csv(file)), ID = "ID", Long = "Long", Lat = "Lat", output_filename)
