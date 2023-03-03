library(magrittr, include.only - "%>%")

gpx_format <- function(x) {
  x %>% dplyr::select(ID = ident,Long= Longitude, Lat= Latitude)
}
#gpx_format(rand_sites)

## Create a field site list
## Create a field site map.
## Create field gps points. 

output_gpx <- function(x, filename) {
  outname <- paste(filename,".gpx", sep="")
  if(outname %in% dir()) {file.remove(outname)}
  x_sf <- sf::st_as_sf(x, coords = c("Long", "Lat"))
  sf::st_write(obj = x_sf, dsn=outname, type="w")
  tx <- readLines(paste(filename,".gpx", sep=""))
  tx2 <- gsub(pattern = "<gpx version=\"1.1\" creator=\"pgirbric\">", replace = "<gpx creator=\"Esri\" version=\"1.1\" xalan=\"http://xml.apache.org/xalan\" xmlns=\"http://www.topografix.com/GPX/1/1\" xsi=\"http://www.w3.org/2001/XMLSchema-instance\">", x = tx)
  writeLines(tx2, paste(filename,".gpx", sep=""))
}
# debug(output_gpx)
# output_gpx(gpx_format(rand_sites), file_name)