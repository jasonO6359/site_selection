library(magrittr, include.only - "%>%")

gpx_format <- function(x) {
  x %>% dplyr::select(name = ident, Long= Longitude, Lat= Latitude)
}
#gpx_format(rand_sites)

## Create a field site list
## Create a field site map.
## Create field gps points. 

# tst <- readLines("test_gpx_367.gpx")

add_gpx_data <- function(gpx_file, insert_data = NA, tagSchema = NA) {
 out <- gpx_file
 wpt_detect <- stringr::str_detect(out, pattern = "<wpt.")
 wpt_ct <- sum(wpt_detect)
 insert_obs <- ifelse(is.null(nrow(insert_data)), length(insert_data), nrow(insert_data))
 if(insert_obs != wpt_ct) {
   stop("Number of waypoints does not match nrow of insert_data")
 }
 inst <- c(0)
 for(x in 1:length(wpt_detect)) {
   inst[x] <- sum(wpt_detect[1:x])
 }
 inst[!wpt_detect] = 0
 insert_index = 0
 for(pt in 1:wpt_ct) {
   for(tags in 1:length(tagSchema)) {
     tagName = names(tagSchema)[tags]
     tagVal = insert_data[[tagSchema[[tags]]]][pt]
     tag = paste("<",tagName,">",tagVal,"</",tagName,">", sep = "")
     insert_pt <- which(inst == pt) + insert_index
     out <- c(out[1:insert_pt], tag, out[(insert_pt + 1):length(out)])
     insert_index = insert_index + 1
   }
 }
  out
}


create_gpx <- function(x, filename, output = TRUE) {
  outname <- if(stringr::str_sub(filename, -4) == ".gpx") {
    filename
  } else {
      paste(filename,".gpx", sep="")
    }
  if(outname %in% dir()) {file.remove(outname)}
  x_sf <- sf::st_as_sf(x, agr = c(name = 'identity'), coords = c("Long", "Lat"), crs = 4326)
  sf::st_write(obj = x_sf, dsn = outname, append = FALSE, config_options = c("GPX_USE_EXTENSIONS"="YES"), quiet=TRUE)
  tx <- readLines(outname)
  tx2 <- gsub(pattern = "<gpx version=\"1.1\" creator=\"pgirbric\">", 
              replace = "<gpx creator=\"Esri\" version=\"1.1\" xalan=\"http://xml.apache.org/xalan\" xmlns=\"http://www.topografix.com/GPX/1/1\" xsi=\"http://www.w3.org/2001/XMLSchema-instance\">",
              x = tx)
  if(output) {
    writeLines(tx2, outname)
  }
  tx2
}
 
# debug(output_gpx)
# rsites <- gpx_format(rand_sites)
# file_name <- paste("test_gpx_",sample(1:1000,1), sep = "")
# t <- create_gpx(rsites, file_name, output = FALSE)
# gpx_data <- rand_sites %>% 
#   mutate(flag = ifelse(selection == "Primary", "Navaid, Green", "Navaid, Red")) %>% 
#   select(ident, flag)
# final_gpx <- t %>% 
#   add_gpx_data(gpx_file = ., 
#                insert_data = gpx_data, 
#                tagSchema = c("name" = "ident", "sym" = "flag"))

# DELETE writeLines(ttt, "demo2.gpx")