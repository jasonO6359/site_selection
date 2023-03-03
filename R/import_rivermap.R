#' import river map
#'
#' @param shapefile 
#' @param filter 
#'
#' @return
#' @export
#'
#' @examples
import_rivermap <- function(shapefile, filter=NA) {
  fc <- sf::st_read(shapefile)
  fc <- ggspatial::df_spatial(fc) %>% mutate(NAME = ifelse(NAME == "Oklawaha River", "Ocklawaha River", NAME))
  if(!is.na(filter)) {
    fc <- fc %>% dplyr::filter(NAME == filter)}
  fc
}