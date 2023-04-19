#' format non-standard site list into expected format
#'
#' @param data 
#' @param column_mapping 
#' @param column_fill 
#' @param method 
#'
#' @return
#' @export
#'
#' @examples
format_siteList <- 
  function(data, 
           column_mapping = list(), 
           column_fill = list(), 
           method = "center-line") {
  
  temp = data
  if(is.character(temp)) {
    temp = tibble(read.csv(temp, fileEncoding = "UTF-8-BOM"))
  } else if(!is_tibble(temp)){
    temp = tibble(temp)
  }
  
  if(method == "center-line"){
    out = tibble(
      type = character(),
      ident = character(),
      Latitude = numeric(),
      Longitude = numeric(),
      Zone = integer(),
      Boundary = logical(),
      Waterbody = character(),
      Exclude = logical()) %>%
      add_row(type = rep(NA, nrow(temp)))} else if(method == "grid") {
        out = tibble(
          Waterbody = character(),
          type = character(),
          ident = character(),
          Latitude = numeric(),
          Longitude = numeric(),
          Habitat = character(),
          Zone = integer(),
          Grid = integer(),
          Minigrid = integer(),
          Exclude = logical()) %>%
          add_row(type = rep(NA, nrow(temp)))
      } else {
        stop("method not recognized")
      }
  
  if(length(column_mapping) > 0) {
    for(x in 1:length(column_mapping)){
      var = names(column_mapping)[[x]]
      map = column_mapping[[x]]
      insert.dat = temp %>% dplyr::select(all_of(map)) %>% pull()
      out = out %>% 
        mutate(across(all_of(var), ~ insert.dat))
    }
  }
  
  if(length(column_fill) > 0) {
    for(x in 1:length(column_fill)){
      var = names(column_fill)[[x]]
      map = column_fill[[x]]
      insert.dat = map
      out = out %>% 
        mutate(across(all_of(var), ~ insert.dat))
    }
  }
  out
}

# test = format_siteList("Rodman.csv",
#                        column_mapping = list(Latitude = "lat", ident = "name", Longitude = "lon"),
#                        column_fill = list(Waterbody = "Test Lake"))

