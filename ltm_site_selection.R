load("ltm_sites.RData")
library(tidyverse)
library(sf)
library(ggmap)
library(sp)
library(ggspatial)
library(knitr)

default_selection <- list()
default_selection[["Suwannee River"]] = list(strata = TRUE,
                                             method = "center-line",
                                             primary = c(10), 
                                             alternate = c(5),
                                             map_lims_x = NA,
                                             map_lims_y = NA)
default_selection[["Santa Fe River"]] = list(strata = TRUE, 
                                             method = "center-line",
                                             primary = c("1" = 5,
                                                         "2" = 10,
                                                         "3" = 14,
                                                         "4" = 7), 
                                             alternate = c("1" = 2,
                                                           "2" = 3,
                                                           "3" = 3,
                                                           "4" = 3),
                                             map_lims_x = c(-82.88831, -82.52220),
                                             map_lims_y = c(29.82345, 29.95743))
default_selection[["Ocklawaha River"]] = list(strata = TRUE, 
                                              method = "grid",
                                              primary = c("1_Run" = 5,
                                                          "2_Run" = 6,
                                                          "3_IB" = 2,
                                                          "3_OB" = 3,
                                                          "3_Run" = 12,
                                                          "4_IB" = 2,
                                                          "4_OB" = 3,
                                                          "4_Run" = 7), 
                                              alternate = c("1_Run" = 2,
                                                            "2_Run" = 2,
                                                            "3_IB" = 2,
                                                            "3_OB" = 2,
                                                            "3_Run" = 4,
                                                            "4_IB" = 2,
                                                            "4_OB" = 2,
                                                            "4_Run" = 3),
                                              map_lims_x = c(-81.98691, -81.68753),
                                              map_lims_y = c(29.07968, 29.535506))

# format non-standard site list into expected format
format_siteList <- function(data, column_mapping = list(), column_fill = list(), method = "center-line") {
  
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

#write function to randomly select sites from total, inputs = waterbody, strata 
# (with default), selection scheme
random.sites <- function(waterbody = NA, selection_scheme = default_selection, sitelist=NA) {
  
  ##############################
  ## Default selection scheme ##
  ##############################
  
  
  ##################
  ## Input checks ##
  ##################
  
  if(is.null(nrow(sitelist))) {stop("No Site List Provided")}
  site_list = sitelist %>% mutate(Zone = as.character(Zone))
  if(is.na(waterbody)) {warning("No waterbody selected, selecting from all waterbodies in input sitelist")} else {
    site_list = sitelist %>% filter(Waterbody == waterbody)
    }
  if(nrow(site_list)==0) {warning("No sites selected")}
  
  if(is.null(selection_scheme[[waterbody]])) {warning(paste("Selection Scheme not
                                                            found for ",
                                                            waterbody,
                                                            ":: Default scheme 
                                                            used (25 primaries,
                                                            5 alternates, no strata)"))
    ss = list(strata = FALSE,
              primary = c(25),
              alternate = c(5))
    } else {
      ss = selection_scheme[[waterbody]]
      }
  
  if(ss[["strata"]]==FALSE) {
    sites_primary = site_list %>% sample_n(ss[["primary"]]) %>% mutate(selection = "Primary")
    sites_alternate = site_list %>% filter(!(ident %in% sites_primary$ident)) %>%
      sample_n(ss[["alternate"]]) %>% mutate(selection = "Alternate")
    sites = sites_primary %>% bind_rows(sites_alternate) %>% arrange(ident)
  } else {
    if(selection_scheme[[waterbody]]$method == "center-line"){
      site_list = site_list %>% mutate("Strata" = Zone)
      } else if(selection_scheme[[waterbody]]$method == "grid"){
        site_list = site_list %>%
          unite("Strata", Zone, Habitat, remove=FALSE)
      }
    strata = site_list %>% distinct(Strata)
    print(strata)
    
    n_strata = nrow(strata)
    print(n_strata)
    if(length(ss$primary) == 1) {pick_strata = rep(ss$primary,n_strata)
    names(pick_strata) == strata} else {
      if(length(ss$primary) == n_strata) {pick_strata = ss$primary} else {stop(paste("
                  strata selection vector length does 
                  not match number of strata::strata are: ",strata))}
    }
    if(length(ss$alternate) == 1) {pick_alt = rep(ss$alternate,n_strata)
    names(pick_alt) == strata} else {
      if(length(ss$alternate) == n_strata) {pick_alt = ss$alternate} else {stop("
                  alt strata selection vector length does 
                  not match number of strata")}
    }
    sites = site_list %>% sample_n(0)
    for(x in 1:n_strata) {
      cur_zone = strata$Strata[[x]]
      print(paste("Zone",cur_zone))
      print(pick_strata[cur_zone])
      print(pick_alt[[cur_zone]])
      site_sub = site_list %>% filter(Strata == cur_zone)
      site_sub_primary = site_sub %>%
        sample_n(pick_strata[[cur_zone]]) %>% mutate(selection = "Primary")
      site_sub_alt = site_sub %>% filter(!(ident %in% site_sub_primary$ident)) %>% 
        sample_n(pick_alt[[cur_zone]]) %>% mutate(selection = "Alternate")
      sites = sites %>% bind_rows(site_sub_primary) %>% bind_rows(site_sub_alt)
    }
  } 

  sites = sites %>% arrange(Zone, ident, desc(selection)) %>% 
    mutate(Zone = as.factor(Zone),
           Latitude = as.numeric(Latitude), 
           Longitude = as.numeric(Longitude))
}


gpx_format <- function(x) {
  x %>% dplyr::select(ID = ident,Long= Longitude, Lat= Latitude)
}
#head(test)
 
## Create a field site list
## Create a field site map.
## Create field gps points. 

output_gpx <- function(x, filename) {
  sf::st_write(data.frame(x), filename=filename, type="w")
  tx <- readLines(paste(filename,".gpx", sep=""))
  tx2 <- gsub(pattern = "<gpx version=\"1.1\" creator=\"pgirbric\">", replace = "<gpx creator=\"Esri\" version=\"1.1\" xalan=\"http://xml.apache.org/xalan\" xmlns=\"http://www.topografix.com/GPX/1/1\" xsi=\"http://www.w3.org/2001/XMLSchema-instance\">", x = tx)
  writeLines(tx2, paste(filename,".gpx", sep=""))
  }




import.rivermap <- function(shapefile, filter=NA) {
fc <- sf::st_read(shapefile)
fc <- ggspatial::df_spatial(fc) %>% mutate(NAME = ifelse(NAME == "Oklawaha River", "Ocklawaha River", NAME))
if(!is.na(filter)) {
  fc <- fc %>% dplyr::filter(NAME == filter)}
fc
}
# View the feature class

plot.sitemap <- function(basemap, 
                         sites, 
                         point_size = 4, 
                         map_title = "NO TITLE", 
                         save.to.file = FALSE, 
                         file_name = "NULL_FILENAME") {
wb = unique(sites$Waterbody)[[1]]

edge = 0.00001
map_xlim = c(min(basemap$x)-edge,max(basemap$x))+edge
map_ylim = c(min(basemap$y)-edge,max(basemap$y)+edge)

if(!is.null(default_selection[[wb]][[1]])) {
  if(!is.na(default_selection[[wb]]$map_lims_x[[1]])){
  map_xlim = default_selection[[wb]]$map_lims_x}
  if(!is.na(default_selection[[wb]]$map_lims_y[[1]])){
  map_ylim = default_selection[[wb]]$map_lims_y}
}
print(map_xlim)
print(map_ylim)
a <- ggplot() + 
  ggspatial::geom_spatial_path(data=basemap, 
                               aes(x=x, y=y, group=feature_id), 
                               color="black",
                               crs = 4326) +
  theme_void() + 
  coord_map(xlim = map_xlim, ylim= map_ylim) +
  geom_point(data=sites, aes(x=Longitude, y=Latitude, shape=Zone, fill=selection), color = "black", size = 4) +
  scale_shape_manual(values = c(21, 22, 24, 23)) +
  #scale_fill_manual(labels = c("Alt","Prime"),values = c("orange", "blue")) +
  guides(fill=guide_legend(override.aes=list(shape=21))) +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  ggtitle(map_title) #+
  #facet_wrap(.~Zone)
print(a)

  if(save.to.file) {
    
    lenwid = (max(sites$Latitude)-min(sites$Latitude))/(max(sites$Longitude)-
                                                          min(sites$Longitude))
    rs = 300
    if(lenwid <= 1) {ht = 8.5*rs
    wid = ifelse(ht/lenwid > 11*rs, 11*rs, ht/lenwid)} else {
      ht = 11*rs
      wid = ifelse(ht/lenwid > 8.5*rs, 8.5*rs, ht/lenwid)}
    png(paste(file_name,".png", sep=""),
         res = rs, 
         height = ht,
         width = wid)
    print(a)
    dev.off()
    
  }

a
}
