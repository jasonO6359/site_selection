#' plot site map
#'
#' @param basemap 
#' @param sites 
#' @param point_size 
#' @param map_title 
#' @param save.to.file 
#' @param file_name 
#'
#' @return
#' @export
#'
#' @examples
plot_sitemap <- function(basemap, 
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
  #print(map_xlim)
  #print(map_ylim)
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