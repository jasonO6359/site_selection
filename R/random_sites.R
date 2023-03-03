#' randomly select sites from total
#'
#' @param waterbody 
#' @param selection_scheme 
#' @param sitelist 
#'
#' @return
#' @export
#'
#' @examples
random_sites <- function(waterbody = NA, selection_scheme = default_selection, sitelist=NA) {
  
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