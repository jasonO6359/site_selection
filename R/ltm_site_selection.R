library(tidyverse)
# library(sf)
# library(ggmap)
# library(sp)
# library(ggspatial)
# library(knitr)

# import -----------------------------------------------------------------------

## ltm_sites -------------------------------------------------------------------

load("data/ltm_sites.RData")

## default_selection -----------------------------------------------------------

source("R/default_selection.R")

if(!is.null(default_selection[[params$river]])) {
  site_selection_method = default_selection[[params$river]]$method
}

## format_siteList() -----------------------------------------------------------

source("R/format_siteList.R")

## random_sites()    -----------------------------------------------------------

source("R/random_sites.R")

## gpx_tools -------------------------------------------------------------------

source("R/gpx_tools.R")

## import_rivermap() -----------------------------------------------------------

source("R/import_rivermap.R")

## plot_sitemap() -----------------------------------------------------------

source("R/plot_sitemap.R")


# View the feature class


