print('#############################')
print('Site Selection Startup Script')
print('#############################')


################################################################################
# Set parameters
################################################################################
cur_year <- lubridate::year(lubridate::today())
set_year <- readline(paste0("Use current year? (", cur_year , ") (Y/N): "))

if(stringr::str_to_lower(set_year) == "y") {
  
  year <- cur_year
} else if(stringr::str_to_lower(set_year) == "n") {
  year <- readline("Enter year: ")
} else (stop("selection not recognized"))

# Ocklawaha River

river_list <- 
  list("ock" = list(river = "Ocklawaha River",
                    year = year,
                    target = "Community",
                    data = "data/csv/Ocklawaha_sites_all.csv",
                    column_mapping = 'list(Latitude = "Y",
                        ident = "FID_1_1",
                        Longitude = "X",
                        Habitat = "Habitat",
                        Zone = "River.Segment",
                        Grid = "GRID",
                        Waterbody = "REGION",
                        Minigrid = "Minigrid")'),
       "sfr" = list(river = "Santa Fe River",
                    year = year,
                    target = "Community",
                    data = NULL,
                    column_mapping = NULL),
       "suw" = list(river = "Suwannee River",
                    year = year, 
                    target = "Community",
                    data = NULL,
                    column_mapping = NULL)
       )

################################################################################

for(river in river_list) {

parms <- list(river = river$river, 
               year = river$year, 
               target = river$target, 
               data = river$data, 
               column_mapping = river$column_mapping)

file_name = paste0(parms$river, "_", parms$year,"_sites")
render_file = "md/ltm_site_selection"

formats <- c("html", "pdf")
for(f in formats) {
  parms$format <- ifelse(f == 'html', 'html', 'latex')
  render_qmd <- paste0(render_file, ".qmd")
  render_out <- paste0(render_file, ".", f)
  out_name <- paste0(file_name, ".", f)
  quarto::quarto_render(paste0(render_file,".qmd"),
                        execute_params = parms,
                        execute_dir = getwd(),
                        output_format = f,
                        profile="startup")
  
  file.copy(render_out,
            paste0("_output/",out_name))
  # 
  # file.remove(render_out)
  
}


}
# quarto::quarto_render("md/test.qmd")
# # renv::snapshot()

