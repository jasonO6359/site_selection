source("renv/activate.R")

renv_status <- renv::status()
renv_synced <- renv_status$synchronized

if(renv_synced == FALSE) {
  
  renv::restore()
  
}

