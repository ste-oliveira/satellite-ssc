#### SET DIRECTORIES ####
# Set root directory
wd_root <- "../"

# Imports folder (store all import files here)
wd_imports <- paste0(wd_root,"/imports/")

# Exports folder (save all figures, tables here)
wd_exports <- paste0(wd_root,"/exports/")

wd_figures <- paste0(wd_exports, "/ssc-figures/")
wd_exports_gc <- paste0(wd_exports,"/ssc-gc-plots/")
wd_station_standalone <- paste0(wd_exports, "/ssc-station-vs-standalone-models/")
wd_standalone_models <- paste0(wd_exports, "/ssc-standalone-models/")
wd_standalone_figures <- paste0(wd_standalone_models, "/ssc-standalone-figures/")
wd_autocorrelation <- paste0(wd_exports, "/ssc-autocorrelation/")

# Create folders within root directory to organize outputs if those folders do not exist
export_folder_paths <- c(wd_exports, wd_figures, wd_exports_gc,wd_station_standalone, 
                         wd_standalone_models, wd_standalone_figures, wd_autocorrelation)

for(i in 1:length(export_folder_paths)){
   path_sel <- export_folder_paths[i]
   if(!dir.exists(path_sel)){
      dir.create(path_sel)}
}

#### INITIALIZE MAP DATA FOR TAQUARI ####
setwd (wd_imports)