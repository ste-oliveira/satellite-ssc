#### LIBRARY IMPORTS ####

#install.packages("wrapr")
library(wrapr)

#install.packages("base")
library(base)

#install.packages("mod")
library(mod)

#install.packages("dataRetrieval")
library(dataRetrieval)

#install.packages("tidyhydat")
library(tidyhydat)

#install.packages("readr")
library(readr)

#install.packages("readxl")
library(readxl)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("ggthemes")
library(ggthemes)

#install.packages("RColorBrewer")
library(RColorBrewer)

#install.packages("ggpubr")
library(ggpubr)

#install.packages("gstat")
library(gstat)

#install.packages("ggspatial")
library(ggspatial)

#install.packages("svglite")
library(svglite)

#install.packages("plotly")
library(plotly)

#install.packages("data.table")
library(data.table)

#install.packages("dplyr")
library(dplyr) #hoping to move away from dplyr

#install.packages("tidyverse")
library(tidyverse)

#install.packages("tidyquant")
library(tidyquant)

#install.packages("tidyr")
library(tidyr)

#install.packages("broom")
library(broom)

#install.packages("modelr")
library(modelr)

#install.packages("scales")
library(scales)

#install.packages("kdensity")
library(kdensity)

#install.packages("NbClust")
library(NbClust)

#install.packages("zoo")
library(zoo)

#install.packages("segmented")
library(segmented)

#install.packages("lubridate")
library(lubridate)

#install.packages("reshape2")
library(reshape2)

#install.packages("matrixStats")
library(matrixStats)

#install.packages("smoother")
library(smoother)

#install.packages("glmnet")
library(glmnet)

#install.packages("boot")
library(boot)

#install.packages("kernelboot")
library(kernelboot)

#install.packages("np")
library(np)

#install.packages("automap")
library(automap)

#detach("package:sp", unload = TRUE)
#install.packages("sp", dep = TRUE)
require (sp)
library(sp)

#install.packages("USAboundaries")
library(USAboundaries)

#install.packages("sf")
library(sf)

#install.packages("rgeos")
library(rgeos)

#install.packages("raster")
library(raster)

#install.packages("rgdal")
library(rgdal)

#install.packages("maptools")
library(maptools)

#install.packages("PBSmapping")
library(PBSmapping)

#### SET DIRECTORIES ####
  # Set root directory
  wd_root <- "C:/Users/stefo/Documents/Doutorado/Dados_Hidro/GIT/satellite-ssc/"
  
  # Imports folder (store all import files here)
  wd_imports <- paste0(wd_root,"/imports/")
  
  # Exports folder (save all figures, tables here)
  wd_exports <- paste0(wd_root,"/exports/")
  
  wd_figures <- paste0(wd_exports, "/ssc-figures")
  wd_exports_gc <- paste0(wd_exports,"/ssc-gc-plots")
  wd_station_standalone <- paste0(wd_exports, "/ssc-station-vs-standalone-models")
  wd_standalone_models <- paste0(wd_exports, "/ssc-standalone-models")
  wd_standalone_figures <- paste0(wd_standalone_models, "/ssc-standalone-figures")
  wd_autocorrelation <- paste0(wd_exports, "/ssc-autocorrelation")

  # Create folders within root directory to organize outputs if those folders do not exist
  export_folder_paths <- c(wd_exports, wd_figures, wd_exports_gc,wd_station_standalone, 
                           wd_standalone_models, wd_standalone_figures, wd_autocorrelation)
  
 
  #### INITIALIZE MAP DATA FOR TAQUARI ####
  setwd (wd_imports)
  
  # set projection for states shapefile
  projection <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  # import taquari provinces
  taquari <- read_sf(dsn = "C:/Users/stefo/Documents/Doutorado/Dados_Hidro/GIT/satellite-ssc/imports/shape/estacoes_sed_gee_taquari.shp", layer = "estacoes_sed_gee_taquari")
  taquari_geom <- st_geometry(taquari)
  # attributes(taquari_geom)

  # convert shapefile to Spatial class
  taquari <- as(taquari, 'Spatial')
  taquari <- spTransform(taquari, projection)
  
  # fortify state shapefile.
  #taquari <- fortify(taquari)
  
  #####Se quiser visualizar os pontos, descomenta a linha abaixo
  #plot(taquari)

  
  #####N?o precisamos igualar a proje??o porque estamos usando apenas uma camada
  # coordinates(canada_prov) <- ~long+lat
  #proj4string(canada_prov) <- proj4string(us_states)
  
  
  ######N?o precisamos deste bloco pq ele s? est? renomeando a 
  ######coluna das camadas, selecionando as que tem nome e juntando 
  ######os pontos do eua com canada.
  #names(canada_prov) <- c('name','frenchName') # rename columns labeling canadian provinces
  #canada_prov <- canada_prov[,c('name')] # only select column with province name
  #us_states <- us_states[,c('name')] # only select column with province name
  #us_states <- rbind(us_states,canada_prov) # combine canadian and us shapefiles
  
  
  #### --- ####
  #### IMPORT AND CLEAN -- LANDSAT DATA ####
  set.seed(1)
  # raw continuous data for regression
  
  # Import landsat spectral data from each site of interest
  ls_raw <- fread("C:/Users/stefo/Documents/Doutorado/Dados_Hidro/GIT/satellite-ssc/imports/gee/table1_taquari.csv") 
  
  ###### Coloque para a coluna site_no como taquari, mas acho que vamos precisar colocar
  ###### o nome da esta??o na hora de exportar os dados do GEE. (n?o tenho certeza disso ainda)
  #ls_raw_1 <- na.omit(ls_raw[,':='(site_no = "ifelse(name == "",as.character(site_no),gsub('usgs|qw|dv',"",name))",
  ls_raw_1 <- na.omit(ls_raw[,':='(site_no = "Taquari",
                                   # Rename columns for simplicity
                                   B1 = B1_median,
                                   B2 = B2_median,
                                   B3 = B3_median,
                                   B4 = B4_median,
                                   B5 = B5_median,
                                   B6 = B6_median,
                                   B7 = B7_median,
                                   nd52 = nd_median,
                                   num_pix = B2_count,
                                   sample_dt = ymd(date),
                                   landsat_dt = ymd(date)
  )], cols = c('B1','B2','B3','B4','B5','B7'))[
    B1 > 0 & B2 > 0 & B3 > 0 & B4 > 0 & B5 > 0 & B7 > 0][
      ,':='( 
        # add new columns with band ratios
        ##### Removi as colunas snow_ice_qa_3km e cloud_qa_3km para funcionar. Temos que verificar se vamos 
        #### precisar dessas colunas depois
        B2.B1 = B2/B1,
        B3.B1 = B3/B1,
        B4.B1 = B4/B1,
        B5.B1 = B5/B1,
        B7.B1 = B7/B1,
        B3.B2 = B3/B2,
        B4.B2 = B4/B2,
        B5.B2 = B5/B2,
        B7.B2 = B7/B2,
        B4.B3 = B4/B3,
        B5.B3 = B5/B3,
        B7.B3 = B7/B3,
        B5.B4 = B5/B4,
        B7.B4 = B7/B4,
        B7.B5 = B7/B5,
        Latitude = lat,
        Longitude = lon,
        station_nm = site_no,
        sensor = ifelse(grepl('LT',`system:index`),'Landsat 5','Landsat 7'))][ 
          # select only columns of interest
          ,.(station_nm, sensor, site_no, Latitude,Longitude,sample_dt, num_pix, landsat_dt,
             B1,B2,B3,B4,B5,B6,B7,B2.B1,B2.B1,B3.B1,B4.B1,B5.B1,B7.B1,B3.B2,B4.B2,B5.B2,
               B7.B2,B4.B3,B5.B3,B7.B3,B5.B4,B7.B4,B7.B5,nd52,cloud_cover,cloud_qa_count,snow_ice_qa_count, 
             solar_az, solar_zen,sr_atmos_opacity_median,sr_cloud_qa_median
          )]
  
  ## Identify sites with too few Landsat measurements to be reliable
  
  # Calculate number of satellite samples, per site
  n_sat_samples <- ls_raw_1[,.(N_samples = .N), 
                            by = .(site_no, Latitude, Longitude)]
  
  #retirei#stns_too_narrow <- fread('ssc_stns_too_narrow.dat')# retirei
  #retirei# Select sites with > 100 satellite measurements, remove sites that are too narrow ##
  #retirei#site_no_n100 <- n_sat_samples[N_samples >= 100 & 
  #retirei                                !(site_no %chin% stns_too_narrow$station_nm), 
  #retirei                              site_no] 
  #retirei
  #retirei## Filter landsat data by sites with > 100 satellite measurements & wide enough river ##
  #retirei#ls_raw_1 <- ls_raw_1[site_no %chin% site_no_n100]
  #retirei#n_sat_samples_n100 <- n_sat_samples[site_no %chin% site_no_n100]
 
  
  
  # Plot number of satellite samples per site as a histogram
  n_sat_samples_histogram <- 
    ggplot(n_sat_samples, aes(x = N_samples)) + 
    geom_histogram(color = 'black', lwd = 0.25, binwidth = 100) +
    geom_vline(data = n_sat_samples[,.(N_samples = mean(N_samples))], aes(xintercept = N_samples),
               color = 'orange', lty = 'dashed') + 
    geom_text(data = n_sat_samples[,.(N_samples = mean(N_samples))], 
              aes(label = paste0('mean = ',round(N_samples), ' samples'), x = N_samples + 40, y = 120),
              hjust = 0, size = 3) +
  
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
    labs(
      x = 'Number of cloud-free satellite samples/site',
      y = 'Number of sites'
    )
  
   # Save satellite images/site histogram
   ggsave(n_sat_samples_histogram, filename = paste0(wd_figures,'n_sat_samples_histogram.pdf'), width = 4, height = 4, useDingbats = F)
  
   
   
   #### IMPORT AND CLEAN -- IN SITU DATA ####
   
   # IMPORTAR DADOS IN SITU
   taquari_insitu_raw <- fread('taquari_insitu.csv') [,'EstacaoCodigo' := as.character(EstacaoCodigo)]
   taquari_insitu_site_nos <- unique(taquari_insitu_raw[!is.na(EstacaoCodigo),EstacaoCodigo])
  
    # Import site info, add and rename columns
   taquari_sts <- fread('taquari_insitu.csv',
                      colClasses =list(character = 'EstacaoCodigo'))[
                        , .(EstacaoCodigo = as.character(EstacaoCodigo), 
                            river_nm = paste0("River Name", " at ", "Fluviometric Station"), 
                            drainage_area_km2 = as.numeric("Drainage Area (KmÂ² )")*1000, 
                            Latitude, Longitude)][EstacaoCodigo %chin% taquari_insitu_site_nos]
   
   setkey(taquari_insitu_raw,EstacaoCodigo)
   setkey(taquari_sts,EstacaoCodigo)
   
   # Join raw in situ data with site info, add and rename columns
   taquari_insitu_raw_1 <- taquari_insitu_raw[taquari_sts][,':='(agency_cd = 'ANA',
                                                               EstacaoCodigo = as.character(EstacaoCodigo),                                                  
                                                     sample_depth_m = 'Surface', 
                                                     sample_datetime = ymd_hms(Date),
                                                     sample_dt = date(ymd_hms(Date)),
                                                     # sample_tm = as.character(hms(ymd_hms(Date))),
                                                     Q_cms = NA,
                                                     data_type = 'qw',
                                                     POC_mgL = NA,
                                                     p63 = NA,
                                                     width_m = NA,
                                                     alt_m = NA,
                                                     begin_date = min(date(ymd_hms(Date))),
                                                     end_date = max(date(ymd_hms(Date)))
   )][!is.na(SSC_mgL),.(agency_cd,station_nm, site_no, sample_dt, 
                        SSC_mgL, POC_mgL, p63, sample_method, sampler, sample_depth_m, width_m,
                        Latitude, Longitude, drainage_area_km2, alt_m,
                        data_type,begin_date,end_date)]
   
  
   
   
   
   
   
   
   
   
   
   
   
   
   