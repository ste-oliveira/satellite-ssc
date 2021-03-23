#### LIBRARY IMPORTS ####
install.packages('data_table')
library(data.table)

install.packages('sp')
library(sp)

install.packages('ggplot2')
library(ggplot2)

#### SET DIRECTORIES ####
  # Set root directory
  wd_root <- "C:/Users/stefo/Documents/Doutorado/Dados_Hidro/GIT/satellite-scc"
  
  # Imports folder (store all import files here)
  wd_imports <- paste0(wd_root,"C:/Users/stefo/Documents/Doutorado/Dados_Hidro/GIT/satellite-scc")
  # Exports folder (save all figures, tables here)
  wd_exports <- paste0(wd_root,"C:/Users/stefo/Documents/Doutorado/Dados_Hidro/GIT/satellite-scc/exports")
  
  wd_figures <- paste0(wd_exports, "C:/Users/stefo/Documents/Doutorado/Dados_Hidro/GIT/satellite-scc/exports/ssc-figures")
  wd_exports_gc <- paste0(wd_exports,"C:/Users/stefo/Documents/Doutorado/Dados_Hidro/GIT/satellite-scc/exports/ssc-gc-plots")
  wd_station_standalone <- paste0(wd_exports, "C:/Users/stefo/Documents/Doutorado/Dados_Hidro/GIT/satellite-scc/exports/ssc-station-vs-standalone-models")
  wd_standalone_models <- paste0(wd_exports, "C:/Users/stefo/Documents/Doutorado/Dados_Hidro/GIT/satellite-scc/exports/ssc-standalone-models")
  wd_standalone_figures <- paste0(wd_standalone_models, "C:/Users/stefo/Documents/Doutorado/Dados_Hidro/GIT/satellite-scc/exports/ssc-standalone-figures")
  wd_autocorrelation <- paste0(wd_exports, "C:/Users/stefo/Documents/Doutorado/Dados_Hidro/GIT/satellite-scc/exports/ssc-autocorrelation")

  # Create folders within root directory to organize outputs if those folders do not exist
  export_folder_paths <- c(wd_exports, wd_figures, wd_exports_gc,wd_station_standalone, 
                           wd_standalone_models, wd_standalone_figures, wd_autocorrelation)
 
  
  #### INITIALIZE MAP DATA FOR TAQUARI ####

  # taquari <- taquari[taquari$state_abbr != "AK" & taquari$state_abbr != "HI" & taquari$state_abbr != "PR",]
  
  # convert shapefile to Spatial class
  taquari <- as(taquari, 'Spatial')
  
  # plot(taquari)
  # plot(taquari_merge)
  
  # set projection for states shapefile
  projection <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  
  #### --- ####
  #### IMPORT AND CLEAN -- LANDSAT DATA ####
  set.seed(1)
  # raw continuous data for regression
  
  # Import landsat spectral data from each site of interest
  ls_raw <- fread('C:/Users/stefo/Documents/Doutorado/Dados_Hidro/GEE/Taquari/table1_taquari.csv') 
  
  ls_raw_1 <- na.omit(ls_raw[,':='(site_no = ifelse(name == "",as.character(site_no),gsub('usgs|qw|dv',"",name)),
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
             B7.B2,B4.B3,B5.B3,B7.B3,B5.B4,B7.B4,B7.B5,nd52,cloud_cover,cloud_qa_count,cloud_qa_3km,snow_ice_qa_count, snow_ice_qa_3km,
             solar_az, solar_zen,sr_atmos_opacity_median,sr_cloud_qa_median
          )]

  ## Identify sites with too few Landsat measurements to be reliable
  
  # Calculate number of satellite samples, per site
  n_sat_samples <- ls_raw_1[,.(N_samples = .N), 
                            by = .(site_no, Latitude, Longitude)]
 
  stns_too_narrow <- fread('ssc_stns_too_narrow.dat')
  
  # Select sites with > 100 satellite measurements, remove sites that are too narrow
  site_no_n100 <- n_sat_samples[N_samples >= 100 &
                                  !(site_no %chin% stns_too_narrow$station_nm), 
                                site_no]   

  # Filter landsat data by sites with > 100 satellite measurements & wide enough river
  ls_raw_1 <- ls_raw_1[site_no %chin% site_no_n100]

  n_sat_samples_n100 <- n_sat_samples[site_no %chin% site_no_n100]
  
  # Plot number of satellite samples per site as a histogram
  n_sat_samples_histogram <- 
    ggplot(n_sat_samples_n100, aes(x = N_samples)) + 
    geom_histogram(color = 'black', lwd = 0.25, binwidth = 100) +
    geom_vline(data = n_sat_samples_n100[,.(N_samples = mean(N_samples))], aes(xintercept = N_samples),
               color = 'orange', lty = 'dashed') + 
    geom_text(data = n_sat_samples_n100[,.(N_samples = mean(N_samples))], 
              aes(label = paste0('mean = ',round(N_samples), ' samples'), x = N_samples + 40, y = 120),
              hjust = 0, size = 3) +
    season_facet + 
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
    labs(
      x = 'Number of cloud-free satellite samples/site',
      y = 'Number of sites'
    )
  # Save satellite images/site histogram
  ggsave(n_sat_samples_histogram, filename = paste0(wd_figures,'n_sat_samples_histogram.pdf'), width = 4, height = 4, useDingbats = F)

  #help
  ?na.omit
  