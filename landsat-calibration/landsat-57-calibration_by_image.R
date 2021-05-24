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

library(cluster)
library(factoextra)
library(FactoMineR)


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

# set projection for states shapefile
projection <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# import taquari provinces
taquari <- read_sf(dsn = "shape/estacoes_sed_gee_taquari.shp", layer = "estacoes_sed_gee_taquari")
taquari_geom <- st_geometry(taquari)
# attributes(taquari_geom)

# convert shapefile to Spatial class
taquari <- as(taquari, 'Spatial')
taquari <- spTransform(taquari, projection)

#### IMPORT AND CLEAN -- LANDSAT DATA ####
set.seed(1)
# raw continuous data for regression

# Import landsat spectral data from each site of interest
ls_raw <- fread("gee/table1_taquari.csv")

###### Coloque para a coluna site_no como taquari, mas acho que vamos precisar colocar
###### o nome da esta??o na hora de exportar os dados do GEE. (n?o tenho certeza disso ainda)
ls_raw_1 <- 
     na.omit(ls_raw[,
        ':='(site_no = as.character(site_no), # Rename columns for simplicity
          station_nm = station_nm,
          B1 = B1_median,
          B2 = B2_median,
          B3 = B3_median,
          B4 = B4_median,
          B5 = B5_median,
          B6 = B6_median,
          B7 = B7_median,
          nd52 = nd_median,
          num_pix = B2_count,
          landsat_dt = dmy(date)
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
                    sensor = ifelse(grepl('LT',`system:index`),'Landsat 5','Landsat 7'))
          ][ 
               # select only columns of interest
               ,.(station_nm, sensor, site_no, Latitude,Longitude, num_pix, landsat_dt,
                  B1,B2,B3,B4,B5,B6,B7,B2.B1,B2.B1,B3.B1,B4.B1,B5.B1,B7.B1,B3.B2,B4.B2,B5.B2,
                  B7.B2,B4.B3,B5.B3,B7.B3,B5.B4,B7.B4,B7.B5,nd52,cloud_cover,cloud_qa_count,snow_ice_qa_count, 
                  solar_az, solar_zen,sr_atmos_opacity_median,sr_cloud_qa_median
               )]

## Identify sites with too few Landsat measurements to be reliable

# Calculate number of satellite samples, per site
n_sat_samples <- 
     ls_raw_1[,.(N_samples = .N), by = .(station_nm)]

# Plot number of satellite samples per site as a histogram
# Renan - Plotar número de amostras por estação
n_sat_samples_histogram <- 
        ggplot(n_sat_samples, aes(x = station_nm, y=N_samples)) + 
        geom_bar(stat="identity", width=0.5, color = 'black') +
        labs(
                x = 'Estação',
                y = 'Número de amostras sem interferências atmosféricas'
        )
        
# Save satellite images/site histogram
ggsave(n_sat_samples_histogram, filename = paste0(wd_figures,'n_sat_samples_histogram.pdf'), width = 10, height = 4, useDingbats = F)

#### IMPORT AND CLEAN -- IN SITU DATA ####

# IMPORTAR DADOS IN SITU
taquari_insitu_raw <- fread('taquari_insitu.csv') [, 'site_no' := as.character(site_no)]

#Renan - Mantendo apenas dados da estacao Pedro Gomes - 66845000
#taquari_insitu_raw <- taquari_insitu_raw[site_no == 66845000] 

taquari_insitu_site_nos <- unique(taquari_insitu_raw[!is.na(station_nm),station_nm])

#### JOIN LANDSAT AND IN SITU DATA, WITH LAG OF UP TO 10 DAYS, RESTRICT TO < 3 DAYS ####
## Join Landsat data with in situ data, allowing for as much as a 10-day lead/lag
# Join Landsat data with in situ data
lag_days <- 8 
# taquari_insitu_raw <- setDT(ls_clean)[all_acy_insitu_daily_mean, roll = lag_days][ # uni-directional join
ls_insitu_raw <- setDT(ls_raw_1)[, ':='( 
             match_dt_start = landsat_dt - lag_days,
             match_dt_end = landsat_dt + lag_days)
        ][
             taquari_insitu_raw[,':='( match_dt = dmy(sample_date) )
        ], 
             ####falta incluir a estacao
             on = .(station_nm == station_nm, match_dt_start <= match_dt, match_dt_end >= match_dt)
        ][ 
             # bi-directional join
             !is.na(B1)
        ][ # removes days with in situ SSC but no Landsat image
             ,lag_days := as.numeric(difftime(dmy(sample_date),landsat_dt),'days')
        ][
          # Add Log10 SSC, squared cols, and remove no values, too cold (B6 < 269) values
          as.double(ConcentracaoMatSuspensao) > 0,
          ':='(
               log10_SSC_mgL = log10(ConcentracaoMatSuspensao),
               B1.2 = B1^2,
               B2.2 = B2^2,
               B3.2 = B3^2,
               B4.2 = B4^2,
               B5.2 = B5^2,
               B7.2 = B7^2
          )
        ][
          !is.na(log10_SSC_mgL) & B6 > 268
        ]

#### REGRESSION VARIABLES ####
# Select explanatory variables (regressors) for mulitple regression model
regressors_no_site <- c('B1', 'B2', 'B3', 'B4', 'B5', 'B7', # raw bands
                        'B2.B1', 'B3.B1', 'B4.B1', 'B5.B1', 'B7.B1', # band ratios
                        'B3.B2', 'B4.B2', 'B5.B2', 'B7.B2',
                        'B4.B3', 'B5.B3', 'B7.B3',
                        'B5.B4', 'B7.B4', 'B7.B5')
regressors_all <- c('B1', 'B2', 'B3', 'B4', 'B5', 'B7', # raw bands
                    'B1.2', 'B2.2', 'B3.2', 'B4.2', 'B5.2', 'B7.2', # squared bands
                    'site_no', # no clear way to add categorical variables
                    'B2.B1', 'B3.B1', 'B4.B1', 'B5.B1', 'B7.B1', # band ratios
                    'B3.B2', 'B4.B2', 'B5.B2', 'B7.B2',
                    'B4.B3', 'B5.B3', 'B7.B3',
                    'B5.B4', 'B7.B4', 'B7.B5')

# Select regressors - bands and band ratios
regressors_primary <- c(regressors_no_site, 'B4.B3.B1') # all regressors

##### ESCOLHER A ESTACAO PARA TESTAR OS MODELOS ####

#Renan - Mantendo apenas dados da estacao Pedro Gomes - 66845000, Coxim - 66870000
ls_insitu_raw <- ls_insitu_raw[site_no == 66845000]
ls_raw_1 <- ls_raw_1[site_no == 66845000]
taquari_insitu_raw <- taquari_insitu_raw[site_no == 66845000] 

# Select minimum lead/lag row
setkey(ls_insitu_raw[,abs_lag_days := abs(lag_days)], abs_lag_days)
ls_insitu_raw <- ls_insitu_raw[, .SD[1], .(site_no, sample_date)]

fwrite(ls_insitu_raw,paste0(wd_exports,'ls_insitu_match.csv'))


#### RUN REGRESSION FOR CLUSTERING WITH 1-7 CLUSTERS -- TAKES ~45 MINS ####
# https://en.wikipedia.org/wiki/Color_quantization something to check out
set.seed(1)
site_band_quantiles_all <- ls_raw_1[,':='(B4.B3.B1=B4.B3/B1)]

#Renan - Diminui a qtd de variaveis utilizadas
vis_nir_bands <- c('B1','B2','B3','B4','B2.B1','B3.B1','B4.B1','B3.B2','B4.B2','B4.B3', 'B4.B3.B1')

site_band_scaling_all <- scale(site_band_quantiles_all[,..vis_nir_bands])
cluster_var_combinations <- Map(as.data.frame, sapply(seq_along(vis_nir_bands), function(k) t(combn(vis_nir_bands,k))))

#Renan - Diminui o loop de 4 para 3 para funcionar com a quantidade de variaveis
for(i in 3:length(vis_nir_bands)){
     cluster_var_k_sel <- cluster_var_combinations[[i]]
     for(k in 1:nrow(cluster_var_k_sel)){
          print(paste0(i, " ", k))
          cluster_var_sel <- c(as.matrix(cluster_var_k_sel[k,]))
          
          cluster_var_label <- paste(cluster_var_sel, collapse = "_")
          #Renan - Alterei os parâmetros do nbclust para rodar com a qdt de registros disponiveis min.nc e max.nc
          ccc_result <- data.table(cbind(cluster_var_label, i, c(1:5), NbClust(site_band_scaling_all[,cluster_var_sel],
                                                                               min.nc=2, max.nc=4, index="ccc", method="kmeans")$All.index))
          print(ccc_result)
          colnames(ccc_result) <- c('variables','nvars','nclusters','ccc')
          if(k == 1 & i == 3){
               ccc_master <- ccc_result
          }else{
               ccc_master <- rbind(ccc_master, ccc_result)
          }
          if(k%%100 == 0){
               print(ccc_result)
          }
     }
}

ccc_analysis <- ccc_master[,':='(nvars = as.numeric(nvars),
                                 nclusters = as.numeric(nclusters),
                                 ccc = as.numeric(ccc))]

#Renan - Alterei o parametro da melhor configuracao para o kmeans, para retornar algum dados 
#Renan - Desabilitar o clustering para usar somente a estacao coxim
ccc_best <- ccc_analysis[nclusters == 1 & nvars == 11 ][, .(mean_ccc = mean(ccc, na.rm = T)), by = variables][order(-mean_ccc)]

ccc_plot <- ggplot(ccc_analysis, aes(x = factor(nclusters), y = ccc, color = factor(nvars))) + 
     geom_boxplot() +
     # geom_point() + 
     # scale_color_fivethirtyeight() +
     season_facet + 
     theme(legend.position = 'right') + 
     labs(
          x = 'Number of clusters',
          y = 'Cubic clustering criterion',
          color = 'Number of variables'
     )

ggsave(ccc_plot, filename = paste0(wd_exports,'ccc_optimize_plot.pdf'), width = 7, height = 7)

# Calculate k-means cluster based on all regressors at all sites
# # Using raw band and band ratio values
# Select colors for plotting
cl_colors <- brewer.pal(name = 'Paired',n=12)

# Select variables to use for clustering
# clustering_vars <- c('B1','B4','B2.B1','B3.B1', 'B4.B3.B1')
# Renan - Selecionar variaveis
clustering_vars <- unlist(strsplit(as.character(ccc_best[1,'variables']),'_')) # based on optimal cluster vars from ccc analysis

# clustering_vars <- c('B1','B4','B2.B1','B3.B1')

# Compute number of in situ-landsat pairs per station
# Renan - Removi o filtro abs_lag_days < 3 para retornar algum registros, esse dado esta estranho no tabela
n_insitu_samples_bySite <- ls_insitu_raw[!is.na(log10_SSC_mgL) & abs_lag_days < 9,.(N_insitu_samples = .N), by = .(site_no)]
# Compute band median at each site for clustering variables
# setkey(n_insitu_samples_bySite,site_no)
# Renan -  ls_raw_1 eh o ls_clean
setkey(ls_raw_1,site_no)

write_csv(site_band_quantiles_all, paste0(wd_exports,'all_sts_band_medians.csv'))

#Renan - Diminui a quantidade de testes de 10 para 1 clusters. Aumentar para 2 depois.
## Prepare data for cluster analysis
clusters_calculated_list <- rep(list(NA), 1)
ssc_model_cl_list <- rep(list(NA), 1)
ssc_cluster_color_plot_list <- rep(list(NA), 1)
ssc_cluster_false_color_plot_list <- rep(list(NA), 1)

for(i in c(1:1)){ # test different cluster numbers
     # for(i in 5){ # test different cluster numbers
     
     n_centers <- c(1:1)[i]
     
     cluster_col_name <- paste0('cluster_n',n_centers)
     # Calculate k-means cluster based on all regressors at all sites
     # # Using raw band and band ratio values
     site_band_scaling <- scale(site_band_quantiles_all[,..clustering_vars])
     clusters_calculated <- kmeans(site_band_scaling, centers = n_centers, nstart = 1000, iter.max = 10000)
     
     clusters_calculated_list[[i]] <- clusters_calculated
     # , algorithm = 'MacQueen'
     
     # Compute cluster centers
     cluster_centers <- clusters_calculated$centers
     
     # Assign cluster to each site
     site_band_quantiles_all$cluster <- clusters_calculated$cluster
     
     clustered_sites <- site_band_quantiles_all[,.(landsat_dt,cluster)]
     
     write_csv(site_band_quantiles_all,paste0(wd_exports,'site_band_quantiles_n',i,'.csv'))
     
     
     # TYPICAL RIVER SEDIMENT COLOR AT DIFFERENT CLUSTERS
     
     # Select SSC categories for plotting
     # ssc_categories <- c(0,25,50,100,250,500,750,1000,1500, 1e6)
     ssc_categories <- c(0,50,100,250,500,750,1000)
     # ssc_categories <- c(0,50,100,200,500,1e6)
     # ssc_categories <- c(0,10,25,50,75,100,150,200,250,300,350, 400, 450, 500,600, 700, 800,900,1000,1100,1500, 1e6)
     
     # Generate SSC labels as 'low value' - 'high value'
     ssc_category_labels <- paste0(ssc_categories[-length(ssc_categories)],'-',c(ssc_categories[-1]))
     # Make highest SSC category "> highest value"
     ssc_category_labels[length(ssc_category_labels)] <- paste0('> ', ssc_categories[length(ssc_category_labels)])
     
     ## Add cluster group as column to ls-insitu matched data.table
     ## Renan de match_name para site_no
     setkey(ls_insitu_raw, landsat_dt)
     setkey(clustered_sites, landsat_dt)
     ls_insitu_cl <- ls_insitu_raw[clustered_sites][
          ,':='(cluster_sel = cluster,
                # # Categorize SSC value as one of selected categories
                ssc_category = cut(10^log10_SSC_mgL, 
                                   breaks = ssc_categories,
                                   labels = ssc_category_labels))][]
     # Select cluster for analysis
     
     # # Generate median B,G,R, near-infrared for each SSC category and each cluster or site
     # Renan - Removi o filtro abs_lag_days < 3 para retornar algum registros, esse dado esta estranho no tabela
     
     ssc_category_color <- ls_insitu_cl[abs(lag_days) < 8,
                                        keyby = .(cluster_sel, ssc_category),
                                        lapply(.SD, median,na.rm = T),
                                        .SDcols = c('B1','B2','B3', 'B4')]
     
     # Renan - Removi todos NAS
     ssc_category_color <-  ssc_category_color[!is.na(B1)]
     
     
     # Create true-color and false-color plots of 'typical' river color for each SSC category at each cluster group
     for(j in 1:2){
          color_sel <- c('true_color','false_color')[j]
          # raster_color_types <- c(geom_raster(aes(fill = rgb(B3_median/3000,B2_median/3000,B1_median/3000))), # true color
          #                         geom_raster(aes(fill = rgb(B4_median/4000,B3_median/4000,B2_median/4000))) # false color)
          # )
          # data.table version
          raster_color_types <- c(geom_raster(aes(fill = rgb(B3/3000,B2/3000,B1/3000))), # true color
                                  geom_raster(aes(fill = rgb(B4/4000,B3/4000,B2/4000))) # false color)
          )
          cluster_ssc_category_color_plot <- 
               ggplot(ssc_category_color, aes(x = as.factor(cluster_sel), y = ssc_category)) +
               # ggplot(ssc_category_color, aes(x = reorder(paste0(cluster_sel, ' ', site_no), cluster_sel), y = ssc_category)) + # for by site
               raster_color_types[j] +
               scale_fill_identity() +
               season_facet + 
               # scale_x_continuous(expansion(add = c(0,0))) + 
               # scale_y_discrete(expansion(mult = c(0,0))) +
               # theme(axis.text.x = element_text(angle = 90)) + 
               labs(
                    y = 'SSC range (mg/L)',
                    x = 'River grouping'
               )
          
          ggsave(cluster_ssc_category_color_plot, filename = paste0(wd_figures, cluster_col_name,'_ssc_category_color_plot_',color_sel,'.pdf'),
                 width = 3, height = 3.4, useDingbats = F)
          ggsave(cluster_ssc_category_color_plot, filename = paste0(wd_figures, cluster_col_name,'_ssc_category_color_plot_',color_sel,'.png'),
                 width = 3, height = 3.4)
          if(j == 1){
               ssc_cluster_color_plot_list[[i]] <- cluster_ssc_category_color_plot
          }else{
               ssc_cluster_false_color_plot_list[[i]] <- cluster_ssc_category_color_plot
          }
     }
     
     # Establish a holdout set for testing statistics
     
     ls_insitu_cl <- getHoldout(ls_insitu_cl)
     # Generate calibration model for each cluster
     ssc_model_cl_iterate <- getModels_lasso(ls_insitu_cl[abs_lag_days < 9 & site_no %chin% n_insitu_samples_bySite[
          N_insitu_samples > 1]$site_no],
          #  .SD[sample(x = .N, 
          #               size = min(.N,min(.N, 500)))], 
          #  by = site_no], 
          regressors_all)
     
     ssc_model_cl_list[[i]] <- ssc_model_cl_iterate
     # Predicted values from calibration model
     ssc_model_cl_iterate_pred <- ssc_model_cl_iterate[[1]]
     # Calculate relative error and relative station bias for holdout set using selected cluster model
     # Saves a histogram of all errors
     # Saves a panel boxplot of station bias at each cluster, with each the model type on a different panel
     ssc_model_cl_iterate_rerr_bias <- getErrorBias(ssc_model_cl_iterate_pred, paste0('ssc_model_rerr_', cluster_col_name))
     ssc_model_cl_iterate_rerr <- ssc_model_cl_iterate_rerr_bias[[1]]
     
     # ssc_model_cl_iterate_rmse <- getRMSE(ssc_model_cl_iterate_pred)
     
     # Prepare relative error for plotting
     rel_error_annotate <- data.frame(rel_error = 
                                           paste0('Rel. err = ', 
                                                  round(ssc_model_cl_iterate_rerr[,.(mape_gl_ind,mape_cl_ind,mape_st_ind)],2))[2]) %>% 
          mutate(holdout25 = c('holdout'), SSC_mgL = 1, pred = 39000)
     # Plot actual vs. predicted for holdout. Annotate with RMSE.
     ssc_cluster_iterate_plot_holdout <- get_sscPlot(ssc_model_cl_iterate_pred,"byCluster",'no','no') +
          geom_text(data = rel_error_annotate, 
                    aes(x = SSC_mgL, y = pred, label = rel_error), 
                    hjust = 0,
                    vjust = 0)
     
     # SAVE FIGURE
     # ggsave(ssc_cluster_iterate_plot_holdout, filename = paste0('ssc_', cluster_col_name, '_iterate_plot_holdout.pdf'), useDingbats = F, 
     #        width = 6, height = 7)
     ggsave(ssc_cluster_iterate_plot_holdout, filename = paste0(wd_exports, 'ssc_', cluster_col_name, '_iterate_plot_holdout.pdf'), 
            width = 6, height = 7)
     
     
     # Calculate model statistics
     n_clusters_df <- data.frame(n_clusters = n_centers)
     
     if(i == 1){
          cl_stats <- cbind(data.frame(ssc_model_cl_iterate_rerr),n_clusters_df)
     }else{
          cl_stats <- rbind(cl_stats,
                            cbind(data.frame(ssc_model_cl_iterate_rerr),n_clusters_df))
     }
}
