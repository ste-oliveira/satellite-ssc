##### LOAD SOURCES #####
source("../landsat-calibration/libraries.R")
source("../landsat-calibration/config.R")
source("../landsat-calibration/functions.R")
source("../landsat-calibration/data_treatment.R")
source("../landsat-calibration/plot.R")
source("../landsat-calibration/clustering.R")
source("../landsat-calibration/model.R")

#### IMPORT AND CLEAN -- LANDSAT DATA ####
set.seed(1)

# Import landsat spectral data from each site of interest
ls_sr_data <- importLandsatSurfaceReflectanceData()

## Identify sites with too few Landsat measurements to be reliable

# Calculate number of satellite samples, per site
n_sat_samples <- ls_sr_data[,.(N_samples = .N), by = .(station_nm)]

#### IMPORT AND CLEAN -- IN SITU DATA ####
insitu_data <- importInSituData()

#Renan - Mantendo apenas dados da estacao Pedro Gomes - 66845000
#taquari_insitu_raw <- taquari_insitu_raw[site_no == 66845000] 
insitu_data_site_nos <- unique(insitu_data[!is.na(site_no),site_no])

#### JOIN LANDSAT AND IN SITU DATA, WITH LAG OF UP TO 10 DAYS, RESTRICT TO < 3 DAYS ####
## Join Landsat data with in situ data, allowing for as much as a 10-day lead/lag
# Join Landsat data with in situ data
lag_days <- 8
ls_sr_insitu_data <- joinSRInSituData(ls_sr_data, insitu_data, lagdays)

##### ESCOLHER A ESTACAO PARA TESTAR OS MODELOS ####
# # #Renan - Mantendo apenas dados da estacao Pedro Gomes - 66845000, Coxim - 66870000
# insitu_data <- insitu_data[(site_no == 66845000 | site_no == 66870000 | site_no == 66855000)]
# ls_sr_data <- ls_sr_data[(site_no == 66845000 | site_no == 66870000 | site_no == 66855000)]
# ls_sr_insitu_data <- ls_sr_insitu_data[(site_no == 66845000 | site_no == 66870000 | site_no == 66855000)]
# # ls_sr_data <- ls_sr_data[( site_no == 66845000)]
# insitu_data <- insitu_data[( site_no == 66845000)]
# ls_sr_insitu_data <- ls_sr_insitu_data[(site_no == 66845000)]

# Select minimum lead/lag row
setkey(ls_sr_insitu_data[,abs_lag_days := abs(lag_days)], abs_lag_days)
ls_sr_insitu_data <- ls_sr_insitu_data[, .SD[1], .(site_no, sample_date)]

dataset_summary <- summarizeDataSet()
plotDataSet(dataset_summary)

#### RUN REGRESSION FOR CLUSTERING WITH 1-7 CLUSTERS -- TAKES ~45 MINS ####
# https://en.wikipedia.org/wiki/Color_quantization something to check out
set.seed(1)

site_band_quantiles_all <- ls_sr_data[,':='(B4.B3.B1=B4.B3/B1)]

#Segundo e terceiro parametros sao respectivamente limite minimo de clusters e limite max de variaves
clustering_vars <- runClusterAnalysis(site_band_quantiles_all, 1, 11)

# clustering_vars <- c('B1','B4','B2.B1','B3.B1')
# Compute number of in situ-landsat pairs per station
# Renan - Removi o filtro abs_lag_days < 3 para retornar algum registros, esse dado esta estranho no tabela
n_insitu_samples_bySite <- ls_sr_insitu_data[!is.na(log10_SSC_mgL) & abs_lag_days <= 8,.(N_insitu_samples = .N), by = .(site_no)]

# Compute band median at each site for clustering variables
# setkey(n_insitu_samples_bySite,site_no)
# Renan -  ls_raw_1 eh o ls_clean
setkey(ls_sr_data,site_no)

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
  
  clustered_sites <- runKMeansCluster(site_band_quantiles_all, clustering_vars, n_centers)
  
  ## Add cluster group as column to ls-insitu matched data.table
  ## Renan de match_name para site_no
  setkey(ls_sr_insitu_data, landsat_dt)
  #setkey(clustered_sites, landsat_dt)
  #TODO Renan - Fixando 1 cluster
  ls_insitu_cl <- ls_sr_insitu_data[
      ,':='(cluster_sel = 1,
            # # Categorize SSC value as one of selected categories
            ssc_category = cut(10^log10_SSC_mgL, 
                               breaks = ssc_categories,
                               labels = ssc_category_labels))][]
  
  # Select cluster for analysis
  # # Generate median B,G,R, near-infrared for each SSC category and each cluster or site
  # Renan - Removi o filtro abs_lag_days < 3 para retornar algum registros, esse dado esta estranho no tabela
  ssc_category_color <- ls_insitu_cl[abs(lag_days) <= 8,
                                    keyby = .(cluster_sel, ssc_category),
                                    lapply(.SD, median,na.rm = T),
                                    .SDcols = c('B1','B2','B3', 'B4')]
  
  # Renan - Removi todos NAS
  ssc_category_color <-  ssc_category_color[!is.na(B1)]
  plotClusterSSCCategoryColor(ssc_category_color)
  
  # TODO Establish a holdout set for testing statistics
  ls_insitu_cl <- getHoldout(ls_insitu_cl)
  
  # Generate calibration model for each cluster
  ssc_model_cl_iterate <- getModels_lasso(ls_insitu_cl[abs_lag_days <= 8 & site_no %chin% n_insitu_samples_bySite[
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
  plotRelativeError(ssc_model_cl_iterate_rerr) 
     
  #Removendo imagens sobrepostas do landsat 5 e landsat 7
  landsat_serie <- getLandsatHistoricalSerie(ls_sr_data, insitu_data_site_nos)
  ssc_model <- ssc_model_cl_iterate[2][[1]][[1]]
  
  #TODO - Corrigir função de predição 
  #predictedSSC <- predictSSC(ssc_model, regressors_all)
  regressors_sel <- regressors_all[-which(regressors_all == 'site_no')]
  matrix <- data.matrix(landsat_serie[,..regressors_sel])
  predictedSSC <- cbind(landsat_serie, predict=predict(object=ssc_model, newx = matrix,  s = "lambda.min", type="response"))
  
  generatePredictedHistoricalSerieByStation(predictedSSC, landsat_serie)
  
  # Calculate model statistics
  n_clusters_df <- data.frame(n_clusters = n_centers)
  
  if(i == 1){
      cl_stats <- cbind(data.frame(ssc_model_cl_iterate_rerr), n_clusters_df)
  }else{
      cl_stats <- rbind(cl_stats,
                        cbind(data.frame(ssc_model_cl_iterate_rerr),n_clusters_df))
  }
}

