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

#### IMPORT AND CLEAN -- IN SITU DATA ####
insitu_data <- importInSituData()

principais_estacoes <- fread("principais_estacoes.csv")
principais_estacoes <- principais_estacoes[, ':=' (
  site_no = as.character(site_no))]$site_no
  
#Renan - Mantendo apenas dados da estacao Pedro Gomes - 66845000
#taquari_insitu_raw <- taquari_insitu_raw[site_no == 66845000] 
insitu_data_site_nos <- unique(insitu_data[insitu_data$EstacaoCodigo  %in% principais_estacoes]$site_no)

# Import landsat spectral data from each site of interest
ls_sr_data <- importLandsatSurfaceReflectanceData()[site_no %chin% insitu_data_site_nos]

ls_sr_data <- ls_sr_data[,  ':='(
                        ndssi = (ls_sr_data$B4-ls_sr_data$B1)/(ls_sr_data$B4+ls_sr_data$B1),
                        nsmi = (ls_sr_data$B3 + ls_sr_data$B2 - ls_sr_data$B1)/(ls_sr_data$B3 +ls_sr_data$B2 + ls_sr_data$B1),
                        index_montaigner_2014 = (ls_sr_data$B4+ ls_sr_data$B3)/(ls_sr_data$B2 + ls_sr_data$B1))]

ggplot()+
  geom_boxplot(aes(x = as.factor(ls_sr_data$station_nm), y = ls_sr_data$index_montaigner_2014))+
  theme_clean()

## Identify sites with too few Landsat measurements to be reliable
# Calculate number of satellite samples, per site
n_sat_samples <- ls_sr_data[,.(N_samples = .N), by = .(site_no)]
site_no_n100 <- n_sat_samples[N_samples >= 100, site_no]
ls_sr_data <- ls_sr_data[site_no %chin% site_no_n100 & num_pix >2 ]

#### JOIN LANDSAT AND IN SITU DATA, WITH LAG OF UP TO 10 DAYS, RESTRICT TO < 3 DAYS ####
## Join Landsat data with in situ data, allowing for as much as a 10-day lead/lag
lag_days <- 3
ls_sr_insitu_data <- joinSRInSituData(ls_sr_data, insitu_data, lagdays)

write_csv(ls_sr_insitu_data, paste0(wd_exports,'ls_sr_insitu_data.csv'))


# Select minimum lead/lag row
setkey(ls_sr_insitu_data[,abs_lag_days := abs(lag_days)], abs_lag_days)
ls_sr_insitu_data <- ls_sr_insitu_data[, .SD[1], .(site_no, sample_date)]

dataset_summary <- summarizeDataSet()
plotDataSet(dataset_summary)

#### RUN REGRESSION FOR CLUSTERING WITH 1-7 CLUSTERS -- TAKES ~45 MINS ####
# https://en.wikipedia.org/wiki/Color_quantization something to check out
set.seed(1)

# site_band_quantiles_all <- ls_sr_data[,':='(B4.B3.B1=B4.B3/B1)]
site_band_quantiles_all <- ls_sr_data[
  ,.(N_samples = .N,
     B1 = median(B1),
     B2 = median(B2),
     B3 = median(B3)
     # B4 = median(B4),
     # B5 = median(B5),
     # B7 = median(B7),
     # B2.B1 = median(B2.B1),
     # B3.B1 = median(B3.B1),
     # B4.B1 = median(B4.B1),
     # B3.B2 = median(B3.B2),
     # B4.B2 = median(B4.B2),
     # B4.B3 = median(B4.B3),
     # B4.B3.B1 = median(B4.B3/B1)
     ),
  by = .(station_nm,site_no, Latitude, Longitude)]


#Segundo e terceiro parametros sao respectivamente limite minimo de clusters e limite max de variaves
ccc_analysis <- runClusterAnalysis(site_band_quantiles_all, 2, 3)

#https://support.sas.com/documentation/onlinedoc/v82/techreport_a108.pdf
#ccc_best <- ccc_analysis[nclusters == clusters & nvars < vars][, .(mean_ccc = mean(ccc, na.rm = T)), by = variables][order(-mean_ccc)]
ccc_best <- ccc_analysis[nclusters == 3 & nvars > 2][, .(mean_ccc = mean(ccc, na.rm = T)), by = variables][order(-mean_ccc)]

### Nao precisamos desse grafico por enquanto
ccc_plot <- ggplot(ccc_analysis, aes(x = factor(nclusters), y = ccc, color = factor(nvars))) +
  geom_boxplot() +
  # geom_point() +
  # scale_color_fivethirtyeight() +
  theme(legend.position = 'right') +
  theme_clean() + 
  labs(
    x = 'Quantidade de Clusters',
    y = 'Cubic Clustering Criterion',
    color = 'Quantidade de Variáveis'
  )

plot(ccc_plot)
# ggsave(ccc_plot, filename = paste0(wd_exports,'ccc_optimize_plot.png'), width = 7, height = 7)

# Calculate k-means cluster based on all regressors at all sites
# # Using raw band and band ratio values
# Select colors for plotting
#cl_colors <- brewer.pal(name = 'Paired',n=12)

# Select variables to use for clustering
# Renan - Selecionar variaveis
clustering_vars <- unlist(strsplit(as.character(ccc_analysis[12,'variables']),'_')) # based on optimal cluster vars from ccc analysis


# Compute number of in situ-landsat pairs per station
# Renan - Removi o filtro abs_lag_days < 3 para retornar algum registros, esse dado esta estranho no tabela
n_insitu_samples_bySite <- ls_sr_insitu_data[!is.na(log10_SSC_mgL) & abs_lag_days <= 3,.(N_insitu_samples = .N), by = .(site_no)]

# Compute band median at each site for clustering variables
setkey(ls_sr_data,site_no)

write_csv(site_band_quantiles_all, paste0(wd_exports,'all_sts_band_medians.csv'))

#Renan - Diminui a quantidade de testes de 10 para 1 clusters. Aumentar para 2 depois.
## Prepare data for cluster analysis
clusters_calculated_list <- rep(list(NA), 5)
ssc_model_cl_list <- rep(list(NA), 5)
ssc_cluster_color_plot_list <- rep(list(NA), 5)
ssc_cluster_false_color_plot_list <- rep(list(NA), 5)

for(i in c(1:5)){ # test different cluster numbers
  # for(i in 5){ # test different cluster numbers
  
  n_centers <- c(1:10)[i]
  cluster_col_name <- paste0('cluster_n',n_centers)
  
  clustered_sites <- runKMeansCluster(site_band_quantiles_all, clustering_vars, n_centers)
  
  ## Add cluster group as column to ls-insitu matched data.table
  setkey(ls_sr_insitu_data, site_no)
  setkey(clustered_sites, site_no)
  #TODO Renan - Fixando 1 cluster
  ls_insitu_cl <- ls_sr_insitu_data[clustered_sites][
    ,':='(cluster_sel = cluster,
          # # Categorize SSC value as one of selected categories
          ssc_category = cut(10^log10_SSC_mgL, 
          breaks = ssc_categories,
          labels = ssc_category_labels))][]
  
  # Select cluster for analysis
  # # Generate median B,G,R, near-infrared for each SSC category and each cluster or site
  # Renan - Removi o filtro abs_lag_days < 3 para retornar algum registros, esse dado esta estranho no tabela
  ssc_category_color <- ls_insitu_cl[abs(lag_days) <= 3,
                                    keyby = .(cluster_sel, ssc_category),
                                    lapply(.SD, median,na.rm = T),
                                    .SDcols = c('B1','B2','B3', 'B4')]
  
  # Renan - Removi todos NAS
  ssc_category_color <-  ssc_category_color[!is.na(B1)]
  plotClusterSSCCategoryColor(ssc_category_color)
  
  # TODO Establish a holdout set for testing statistics
  ls_insitu_cl <- getHoldout(ls_insitu_cl)
  
  # Generate calibration model for each cluster
  ssc_model_cl_iterate <- getModels_lasso(
    ls_insitu_cl[abs_lag_days <= 3 & site_no %chin% n_insitu_samples_bySite[N_insitu_samples > 5]$site_no], regressors_all)
  
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
 
  # Calculate model statistics
  n_clusters_df <- data.frame(n_clusters = n_centers)
  
  if(i == 1){
      cl_stats <- cbind(data.frame(ssc_model_cl_iterate_rerr), n_clusters_df)
  }else{
      cl_stats <- rbind(cl_stats,
                        cbind(data.frame(ssc_model_cl_iterate_rerr),n_clusters_df))
  }
}

#### CLUSTER STATS -- COMPUTE AFTER INITIAL RUN OF MULTIPLE CLUSTERS, GENERATE FUNCTION FOR ID-ING CLUSTER ####
colnames(cl_stats) <- c('mape_gl_ind','mape_cl_ind','mape_st_ind',
                        'bias_gl','bias_cl','bias_st',
                        'n_clusters')
# SAVE TABLE
# If you don't want to save prior results to Environment
# write_csv(cl_stats, path = paste0(wd_exports,'ssc_cluster_statistics_nclusters.csv'))
# cl_stats <- read_csv('ssc_cluster_statistics_nclusters.csv')

cl_stats_melt <- cl_stats %>% melt(measure.vars = c('bias_cl','mape_cl_ind')) %>% 
  mutate(error_term = factor(variable, 
                             levels = c('bias_cl','mape_cl_ind'),                  
                             labels = c('Median rel. station bias','Median rel. error'),
                             ordered = T))
cluster_optimize_plot <- ggplot(cl_stats_melt, 
                                aes(x = n_clusters, y = value, group = error_term, color = error_term)) +
  scale_color_manual(values = c('#F26E50','#405173')) +
  geom_point() + 
  geom_line() + 
  scale_y_continuous(limits = c(0, 1)) + 
  scale_x_continuous(limits = c(1, 10), breaks = c(1:10)) +
  #season_facet +
  theme_clean()+
  theme(legend.position = c(0.8,0.8)) +
  labs(
    x = 'Number of cluster groups',
    y = 'Median rel. station bias, Median rel. error',
    color = ''
  )

plot(cluster_optimize_plot)

# SAVE FIGURE
ggsave(cluster_optimize_plot, filename = paste0(wd_exports,'cluster_optimize_plot.pdf'), width = 4, height = 4, useDingbats = F)

#### SELECT MODEL & MAKE ERROR AND UNCERTAINTY CALCULATIONS ####
# Select number of clusters based on best fit
cluster_n_best <- 3
ssc_model_cl_sel <- ssc_model_cl_list[[cluster_n_best]]

i<-0

###Predicao####
for(cluster_model in ssc_model_cl_sel[2]){
  i<-i+1
  bind <-select(data.frame(ssc_model_cl_sel[1]),site_no ,cluster_sel)
  ls_sr_data_cluster <-
    left_join(data.frame(ls_sr_data), bind, by=c("site_no"))
  ls_sr_data_cluster<-ls_sr_data_cluster[ls_sr_data_cluster$cluster_sel == i,]
  
  landsat_serie <- getLandsatHistoricalSerie(ls_sr_data_cluster)
  ssc_model <- cluster_model[[i]][[1]]
  
  #TODO - Corrigir função de predição
  #predictedSSC <- predictSSC(ssc_model, regressors_all)
  regressors_sel <- regressors_all[-which(regressors_all == 'site_no')]
  matrix <- data.matrix(landsat_serie[,regressors_sel])
  predictedSSC <- cbind(landsat_serie, predict=predict(object=ssc_model, newx = matrix,  s = "lambda.min", type="response"))
  
  generatePredictedHistoricalSerieByStation(predictedSSC, landsat_serie)
  
}


# Get SSC prediction for all in situ data
ssc_model_cl_iterate_pred <- ssc_model_cl_sel[[1]]
# ssc_pred_all <- ssc_model_cl_sel[[1]]
# Get prediction function
ssc_cluster_funs <- ssc_model_cl_sel[[2]]

# Export table of function coefficients
for(n in 1:length(ssc_cluster_funs)){
  cv.opt <- coef(ssc_cluster_funs[[n]], s = "lambda.min")
  coef_ex <- cbind(rownames(cv.opt),as.numeric(cv.opt))
  colnames(coef_ex) <- c('variable', 'value')
  
  write.table(coef_ex, sep = ",", file = paste0(wd_exports,'cluster_selected_cl', n, '_lasso_fit_coeff.csv'), row.names = F)
}

# Get table of matched in situ-landsat with cluster

# Make cluster parallel plots
# Get median band values per cluster
cluster_band_medians <- ssc_model_cl_iterate_pred[, lapply(.SD, median, na.rm=TRUE), 
                                                  by=.(cluster, ssc_category), .SDcols=paste0('B',c(1:5,7))][
                                                    ssc_category %in% c('0-50','100-250','250-500')
                                                  ]
cluster_parallel_plot_avg <- ggplot(reshape2::melt(cluster_band_medians, measure.vars = paste0('B',c(1:5,7))), 
                                    aes(x = variable, y = value/10000, color = as.factor(cluster), 
                                        group = ssc_category, linetype = paste0(ssc_category, ' mg/L'))) + 
  geom_line(size = 0.25) + 
  geom_point(pch = 21, stroke = 0.25) +
  scale_color_brewer(palette = 'Paired') +
  scale_linetype_manual(values=c("dashed", 'dotdash', 'longdash')) +
  facet_wrap(.~paste0('Cluster ',cluster), ncol = 2) +
  #season_facet +
  theme_clean()+
  theme(legend.position = c(0.37, 0.97), legend.background = element_blank()) +
  guides(color = F, linetype = guide_legend(title = element_blank(), 
                                            nrow = 3, keyheight = 0.2, keywidth = 0.7, reverse = T)) +
  labs(
    x = '',
    y = 'Refletância', 
    color = 'Tipo de R',
    linetype = 'SSC range (mg/L)'
  )

plot(cluster_parallel_plot_avg)

ggsave(cluster_parallel_plot_avg, filename = paste0(wd_figures,'cluster_parallel_plot_avg.pdf'), width = 4, height = 4, useDingbats = F)

# Calculate relative error
ssc_model_all_errorbias <- getErrorBias(ssc_model_cl_iterate_pred, 'all_models')

# Save table of match in situ-landsat observations (with cluster)
write_csv(ssc_model_cl_iterate_pred, path = paste0(wd_exports,'ls_insitu_wCluster_sel.csv'))

# Calculate station bias
setkey(n_insitu_samples_bySite,site_no)
setkey(ssc_model_cl_iterate_pred,site_no)
station_bias <- ssc_model_cl_iterate_pred[
  n_insitu_samples_bySite][
    # N_samples > 12
  ] %>% 
  group_by(cluster_sel, site_no) %>% 
  summarise_at(c('log10_SSC_mgL', 'pred_gl','pred_cl','pred_st'), mean.geometric, na.rm = T) %>%
  mutate(
    bias_gl = (10^median(abs(log10(10^pred_gl/10^log10_SSC_mgL)), na.rm = T)-1),
    bias_cl = (10^median(abs(log10(10^pred_cl/10^log10_SSC_mgL)), na.rm = T)-1),
    bias_st = (10^median(abs(log10(10^pred_st/10^log10_SSC_mgL)), na.rm = T)-1))

station_bias_byCluster_density_plot <- ggplot(setDT(station_bias)[!is.na(cluster_sel)][,
                                                                                       cluster_label:=paste0('Cluster ', cluster_sel)], 
                                              aes(x = pred_cl - log10_SSC_mgL)) + 
  geom_vline(aes(xintercept = pred_cl - log10_SSC_mgL), size = 0.1) +
  geom_vline(xintercept = 0, lty = 'dashed',size = 0.75, color = 'orange') +
  geom_density(color = 'red') +
  facet_wrap(.~cluster_label, nrow = 1) + 
  # season_facet + 
  theme_clean()+
  scale_x_continuous(limits = c(-2, 2)) +
  scale_y_continuous(limits = c(0, 4), expand = expansion(add = c(0,0))) + 
  labs(x = 'Avg. model residual, by station')

plot(station_bias_byCluster_density_plot)
ggsave(station_bias_byCluster_density_plot, filename = paste0(wd_exports, 'station_bias_density_all_plots.pdf'), width = 5, height = 6, useDingbats = F)

numpix_vs_error_plot <- ggplot(ssc_model_cl_iterate_pred[!is.na(num_pix)], 
                               aes(x = cut(as.numeric(num_pix), 
                                           breaks = c(0,1,5,10,20,50,100,1e5),
                                           labels = c('1','2-5','6-10','11-20','21-50','50-100','>100')), 
                                   y = abs(pred_cl - log10_SSC_mgL))) + 
  # geom_point(alpha = 0.2) + 
  geom_boxplot(outlier.shape = NA) +
  # geom_smooth(lty = 'dashed', size = 0.75, color = 'orange') + 
  # season_facet + 
  theme_clean()+
  geom_text(data = ssc_model_cl_iterate_pred[!is.na(num_pix), .(N_samples = .N), 
                                             by = cut(as.numeric(num_pix), 
                                                      breaks = c(0,1,5,10,20,50,100,1e5),
                                                      labels = c('1','2-5','6-10','11-20','21-50','50-100','>100'))],
            aes(x = cut, y = 1.5,label = paste0('N =\n',N_samples)), size = 3, hjust = 0.5) +
  scale_y_continuous(limits = c(0,1.65)) +
  # scale_x_log10(labels = fancy_scientific) + 
  labs(x = 'Number of pixels sampled',
       y = 'Model error \nAbs(Log10[predicted SSC] - Log10[in situ SSC])')
plot(numpix_vs_error_plot)

ggsave(numpix_vs_error_plot, filename = paste0(wd_figures,'numpix_vs_error_ncl5.pdf'), 
       width = 5, height = 5, useDingbats = F)

cloud_cover_vs_error_plot <- ggplot(ssc_model_cl_iterate_pred, 
                                    aes(x = cloud_cover, y = abs(pred_cl - log10_SSC_mgL))) + 
  geom_point(alpha = 0.1) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) + 
  theme_clean()+
  labs(x = 'Cloud cover (%)',
       y = 'Abs(Model error) \n(|Log10[predicted SSC] - Log10[in situ SSC]|)')
plot(cloud_cover_vs_error_plot)

ggsave(cloud_cover_vs_error_plot, filename = paste0(wd_exports,'cloud_cover_vs_error_ncl5.pdf'), 
       width = 5, height = 5, useDingbats = F)

cloud_pix_perc_vs_error_plot <- ggplot(ssc_model_cl_iterate_pred, 
                                       aes(x = cloud_qa_count/num_pix, 
                                           y = abs(pred_cl - log10_SSC_mgL))) + 
  geom_point(alpha = 0.1) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) + 
  theme_clean()+
  labs(x = 'Local cloud cover ratio',
       y = 'Abs(Model error) \n(|Log10[predicted SSC] - Log10[in situ SSC]|)')

plot(cloud_pix_perc_vs_error_plot)

ggsave(cloud_pix_perc_vs_error_plot, filename = paste0(wd_exports,'cloud_pix_perc_vs_error_ncl5.pdf'), 
       width = 5, height = 5, useDingbats = F)

