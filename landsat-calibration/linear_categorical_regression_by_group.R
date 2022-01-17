##### LOAD SOURCES #####
source("../landsat-calibration/libraries.R")
source("../landsat-calibration/config.R")
source("../landsat-calibration/functions.R")
source("../landsat-calibration/data_treatment.R")
source("../landsat-calibration/plot.R")
source("../landsat-calibration/clustering.R")
source("../landsat-calibration/model.R")

#### IMPORT AND CLEAN -- IN SITU DATA ####
insitu_data <- importInSituData()

#### IMPORT STATIONS ####
principais_estacoes <- fread("estacoes_selecionadas_grupos.csv")
principais_estacoes <- principais_estacoes[, ':=' (
  site_no = as.character(Codigo))]
codigos_estacoes <- principais_estacoes$site_no  

# Import landsat spectral data from each site of interest
ls_sr_data <- importLandsatSurfaceReflectanceData()[site_no %chin% codigos_estacoes]

# ggplot()+
#   geom_boxplot(aes(x = as.factor(ls_sr_data$station_nm), y = ls_sr_data$index_montaigner_2014))+
#   theme_clean()

## Identify sites with too few Landsat measurements to be reliable
# Calculate number of satellite samples, per site
n_sat_samples <- ls_sr_data[,.(N_samples = .N), by = .(site_no)]
site_no_n100 <- n_sat_samples[N_samples >= 100, site_no]
ls_sr_data <- ls_sr_data[site_no %chin% site_no_n100 & num_pix > 2 ]

#### JOIN LANDSAT AND IN SITU DATA, WITH LAG OF UP TO 10 DAYS, RESTRICT TO < 3 DAYS ####
## Join Landsat data with in situ data, allowing for as much as a 10-day lead/lag
lag_days <- 6
ls_sr_insitu_data <- joinSRInSituData(ls_sr_data, insitu_data, lagdays)
# Select minimum lead/lag row
setkey(ls_sr_insitu_data[,abs_lag_days := abs(lag_days)], abs_lag_days)
ls_sr_insitu_data <- ls_sr_insitu_data[, 
                        list(index_montaigner_2014 = median(index_montaigner_2014)),
                        .(site_no, sample_date, station_nm, log10_SSC_mgL, ConcentracaoMatSuspensao, sensor)]
n_insitu_samples <- ls_sr_insitu_data[,.(N_samples = .N), by = .(site_no)]

ls_sr_insitu_data <- left_join(ls_sr_insitu_data, principais_estacoes, by = c("site_no" = "site_no"))

ls_sr_insitu_data <- mutate(ls_sr_insitu_data, 
                            station_nm =as.factor(station_nm),
                            site_no =as.factor(site_no),
                            sensor = as.factor(sensor))

dataset_summary <- summarizeDataSet(ls_sr_data, insitu_data, ls_sr_insitu_data)
plotDataSetInsitu(dataset_summary)
plotDataSetImage(dataset_summary)
plotDataSetByStation(ls_sr_insitu_data)

#### Modelo com Validacao Cruzada

# Define training control
set.seed(1)

train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 100)

# Train the model
model_lm <- train(log10_SSC_mgL ~ index_montaigner_2014 + Bacia + sensor, data = ls_sr_insitu_data, 
                  method = "lm", trControl = train.control)

ls_sr_insitu_data$pred <- predict(model_lm, ls_sr_insitu_data)
ls_sr_insitu_data$diff <- ls_sr_insitu_data[, .(diff=abs((pred-log10_SSC_mgL)))]

# Summarize the results
print(model_lm)
print(model_lm$finalModel)
summary(model_lm$finalModel)

plot_summs(model_lm$finalModel, omit.coefs = c("(Intercept)", "index_montaigner_2014"))

r_squared <- paste0('R2= ', round(model_lm$results$Rsquared, 2))
rmse <- paste0('RMSE= ', round(model_lm$results$RMSE, 2))
plotErrorLMOutliers(ls_sr_insitu_data, r_squared, rmse)
plotErrorLM(ls_sr_insitu_data, r_squared, rmse)
# ggplot(ls_sr_insitu_data, aes(x = index_montaigner_2014, y = log10_SSC_mgL)) +
#   geom_point() +
#   stat_smooth()
# effect_plot(model, pred = index_montaigner_2014, interval = TRUE, plot.points = TRUE)

# confint(model)
# 
# ls_sr_data$ssc_prediction<-predict(model, ls_sr_data)
# ls_sr_insitu_data$ssc_prediction<-predict(model, ls_sr_insitu_data)
# 
ls_sr_data$ssc_prediction <- predict(model_lm, ls_sr_data)
landsat_serie <- getLandsatHistoricalSerie(ls_sr_data)

generatePredictedHistoricalSerieByStation(landsat_serie)

