
################## SCC TAQUARI ###############

##### LOAD SOURCES #####
source("libraries.R")
source("config.R")
source("database.R")
source("plot.R")

#### 1. IMPORT AND CLEAN -- IN SITU DATA ####
insitu_data <- importInSituData()
#View(insitu_data)
#str(insitu_data)

#### 2. IMPORT STATIONS ####
principais_estacoes <- fread("estacoes_selecionadas_grupos_taquari.csv")
principais_estacoes <- principais_estacoes[, ':=' (
  site_no = as.character(Codigo))]
codigos_estacoes <- principais_estacoes$site_no  

#### 3. IMPORT LANDSAT SPECTRAL DATA FROM EACH SITE OF INTEREST ####
ls_sr_data <- importLandsatSurfaceReflectanceData()[site_no %chin% codigos_estacoes]
#ls_sr_data$landsat_dt <- ymd(ls_sr_data$landsat_dt)
#View(ls_sr_data)
#str(ls_sr_data)

ggplot()+
  geom_boxplot(aes(x = as.factor(ls_sr_data$station_nm), y = ls_sr_data$index_montaigner_2014))+
   theme_clean()

#### 4. IDENTIFY SITES WITH TOO FEW LANDSAT MEASUREMENTS TO BE RELIABLE ####
# Calculate number of satellite samples, per site
n_sat_samples <- ls_sr_data[,.(N_samples = .N), by = .(site_no)]
site_no_n100 <- n_sat_samples[N_samples >= 30, site_no]
ls_sr_data <- ls_sr_data[site_no %chin% site_no_n100 & num_pix > 2 ]
str(ls_sr_data)

#### 5. JOIN LANDSAT AND IN SITU DATA, WITH LAG OF UP TO 10 DAYS, RESTRICT TO < 3 DAYS ####
## Join Landsat data with in situ data, allowing for as much as a 10-day lead/lag
lag_days <- 3
ls_sr_insitu_data <- joinSRInSituData(ls_sr_data, insitu_data, lagdays)
# Select minimum lead/lag row
setkey(ls_sr_insitu_data[,abs_lag_days := abs(lag_days)], abs_lag_days)
ls_sr_insitu_data <- ls_sr_insitu_data[, 
                                       list(index_montaigner_2014 = median(index_montaigner_2014)),
                                       .(site_no, sample_date, station_nm, log10_SSC_mgL, ConcentracaoMatSuspensao, sensor)]
n_insitu_samples <- ls_sr_insitu_data[,.(N_samples = .N), by = .(site_no)]

ls_sr_insitu_data <- left_join(ls_sr_insitu_data, principais_estacoes, by = c("site_no" = "site_no"))

ls_sr_insitu_data <- mutate(ls_sr_insitu_data, 
                            station_nm = as.factor(station_nm),
                            site_no = as.factor(site_no),
                            sensor = as.factor(sensor))

#View(ls_sr_insitu_data)
dataset_summary <- summarizeDataSet(ls_sr_data, insitu_data, ls_sr_insitu_data)

# Save satellite images/site histogram
#ggsave(n_sat_samples_histogram, filename = paste0(wd_figures,'n_sat_samples_histogram.pdf'), width = 4, height = 4, useDingbats = F)
#plot(n_sat_samples_histogram)
plotDataSetInsitu1(dataset_summary)
plotDataSetImage(dataset_summary)
#plotDataSetByStation(ls_sr_insitu_data) ### esta igual o de cima

ggplot()+
  geom_boxplot(aes(x = as.factor(ls_sr_insitu_data$station_nm), y = ls_sr_insitu_data$log10_SSC_mgL))+
  theme_clean()

#### 6. CROSS VALIDATION MODEL ####

# 6.1. Define training control
set.seed(1)

#executar duas vezes
#ls_sr_insitu_data = ls_sr_insitu_data[diff<0.79]

train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 100)

# 6.2. Train the model
model_lm <- train(log10_SSC_mgL ~ index_montaigner_2014 + sensor, data = ls_sr_insitu_data, 
                  method = "lm", trControl = train.control)

extract_eq(model_lm$finalModel, use_coefs = TRUE)

ls_sr_insitu_data$pred <- predict(model_lm, ls_sr_insitu_data)
ls_sr_insitu_data$diff <- ls_sr_insitu_data[, .(diff=abs((pred-log10_SSC_mgL)))]

r_squared <- paste0('R2= ', round(R2_Score(ls_sr_insitu_data$pred, ls_sr_insitu_data$log10_SSC_mgL), 2))
rmse <- paste0('RMSE= ', round(model_lm$results$RMSE, 2))

plotErrorLMOutliers(ls_sr_insitu_data, r_squared, rmse)
plotErrorLM(ls_sr_insitu_data, r_squared, rmse)

#voltar set.seed

# 6.3. Summarize the results
print(model_lm)
print(model_lm$finalModel)
summary(model_lm$finalModel)

plot_summs(model_lm$finalModel, omit.coefs = c("(Intercept)", "index_montaigner_2014"))

r_squared <- paste0('R2= ', round(model_lm$results$Rsquared, 2))
rmse <- paste0('RMSE= ', round(model_lm$results$RMSE, 2))


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

max_ssc_prediction_by_year <- landsat_serie %>%
  group_by(year(ymd(landsat_serie$landsat_dt)), site_no) %>%
  slice(which.max(ssc_prediction))

vazao_data <- na.omit(fread('imports/vazao/VAZOES_ESTACOES.csv'))[EstacaoCodigo==66870000,]
vazao_data <- mutate(vazao_data, 
                     EstacaoCodigo=as.character(EstacaoCodigo), 
                     month=month(dmy(Data)),
                     year=year(dmy(Data)),
                     Data=dmy(Data),
                     Media = as.numeric(Media)) %>% distinct()


landsat_serie_mensal <- mutate(landsat_serie,
                               landsat_dt_month=month(landsat_dt),
                               landsat_dt_year=year(landsat_dt)) %>%
  group_by(landsat_dt_month, landsat_dt_year, site_no, station_nm) %>%
  summarise(media_ssc_prediction=mean(ssc_prediction), landsat_dt=first(landsat_dt))


#### Decomposicao Serie Temporal Vazao ####
hist(vazao_data$Media, breaks=30)
vazao_data_ordenada <- na.omit(vazao_data[order(vazao_data$Data),])
vazao_ts <- na.remove(ts(vazao_data$Media, frequency=12, start=c(1982)))
vazao_serie_decomposta <- decompose(vazao_ts, type="additive")
vazao_serie_decomposta <- data.frame(cbind(vazao_serie_decomposta$trend, vazao_data_ordenada$Data))
colnames(vazao_serie_decomposta) <- c("trend", "data")
vazao_serie_decomposta <- mutate(vazao_serie_decomposta,
                                 data = as.Date(data))

MK = MannKendall(vazao_ts)
summary(MK)
sens.slope(vazao_ts)
pettitt.test(vazao_ts)


#### Decomposicao Serie Temporal CSS ####
hist(10^landsat_serie_mensal$media_ssc_prediction, breaks=30)
landsat_serie_mensal_ordenada <- na.omit(landsat_serie_mensal[order(landsat_serie_mensal$landsat_dt),])
landsat_serie_mensal_ordenada_ts <- na.remove(ts(10^landsat_serie_mensal$media_ssc_prediction, frequency=12, start=c(1984)))
landsat_serie_decomposta <- decompose(landsat_serie_mensal_ordenada_ts, type="additive")
landsat_serie_decomposta <- data.frame(cbind(landsat_serie_decomposta$trend, landsat_serie_mensal_ordenada$landsat_dt))
colnames(landsat_serie_decomposta) <- c("trend", "landsat_dt")
landsat_serie_decomposta <- mutate(landsat_serie_decomposta,
                                   landsat_dt = as.Date(landsat_dt))


MK = MannKendall(landsat_serie_mensal_ordenada_ts)
summary(MK)
sens.slope(landsat_serie_mensal_ordenada_ts)
pettitt.test(landsat_serie_mensal_ordenada_ts)


#### Serie Historia CSS e Vazao ####

generatePredictedHistoricalSerieByStation(landsat_serie, vazao_data, vazao_serie_decomposta, landsat_serie_decomposta)




