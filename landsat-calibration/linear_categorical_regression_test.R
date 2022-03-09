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
                                       list(
                                          B3.B1 = median(B3.B1),
                                          B3.B2 = median(B3.B2),
                                          B4.B3 = median(B4.B3),
                                          B4.B2 = median(B4.B2),
                                          B3.B2.B1 = median(B3.B2.B1),
                                          nsmi = median(nsmi),
                                          ndssi = median(ndssi),
                                          index_montaigner_2014 = median(index_montaigner_2014),
                                          B2.B1 = median(B2.B1)),
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
ls_sr_insitu_data = ls_sr_insitu_data[diff<0.79]

train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 100)

# Train the model
model_lm <- train(log10_SSC_mgL ~ B3.B2.B1 + site_no + sensor, data = ls_sr_insitu_data, 
                  method = "lm", trControl = train.control)

ls_sr_insitu_data$pred <- predict(model_lm, ls_sr_insitu_data)
ls_sr_insitu_data$diff <- ls_sr_insitu_data[, .(diff=abs((pred-log10_SSC_mgL)))]

r_squared <- paste0('R2= ', round(model_lm$results$Rsquared, 2))
rmse <- paste0('RMSE= ', round(model_lm$results$RMSE, 2))

plotErrorLMOutliers(ls_sr_insitu_data, r_squared, rmse)
plotErrorLM(ls_sr_insitu_data, r_squared, rmse)

# Summarize the results
print(model_lm)

print(model_lm$finalModel)

summary(model_lm)

plot_summs(model_lm$finalModel, omit.coefs = c("(Intercept)", "index_montaigner_2014"))

ls_sr_data$ssc_prediction <- predict(model_lm, ls_sr_data)

landsat_serie <- getLandsatHistoricalSerie(ls_sr_data)

max_ssc_prediction_by_year <- landsat_serie %>%
   group_by(year(ymd(landsat_serie$landsat_dt)), site_no) %>%
   slice(which.max(ssc_prediction))

vazao_data <- na.omit(fread('vazao/VAZOES_ESTACOES.csv'))
vazao_data <- mutate(vazao_data, 
                     EstacaoCodigo=as.character(EstacaoCodigo), 
                     month=month(dmy(Data)),
                     year=year(dmy(Data)),
                     Data=dmy(Data),
                     Media = as.numeric(Media)) %>% distinct()

# Seleciona
landsat_serie <- landsat_serie[site_no==66255000]
vazao_data <- vazao_data[EstacaoCodigo == 66255000]

# historical_data<- left_join(landsat_serie, cota_data, by=c('site_no'='EstacaoCodigo', 'landsat_dt' ='Data'))
generatePredictedHistoricalSerieByStation(landsat_serie, vazao_data,max_ssc_prediction_by_year)

### Rela??o vaz?o X css
landsat_serie_mensal <- mutate(landsat_serie,
                        landsat_dt_month=month(landsat_dt),
                        landsat_dt_year=year(landsat_dt)) %>%
                        group_by(landsat_dt_month, landsat_dt_year, site_no, station_nm) %>%
                        summarise(media_ssc_prediction=mean(ssc_prediction))

vazao_ssc <- na.omit(left_join(vazao_data, landsat_serie_mensal, 
                               by=c("year"="landsat_dt_year", "month"="landsat_dt_month", 
                                    "EstacaoCodigo"="site_no"))
                     [,.(station_nm, EstacaoCodigo, Media, media_ssc_prediction)])

estacoes <- codigos_estacoes[codigos_estacoes == 66926000]

for(station in estacoes){
   vazao_ssc_station <- filter(vazao_ssc, EstacaoCodigo == station)
   m_vazao_ssc_station <- lm(media_ssc_prediction ~ Media, data = vazao_ssc_station)
   cor <- cor(vazao_ssc_station$media_ssc_prediction, vazao_ssc_station$Media)
   
   wd_exports_station <- paste0(wd_exports, unique(vazao_ssc_station$station_nm), '/')
   
   if(!dir.exists(wd_exports_station)){
      dir.create(wd_exports_station)
   }
   
   vazao_ssc_plot <- ggplot() +
      geom_point(data=vazao_ssc_station, aes(x=Media, y=media_ssc_prediction), fill="#999999", color="#000000", alpha=0.7, pch = 21, size = 5)+
      stat_smooth(data=vazao_ssc_station, aes(x=Media, y=media_ssc_prediction) , color="#FF0000", method="lm", se=F, size=1.5, formula = y~x)+
      stat_poly_eq(data=vazao_ssc_station, aes(x=Media, y=media_ssc_prediction),
                   label.x.npc = 0.025, label.y.npc = 0.9, colour="#FF0000",
                   formula = y~x, parse = TRUE, size = 4) +
      stat_correlation(data=vazao_ssc_station, aes(x=Media, y=media_ssc_prediction),
                   label.x = 0.025, label.y = 0.95, colour="#FF0000", parse = TRUE, size = 4, small.r = TRUE) +
      
      scale_y_continuous(limits=c(0, 4), breaks = seq(0, 2000, by = 0.5), expand=c(0,0))+
      scale_x_continuous(limits=c(0, 260), breaks = seq(0, 2000, by = 20), expand=c(0,0))+
      ylab("CSS Mensal Estimada (log(mg/L))")+
      xlab("Vazão Média Mensal (m³/s)")+
      ggtitle("Ponto do Grego (66926000)")+
      theme_clean()+
      theme(
         legend.position="none",
         plot.title = element_text(hjust = 0.5),
         axis.text =  element_text(size=10),
         axis.title = element_text(size=12, face = 'bold'),
         axis.title.y.right = element_text(color="#999999"),
         axis.title.y.left = element_text(color="#000000"),
         plot.background = element_blank())
   
   plot(vazao_ssc_plot)
   ggsave(vazao_ssc_plot, filename = paste0(wd_exports_station, '/vazao_css_', unique(vazao_ssc_station$station_nm), '.png'), 
          width = 6, height = 6) 
   # print(summary(m_vazao_ssc_station))
}

# m_vazao_ssc <- lm(10^ssc_prediction ~ Media + site_no, data = vazao_ssc)
# summary(m_vazao_ssc)
# 
# visreg(m_vazao_ssc, "site_no")
# cor(10^vazao_ssc$ssc_prediction, vazao_ssc$Media)
