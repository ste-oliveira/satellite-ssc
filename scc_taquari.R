
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

pluviometria_data <- na.omit(fread('imports/pluviometria/COXIM_PLUVIOMETRIA.csv'))
pluviometria_data_bat <- na.omit(fread('imports/pluviometria/Precipacao_BAT.csv'))


pluviometria_data <- transmute(pluviometria_data, 
                     month=month(dmy(Data)),
                     year=year(dmy(Data)),
                     Data=dmy(Data),
                     Total = as.numeric(Total)) %>% distinct()

pluviometria_data_bat <- transmute(pluviometria_data_bat, 
                               month=month(dmy(Data)),
                               year=year(dmy(Data)),
                               Data=dmy(Data),
                               Total = as.numeric(Total)) %>% distinct()

pluviometria_data_merged <- rbind(pluviometria_data, pluviometria_data_bat)

vazao_data <- na.omit(fread('imports/vazao/VAZOES_ESTACOES.csv'))[EstacaoCodigo==66870000,]
vazao_data <- mutate(vazao_data, 
                     EstacaoCodigo=as.character(EstacaoCodigo), 
                     month=month(dmy(Data)),
                     year=year(dmy(Data)),
                     Data=dmy(Data),
                     Media = as.numeric(Media)) %>% 
              distinct()
vazao_data <-  na.omit(vazao_data[order(vazao_data$Data)])

vazao_data_anual <- group_by(vazao_data, year, EstacaoCodigo) 
vazao_data_anual<-summarise(vazao_data_anual, anual=mean(Media), Data=first(Data))

landsat_serie_mensal <- mutate(landsat_serie,
                               landsat_dt_month=month(landsat_dt),
                               landsat_dt_year=year(landsat_dt),
                               # month_year = paste0(month(landsat_dt, label=TRUE),"-",year(landsat_dt)),
                               # sort_order = year(landsat_dt) *100 + as.POSIXlt(landsat_dt)$mon,
                               ssc_prediction=10^ssc_prediction) %>%
  group_by(landsat_dt_month, landsat_dt_year, site_no, station_nm) %>%
  summarise(media_ssc_prediction=mean(ssc_prediction), landsat_dt=first(landsat_dt))

landsat_serie_anual <- group_by(landsat_serie_mensal, landsat_dt_year, site_no, station_nm) 
landsat_serie_anual <- summarise(landsat_serie_anual, ssc_prediction=mean(media_ssc_prediction), landsat_dt=first(landsat_dt))


# vazao_data <- filter(vazao_data, year>1983)

ssc_vazao<-left_join(landsat_serie_mensal, vazao_data, by = c("landsat_dt_year" = "year", "landsat_dt_month" = "month"))

generateBoxPlotPredictedHistoricalSerieByStation(landsat_serie_mensal, vazao_data, max_ssc_prediction_by_year, ssc_vazao,pluviometria_data_merged)
generatePredictedHistoricalSerie(landsat_serie, vazao_data, max_ssc_prediction_by_year, pluviometria_data_merged)

#### Decomposicao Serie Temporal Vazao ####
vazao_data_anual = mice(vazao_data_anual)
vazao_data_anual = complete(vazao_data_anual)
# vazao_data_anual$anual= scale(vazao_data_anual$anual)


hist(vazao_data_anual$anual, breaks=100)
shapiro.test(vazao_data_anual$anual)
vazao_data_ordenada <- na.omit(vazao_data_anual[order(vazao_data_anual$Data),])
vazao_ts <- na.remove(ts(vazao_data_anual$anual, frequency=1, start=c(1982)))
vazao_serie_decomposta <- decompose(vazao_ts, type="additive")
vazao_serie_decomposta <- data.frame(cbind(vazao_serie_decomposta$trend, vazao_data_ordenada$Data))
colnames(vazao_serie_decomposta) <- c("trend", "data")
vazao_serie_decomposta <- mutate(vazao_serie_decomposta,
                                 data = as.Date(data))


MK = MannKendall(vazao_ts)
summary(MK)
sens.slope(vazao_ts)
pettitt.test(vazao_ts)


#### Correlação Linear Vazao ####
cor(vazao_data_anual$year, vazao_data_anual$anual)
vazao_lm <- lm(data = vazao_data_anual, anual~year)
summary(vazao_lm)

ggplot() + 
  geom_point(aes(x=vazao_data_anual$year, y=vazao_data_anual$anual, colour="blue") ) +
  stat_smooth(data=vazao_data_anual, aes(x=year, y=anual, colour="red",) , method="lm", se=F, size=1.5, formula = y~x)+
  # geom_line(data=vazao_serie_decomposta, aes(x=data, y=trend,  colour="green"), size=1.2)+
  
  scale_y_continuous(limits=c(0, 700), breaks = seq(0, 700, by = 50), expand=c(0,0))+
  scale_x_continuous(limits=c(1966, 2021), breaks = seq(1966, 2021, by = 1), expand=c(0,0))+
  # scale_x_date(date_labels = "%Y", date_breaks = "1 year",
  #              limits = as.Date(c("1984-01-01","2021-01-01")), expand = c(0, 0))+
  
  # scale_color_manual(name="Legenda", values = c("Discharge Trend"="#6a51a3"),
  #                    guide = guide_legend(override.aes = list(linetype = c(1), size = c(1.2), shape=c(NA))))+
  
  stat_poly_eq(data=vazao_data_anual, aes(x=year, y=anual),
               label.x.npc = 0.025, label.y.npc = 0.9, colour="#FF0000",
               formula = y~x, parse = TRUE, size = 4) +
  stat_correlation(data=vazao_data_anual, aes(x=year, y=anual),
                   label.x = 0.025, label.y = 0.95, colour="#FF0000", parse = TRUE, size = 4, small.r = TRUE) +
  
  ylab("Avagare Discharge (m³/s)")+
  theme_clean()+
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0,0,0,"cm"),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.key.width = unit(1, "cm"),
    legend.background = element_rect(color = NA),
    axis.text =  element_text(size=10),
    axis.title = element_text(size=12, face = 'bold'),
    axis.title.y.right = element_text(color="#000000"),
    axis.title.y.left = element_text(color="#000000"),
    axis.text.x=element_text(angle=60, hjust=1),
    plot.background = element_blank())

#plot(ggarrange(predictplot, vazaoplot, nrow=2, ncol=1))


#### Decomposicao Serie Temporal CSS ####
hist(landsat_serie_mensal$media_ssc_prediction, breaks=30)
shapiro.test(landsat_serie_mensal$media_ssc_prediction)

mice_ssc = mice(landsat_serie_mensal)
complece_mice_ssc = complete(mice_ssc)

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


#### Correlação Linear SSC ####
cor(landsat_serie_anual$landsat_dt_year, landsat_serie_anual$ssc_prediction)
ssc_lm <- lm(data = landsat_serie_anual, ssc_prediction~landsat_dt_year)
summary(ssc_lm)

ggplot() + 
  geom_point(aes(x=landsat_serie_anual$landsat_dt_year, y=landsat_serie_anual$ssc_prediction, colour="blue") ) +
  stat_smooth(data=landsat_serie_anual, aes(x=landsat_dt_year, y=ssc_prediction, colour="red",) , method="lm", se=F, size=1.5, formula = y~x)+
  # geom_line(data=vazao_serie_decomposta, aes(x=data, y=trend,  colour="green"), size=1.2)+
  
  scale_y_continuous(limits=c(0, 1000), breaks = seq(0, 1000, by = 100), expand=c(0,0))+
  scale_x_continuous(limits=c(1966, 2021), breaks = seq(1966, 2021, by = 1), expand=c(0,0))+
  # scale_x_date(date_labels = "%Y", date_breaks = "1 year",
  #              limits = as.Date(c("1984-01-01","2021-01-01")), expand = c(0, 0))+
  
  # scale_color_manual(name="Legenda", values = c("Discharge Trend"="#6a51a3"),
  #                    guide = guide_legend(override.aes = list(linetype = c(1), size = c(1.2), shape=c(NA))))+
  
  stat_poly_eq(data=landsat_serie_anual, aes(x=landsat_dt_year, y=ssc_prediction),
               label.x.npc = 0.025, label.y.npc = 0.9, colour="#FF0000",
               formula = y~x, parse = TRUE, size = 4) +
  stat_correlation(data=landsat_serie_anual, aes(x=landsat_dt_year, y=ssc_prediction),
                   label.x = 0.025, label.y = 0.95, colour="#FF0000", parse = TRUE, size = 4, small.r = TRUE) +
  
  ylab("Avagare Discharge (m³/s)")+
  theme_clean()+
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0,0,0,"cm"),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.key.width = unit(1, "cm"),
    legend.background = element_rect(color = NA),
    axis.text =  element_text(size=10),
    axis.title = element_text(size=12, face = 'bold'),
    axis.title.y.right = element_text(color="#000000"),
    axis.title.y.left = element_text(color="#000000"),
    axis.text.x=element_text(angle=60, hjust=1),
    plot.background = element_blank())



#### Serie Historia CSS e Vazao ####

generatePredictedHistoricalSerieByStation(landsat_serie, vazao_data, vazao_serie_decomposta, landsat_serie_decomposta)

generateLinearPredictedHistoricalSerieByStation(landsat_serie, vazao_data_anual, vazao_serie_decomposta, landsat_serie_decomposta)

