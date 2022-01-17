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
# 

principais_estacoes <- fread("estacoes_selecionadas_grupos.csv")
principais_estacoes <- principais_estacoes[, ':=' (
  site_no = as.character(Codigo))]
codigos_estacoes <- principais_estacoes$site_no  

ls_toa_data <- importLandsatTOAData()[site_no %chin% codigos_estacoes]
ls_toa_data <- ls_toa_data[,  ':='(
                        ndssi = (ls_toa_data$B4-ls_toa_data$B1)/(ls_toa_data$B4+ls_toa_data$B1),
                        nsmi = (ls_toa_data$B3 + ls_toa_data$B2 - ls_toa_data$B1)/(ls_toa_data$B3 +ls_toa_data$B2 + ls_toa_data$B1),
                        index_montaigner_2014 = (ls_toa_data$B4+ ls_toa_data$B3)/(ls_toa_data$B2 + ls_toa_data$B1),
                        bi = sqrt((ls_toa_data$B3^2+ls_toa_data$B2^2)/2))]

ggplot()+
  geom_boxplot(aes(x = as.factor(ls_toa_data$station_nm), y = ls_toa_data$index_montaigner_2014))+
  theme_clean()

## Identify sites with too few Landsat measurements to be reliable
# Calculate number of satellite samples, per site
n_sat_samples <- ls_toa_data[,.(N_samples = .N), by = .(site_no)]
site_no_n100 <- n_sat_samples[N_samples >= 100, site_no]
ls_toa_data <- ls_toa_data[site_no %chin% site_no_n100 & num_pix >2 ]

#### JOIN LANDSAT AND IN SITU DATA, WITH LAG OF UP TO 10 DAYS, RESTRICT TO < 3 DAYS ####
## Join Landsat data with in situ data, allowing for as much as a 10-day lead/lag
lag_days <- 6
ls_sr_insitu_data <- joinSRInSituData(ls_toa_data, insitu_data, lagdays)

# Select minimum lead/lag row
setkey(ls_sr_insitu_data[,abs_lag_days := abs(lag_days)], abs_lag_days)
ls_sr_insitu_data <- ls_sr_insitu_data[, .SD[1], .(site_no, sample_date)]

dataset_summary <- summarizeDataSet()
plotDataSet(dataset_summary)

#####LOG Concentracao Mat Suspensao####
ggplot(ls_sr_insitu_data, aes(x = nsmi, y = log10_SSC_mgL)) +
  geom_point() +
  stat_smooth()
cor(ls_sr_insitu_data$nsmi, ls_sr_insitu_data$log10_SSC_mgL)
model <- stan_glm(log10_SSC_mgL ~ nsmi, data = ls_sr_insitu_data)
describe_posterior(model)
posteriors <- insight::get_parameters(model)
summary(model)
confint(model)

ggplot(ls_sr_insitu_data, aes(x = ndssi, y = log10_SSC_mgL)) +
  geom_point() +
  stat_smooth()
cor(ls_sr_insitu_data$ndssi, ls_sr_insitu_data$log10_SSC_mgL)
model <- lm(log10_SSC_mgL ~ ndssi, data = ls_sr_insitu_data)
summary(model)
confint(model)


ggplot(ls_sr_insitu_data, aes(x = bi, y = log10_SSC_mgL)) +
  geom_point() +
  stat_smooth()
cor(ls_sr_insitu_data$bi, ls_sr_insitu_data$log10_SSC_mgL)
model <- gam(log10_SSC_mgL ~ bi, data = ls_sr_insitu_data)
summary(model)
confint(model)
sigma(model)

#Melhor correlacao
ggplot(ls_sr_insitu_data, aes(x = index_montaigner_2014, y = log10_SSC_mgL)) +
  geom_point() +
  stat_smooth()
cor(ls_sr_insitu_data$index_montaigner_2014, ls_sr_insitu_data$log10_SSC_mgL)
model <- lm(log10_SSC_mgL ~ index_montaigner_2014, data = ls_sr_insitu_data)
summary(model)
confint(model)

model <- stan_glm(log10_SSC_mgL ~ index_montaigner_2014, data = ls_sr_insitu_data)
describe_posterior(model)
posteriors <- insight::get_parameters(model)
nrow(posteriors)

ggplot(posteriors, aes(x = index_montaigner_2014)) +
  geom_density(fill = "orange") +
  # The mean in blue
  geom_vline(xintercept = mean(posteriors$index_montaigner_2014), color = "blue", size = 1) +
  # The median in red
  geom_vline(xintercept = median(posteriors$index_montaigner_2014), color = "red", size = 1) +
  # The MAP in purple
  geom_vline(xintercept = map_estimate(posteriors$index_montaigner_2014), color = "purple", size = 1)

hdi(posteriors$index_montaigner_2014, ci = 0.89)


ls_toa_data$ssc_prediction<-predict(model, ls_toa_data)
ls_sr_insitu_data$ssc_prediction<-predict(model, ls_sr_insitu_data)

landsat_serie <- getLandsatHistoricalSerie(ls_toa_data)
generatePredictedHistoricalSerieByStation(ls_sr_insitu_data, ls_toa_data, landsat_serie)

