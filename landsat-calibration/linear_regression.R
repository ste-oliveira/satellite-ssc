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
principais_estacoes <- fread("estacoes_principais.csv")
principais_estacoes <- principais_estacoes[, ':=' (
  site_no = as.character(Codigo))]$site_no



insitu_data_site_nos <- insitu_data[site_no == 66870000]$site_no
#insitu_data_site_nos <- unique(insitu_data[insitu_data$EstacaoCodigo %in% principais_estacoes]$site_no)
# insitu_data_site_nos <- unique(insitu_data[!is.na(site_no),site_no])


# Import landsat spectral data from each site of interest
ls_sr_data <- importLandsatSurfaceReflectanceData()[site_no %chin% insitu_data_site_nos]

ls_sr_data <- ls_sr_data[,  ':='(
                        ndssi = (ls_sr_data$B4-ls_sr_data$B1)/(ls_sr_data$B4+ls_sr_data$B1),
                        nsmi = (ls_sr_data$B3 + ls_sr_data$B2 - ls_sr_data$B1)/(ls_sr_data$B3 +ls_sr_data$B2 + ls_sr_data$B1),
                        index_montaigner_2014 = (ls_sr_data$B4+ ls_sr_data$B3)/(ls_sr_data$B2 + ls_sr_data$B1),
                        bi = sqrt((ls_sr_data$B3^2+ls_sr_data$B2^2)/2))]

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

# Select minimum lead/lag row
setkey(ls_sr_insitu_data[,abs_lag_days := abs(lag_days)], abs_lag_days)
ls_sr_insitu_data <- ls_sr_insitu_data[, .SD[1], .(site_no, sample_date)]

dataset_summary <- summarizeDataSet()
plotDataSet(dataset_summary)

#Melhor correlacao
ggplot(ls_sr_insitu_data, aes(x = index_montaigner_2014, y = log10_SSC_mgL)) +
  geom_point() +
  stat_smooth()
cor(ls_sr_insitu_data$index_montaigner_2014, ls_sr_insitu_data$log10_SSC_mgL)
model <- lm(log10_SSC_mgL ~ index_montaigner_2014, data = ls_sr_insitu_data)
summary(model)
confint(model)


ls_sr_data$ssc_prediction<-predict(model, ls_sr_data)
ls_sr_insitu_data$ssc_prediction<-predict(model, ls_sr_insitu_data)

landsat_serie <- getLandsatHistoricalSerie(ls_sr_data)
generatePredictedHistoricalSerieByStation(ls_sr_insitu_data, ls_sr_data, landsat_serie)

