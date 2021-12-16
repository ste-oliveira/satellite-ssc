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
principais_estacoes <- fread("estacoes_selecionadas_filtrado.csv")
principais_estacoes <- principais_estacoes[, ':=' (
  site_no = as.character(Codigo))]
codigos_estacoes <- principais_estacoes$site_no  

# insitu_data_site_nos <- insitu_data[site_no == 66870000]$site_no
#insitu_data_site_nos <- unique(insitu_data[insitu_data$EstacaoCodigo %in% principais_estacoes]$site_no)
# insitu_data_site_nos <- unique(insitu_data[!is.na(site_no),site_no])

# Import landsat spectral data from each site of interest
ls_sr_data <- importLandsatSurfaceReflectanceData()[site_no %chin% codigos_estacoes]

ls_sr_data <- ls_sr_data[,  ':='(
  ndssi = (ls_sr_data$B4-ls_sr_data$B1)/(ls_sr_data$B4+ls_sr_data$B1),
  nsmi = (ls_sr_data$B3 + ls_sr_data$B2 - ls_sr_data$B1)/(ls_sr_data$B3 +ls_sr_data$B2 + ls_sr_data$B1),
  index_montaigner_2014 = (ls_sr_data$B4+ ls_sr_data$B3)/(ls_sr_data$B2 + ls_sr_data$B1),
  bi = sqrt((ls_sr_data$B3^2+ls_sr_data$B2^2)/2),
  B3_B2_B1 = ls_sr_data$B3/(ls_sr_data$B2+ls_sr_data$B1))]

ggplot()+
  geom_boxplot(aes(x = as.factor(ls_sr_data$station_nm), y = ls_sr_data$index_montaigner_2014))+
  theme_clean()

## Identify sites with too few Landsat measurements to be reliable
# Calculate number of satellite samples, per site
n_sat_samples <- ls_sr_data[,.(N_samples = .N), by = .(site_no)]
site_no_n100 <- n_sat_samples[N_samples >= 100, site_no]
ls_sr_data <- ls_sr_data[site_no %chin% site_no_n100 & num_pix > 2 ]

#### JOIN LANDSAT AND IN SITU DATA, WITH LAG OF UP TO 10 DAYS, RESTRICT TO < 3 DAYS ####
## Join Landsat data with in situ data, allowing for as much as a 10-day lead/lag
lag_days <- 3
ls_sr_insitu_data <- joinSRInSituData(ls_sr_data, insitu_data, lagdays)
# Select minimum lead/lag row
setkey(ls_sr_insitu_data[,abs_lag_days := abs(lag_days)], abs_lag_days)
ls_sr_insitu_data <- ls_sr_insitu_data[, 
                        list(index_montaigner_2014=mean(index_montaigner_2014)), 
                        .(site_no, sample_date, station_nm, log10_SSC_mgL, sensor)]
n_insitu_samples <- ls_sr_insitu_data[,.(N_samples = .N), by = .(site_no)]

ls_sr_insitu_data <- left_join(ls_sr_insitu_data, principais_estacoes, by = c("site_no" = "site_no"))


ls_sr_insitu_data <- mutate(ls_sr_insitu_data, 
                            station_nm =as.factor(station_nm),
                            site_no =as.factor(site_no),
                            sensor = as.factor(sensor),
                            grupo= as.factor(Grupo))

dataset_summary <- summarizeDataSet(ls_sr_data, insitu_data, ls_sr_insitu_data)
plotDataSet(dataset_summary)

#Melhor correlacao
model <- lm(log10_SSC_mgL ~ index_montaigner_2014 + station_nm , data = ls_sr_insitu_data)
summary(model)
summ(model)
plot_summs(model, omit.coefs = c("(Intercept)", "index_montaigner_2014"))



# ggplot(ls_sr_insitu_data, aes(x = index_montaigner_2014, y = log10_SSC_mgL)) +
#   geom_point() +
#   stat_smooth()
# effect_plot(model, pred = index_montaigner_2014, interval = TRUE, plot.points = TRUE)

# confint(model)
# 
# ls_sr_data$ssc_prediction<-predict(model, ls_sr_data)
# ls_sr_insitu_data$ssc_prediction<-predict(model, ls_sr_insitu_data)
# 
# landsat_serie <- getLandsatHistoricalSerie(ls_sr_data)
# generatePredictedHistoricalSerieByStation(ls_sr_insitu_data, ls_sr_data, landsat_serie)

