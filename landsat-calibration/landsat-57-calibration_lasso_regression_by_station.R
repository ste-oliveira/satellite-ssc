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

# insitu_data_site_nos <- insitu_data[site_no == 66870000]$site_no
insitu_data_site_nos <- unique(insitu_data[insitu_data$EstacaoCodigo %in% principais_estacoes]$site_no)
# insitu_data_site_nos <- unique(insitu_data[!is.na(site_no),site_no])

# Import landsat spectral data from each site of interest
ls_sr_data <- importLandsatSurfaceReflectanceData()[site_no %chin% insitu_data_site_nos]

ls_sr_data <- ls_sr_data[,  ':='(
                        ndssi = (ls_sr_data$B4-ls_sr_data$B1)/(ls_sr_data$B4+ls_sr_data$B1),
                        nsmi = (ls_sr_data$B3 + ls_sr_data$B2 - ls_sr_data$B1)/(ls_sr_data$B3 +ls_sr_data$B2 + ls_sr_data$B1),
                        index_montaigner_2014 = (ls_sr_data$B4+ ls_sr_data$B3)/(ls_sr_data$B2 + ls_sr_data$B1),
                        bi = sqrt((ls_sr_data$B3^2+ls_sr_data$B2^2)/2),
                        B3_B2_B1 = ls_sr_data$B3/(ls_sr_data$B2+ls_sr_data$B1)) ]

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

write_csv(ls_sr_insitu_data, paste0(wd_exports,'ls_sr_insitu_data.csv'))

dataset_summary <- summarizeDataSet()
plotDataSet(dataset_summary)

vars <- c("B1", "B2", "B3", "B4", "B5", "B7")

vars <- c("B1", "B2", "B3", "B4", "B5", "B1.2", "B2.B1", "B3.B1", "B4.B1", "B5.B1", "B3.B2", "B4.B2",
          "B5.B2", "B4.B3", "B5.B3", "B5.B4", "B3_B2_B1")

vars <- c("B2", "B3", "B4", "B5", "B2.B1", "B3.B1", "B3.B2", "B4.B3")

station_data <- ls_sr_insitu_data

for(station in principais_estacoes){
  station_data <- ls_sr_insitu_data[site_no == station]

  if(nrow(station_data) > 3){
    print(paste0('############################################################'))
    print(paste0('####### ',unique(station_data$station_nm),'####### '))
    print(paste0('############################################################'))
    
    glm_y <- as.matrix(station_data[,log10_SSC_mgL])
    glm_x <- as.matrix(station_data[,..vars])
    
    print(paste0('#################Regressao Lasso#############################'))
    leave_one_out = nrow(station_data)
    ssc_lm <- cv.glmnet(x = glm_x, y = glm_y, family = 'gaussian', type.measure = "mse", nfolds = leave_one_out, alpha = 1)
    
    cv.opt <- coef(ssc_lm, s = "lambda.1se")
    # plot(ssc_lm)
    
    station_data$glm_pred <- predict(ssc_lm, newx = as.matrix(station_data[,..vars]), type = "response", s = "lambda.1se")
    glm_pred_r2 <- R2_Score( 10^station_data$glm_pred, 10^station_data[,log10_SSC_mgL])
    glm_pred_rmse <- Metrics::rmse(10^station_data[,log10_SSC_mgL] ,  10^station_data$glm_pred)
    glm_pred_sse <- Metrics::sse(10^station_data[,log10_SSC_mgL] ,  10^station_data$glm_pred)
    
    glm_pred_r2_log <- R2_Score( station_data$glm_pred, station_data[,log10_SSC_mgL])
    glm_pred_rmse_log <- Metrics::rmse(station_data[,log10_SSC_mgL] ,  station_data$glm_pred)
    glm_pred_sse_log <- Metrics::sse(station_data[,log10_SSC_mgL] ,  station_data$glm_pred)

    # print(paste0('R2(',unique(station_data$station_nm),')= ', R2_Score(glm_pred, station_data[,log10_SSC_mgL])))
    print(paste0('############################################################'))
    print(cv.opt)
    print(paste0("n= ", nrow(station_data)))
    print(paste0("R2 (log)= ", round(glm_pred_r2_log,2)))
    print(paste0("RMSE(log)= ", round(glm_pred_rmse_log,2)))
    print(paste0("SSE(log)= ", round(glm_pred_sse_log,2)))
    
    error_lasso_plot<-ggplot(data = station_data, aes(x = log10_SSC_mgL, y = glm_pred)) +
      geom_abline(intercept = 0, slope = 1, color = '#333333', size=2)+
      geom_point(aes(),color = '#000000', fill="#FF0000", pch = 21, size = 4)+
      geom_text(aes(x = 1, y = 3, label = paste0("R2= ", round(glm_pred_r2,2))), hjust = 0, vjust = 0, size=5, color="#000000")+
      geom_text(aes(x = 1, y = 2.9, label = paste0("RMSE= ", round(glm_pred_rmse,2))), hjust = 0, vjust = 0, size=5, color="#000000")+
      geom_text(aes(x = 1, y = 2.8, label = paste0("SSE= ", round(glm_pred_sse,2))), hjust = 0, vjust = 0, size=5, color="#000000")+
      geom_text(aes(x = 1, y = 2.7, label = paste0("R2 (Log)= ", round(glm_pred_r2_log,2))), hjust = 0, vjust = 0, size=5, color="#000000")+
      geom_text(aes(x = 1, y = 2.6, label = paste0("RMSE (Log)= ", round(glm_pred_rmse_log,2))), hjust = 0, vjust = 0, size=5, color="#000000")+
      geom_text(aes(x = 1, y = 2.5, label = paste0("SSE=  (Log)= ", round(glm_pred_sse_log,2))), hjust = 0, vjust = 0, size=5, color="#000000")+
      theme_clean() +
      scale_y_continuous(limits = c(1,3), breaks = seq(1, 3, by = 0.1)) + 
      scale_x_continuous(limits = c(1,3), breaks = seq(1, 3, by = 0.1)) +
      # scale_y_continuous(limits = c(0,700), breaks = seq(0, 700, by = 50)) + 
      # scale_x_continuous(limits = c(0,700), breaks = seq(0, 700, by = 50)) +
      theme(
        legend.title = element_blank(),
        legend.position = 'none',
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.background = element_blank()
      ) + 
      labs(
        x = expression(paste("Concentraca de Sedimentos ", italic("In Situ"), " (mg/L)")),
        y = "Concentracao de Sedimentos Estimada por Sateite (mg/L)"
      )
    ggsave(error_lasso_plot, filename = paste0(wd_figures, unique(station_data$station_nm) ,'_error_lasso_plot.png'), width = 10, height = 9)
    
    
    print(paste0('#################Regressao Linear Simples#############################'))
  
    model <- lm(log10_SSC_mgL ~ B2 + B3 + B4 + B5 + B7 + B3.B2, data = station_data)
    summary(model)
    confint(model)
    station_data$lm_pred<-predict(model, station_data)
    lm_pred_r2 <- R2_Score( 10^station_data$lm_pred, 10^station_data[,log10_SSC_mgL])
    lm_pred_rmse <- Metrics::rmse(10^station_data[,log10_SSC_mgL] ,  10^station_data$lm_pred)
    lm_pred_sse <- Metrics::sse(10^station_data[,log10_SSC_mgL] ,  10^station_data$lm_pred)
    
    lm_pred_r2_log <- R2_Score( station_data$lm_pred, station_data[,log10_SSC_mgL])
    lm_pred_rmse_log <- Metrics::rmse(station_data[,log10_SSC_mgL] ,  station_data$lm_pred)
    lm_pred_sse_log <- Metrics::sse(station_data[,log10_SSC_mgL] ,  station_data$lm_pred)
    
    print(paste0("R2 (log)= ", round(lm_pred_r2_log,2)))
    print(paste0("RMSE(log)= ", round(lm_pred_rmse_log,2)))
    print(paste0("SSE(log)= ", round(lm_pred_sse_log,2)))
    
    error_linear_plot<-ggplot(data = station_data, aes(x = log10_SSC_mgL, y = lm_pred)) +
      geom_abline(intercept = 0, slope = 1, color = '#333333', size=2)+
      geom_point(aes(),color = '#000000', fill="#FF0000", pch = 21, size = 4)+
      geom_text(aes(x = 1, y = 3, label = paste0("R2= ", round(lm_pred_r2,2))), hjust = 0, vjust = 0, size=5, color="#000000")+
      geom_text(aes(x = 1, y = 2.9, label = paste0("RMSE= ", round(lm_pred_rmse,2))), hjust = 0, vjust = 0, size=5, color="#000000")+
      geom_text(aes(x = 1, y = 2.8, label = paste0("SSE= ", round(lm_pred_sse,2))), hjust = 0, vjust = 0, size=5, color="#000000")+
      geom_text(aes(x = 1, y = 2.7, label = paste0("R2 (Log)= ", round(lm_pred_r2_log,2))), hjust = 0, vjust = 0, size=5, color="#000000")+
      geom_text(aes(x = 1, y = 2.6, label = paste0("RMSE (Log)= ", round(lm_pred_rmse_log,2))), hjust = 0, vjust = 0, size=5, color="#000000")+
      geom_text(aes(x = 1, y = 2.5, label = paste0("SSE=  (Log)= ", round(lm_pred_sse_log,2))), hjust = 0, vjust = 0, size=5, color="#000000")+
      theme_clean() +
      scale_y_continuous(limits = c(1,3), breaks = seq(1, 3, by = 0.1)) + 
      scale_x_continuous(limits = c(1,3), breaks = seq(1, 3, by = 0.1)) +
      theme(
        legend.title = element_blank(),
        legend.position = 'none',
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.background = element_blank()
      ) + 
      labs(
        x = expression(paste("Concentraca de Sedimentos ", italic("In Situ"), " (mg/L)")),
        y = "Concentracao de Sedimentos Estimada por Sateite (mg/L)"
      )
    # plot(error_linear_plot)
     ggsave(error_linear_plot, filename = paste0(wd_figures, unique(station_data$station_nm) ,'_error_linear_plot.png'), width = 10, height = 9)
    
  }
  print("")
  print("")
  print("")
}

