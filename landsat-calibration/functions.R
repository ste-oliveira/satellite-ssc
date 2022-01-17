regressors_all <- c('B1', 'B2', 'B3', 'B4', 'B5', # raw bands
                  'B1.2', 'B2.2', 'B3.2', 'B4.2', 'B5.2', # squared bands
                  'site_no', # no clear way to add categorical variables
                  'B2.B1', 'B3.B1', 'B4.B1', 'B5.B1', # band ratios
                  'B3.B2', 'B4.B2', 'B5.B2',
                  'B4.B3', 'B5.B3', 'B5.B4')

regressors_no_site <- c('B1', 'B2', 'B3', 'B4', 'B5', # raw bands
          'B2.B1', 'B3.B1', 'B4.B1', 'B5.B1' , # band ratios
          'B3.B2', 'B4.B2', 'B5.B2', 'B4.B3', 'B5.B3', 'B5.B4')

# Select regressors - bands and band ratios
regressors_primary <- c(regressors_no_site, 'B4.B3.B1') # all regressors

# TYPICAL RIVER SEDIMENT COLOR AT DIFFERENT CLUSTERS
# Select SSC categories for plotting
# ssc_categories <- c(0,25,50,100,250,500,750,1000,1500, 1e6)
ssc_categories <- c(0,250,500,750,1000, 1500)
# ssc_categories <- c(0,50,100,200,500,1e6)
# ssc_categories <- c(0,10,25,50,75,100,150,200,250,300,350, 400, 450, 500,600, 700, 800,900,1000,1100,1500, 1e6)

# Generate SSC labels as 'low value' - 'high value'
ssc_category_labels <- paste0(ssc_categories[-length(ssc_categories)],'-',c(ssc_categories[-1]))
# Make highest SSC category "> highest value"
ssc_category_labels[length(ssc_category_labels)] <- paste0('> ', ssc_categories[length(ssc_category_labels)])

generatePredictedHistoricalSerieByStation <- function(landsat_serie, cota_data, max_ssc_prediction_by_year){
   stations_predicted <- data.frame(landsat_serie[, .(station_nm), .(station_nm)])[,1]
   
   for(station in stations_predicted){
      predictedSSC_station <- landsat_serie[station_nm==station]
      cota_data_station <- cota_data[EstacaoCodigo==unique(predictedSSC_station$site_no)]
      max_ssc_prediction_by_year_station <- filter(max_ssc_prediction_by_year,station_nm == station)
      
      wd_exports_station <- paste0(wd_exports, station, '/')

      if(!dir.exists(wd_exports_station)){
         dir.create(wd_exports_station)
      }

      station_summary <- dataset_summary[, ':='(
         serieImage = nrow(landsat_serie[station_nm==station])
      )]

      # write_csv(station_summary, paste0(wd_exports_station, station,'_station_summary.csv'))

      # create data
      landsat_dt <- predictedSSC_station$landsat_dt
      ssc_prediction <- 10^predictedSSC_station$ssc_prediction
      data <- data.frame(landsat_dt,ssc_prediction, name=predictedSSC_station$station_nm)
      data <-data[order(data$landsat_dt),]
      color <-  cbPalette[which(stations_predicted == station)]

      # predictplot <-ggplot() +
      #    stat_smooth(aes(x=data$landsat_dt, y=data$ssc_prediction), method="glm")+
      #    geom_area(aes(x=cota_data_station$Data, y=cota_data_station$Media), position = position_dodge(), fill="#999999", alpha=0.5)+
      #    geom_line(aes(x=data$landsat_dt, y=data$ssc_prediction), size=0.25, color="#000000") +
      #    geom_line(aes(x=max_ssc_prediction_by_year_station$landsat_dt, y=10^max_ssc_prediction_by_year_station$ssc_prediction), size=0.5, color="#FF0000")+
      #    # stat_smooth(aes(x=max_ssc_prediction_by_year_station$landsat_dt, y=10^max_ssc_prediction_by_year_station$ssc_prediction), method="glm", se=F, size=0.5, color="#000000")+
      #    scale_x_date(date_labels = "%b/%y", date_breaks = "1 year",
      #                 limits = as.Date(c("1984-01-01","2021-01-01")), expand = c(0, 0))+
      #    scale_y_continuous(breaks = seq(0, 2000, by = 100), sec.axis = dup_axis(name="Cota Mensal Média (cm)"), expand=c(0,0))+
      #    ylab("Concentração de Sedimentoos Estimada (mg/L)")+
      #    xlab(station)+
      #    theme_clean()+
      #    theme(
      #       axis.text =  element_text(size=10),
      #       axis.title = element_text(size=12, face = 'bold'),
      #       axis.title.y.right = element_text(color="#999999"),
      #       axis.title.y.left = element_text(color="#000000"),
      #       axis.text.x=element_text(angle=60, hjust=1),
      #       plot.background = element_blank())
      # 
      # plot(predictplot)
      
      write.xlsx(predictedSSC_station[,c("station_nm", "sensor", "ssc_prediction", "landsat_dt")], 
                 paste0(wd_exports, station,'.xlsx') ,'predicoes.xlsx')
      write.xlsx(max_ssc_prediction_by_year_station[,c("station_nm", "sensor", "ssc_prediction", "landsat_dt")], 
                 paste0(wd_exports, station,'.xlsx'),'predicoes_maximas_anuais',  append=TRUE)
      write.xlsx(cota_data_station, paste0(wd_exports, station,'.xlsx'), "cotas",  append=TRUE)
      # ggsave(predictplot, filename = paste0(wd_exports_station, 'ssc_', station, '_predict.png'),
      #        width = 12, height = 4.5)


      # insitu_raw_station <- ls_sr_insitu_data[station_nm==station]
      # # insitu_raw_station <- insitu_raw_station[,':='(absoluteError = ae(10^log10_SSC_mgL, 10^ssc_prediction))]
      # 
      # # lagdaysError <- ggplot(insitu_raw_station) +
      # #    geom_point(aes(x=lag_days, y=diff),color = '#000000', fill="#FF0000", pch = 21, size=3)+
      # #    theme(
      # #       legend.position = c(.85, .85),
      # #       legend.key.size = unit(5, 'mm'),
      # #       legend.title = element_text(size=10), #change legend title font size
      # #       legend.text = element_text(size=8),
      # #       plot.background = element_blank())+
      # #    theme_clean()+
      # #    scale_y_continuous(breaks = seq(0, 500, by = 50), limits = c(0, 500))+
      # #    scale_x_continuous(breaks = seq(-9, 9, by = 1), limits = c(-9, 9))+
      # #    labs(
      # #       y = 'Erro Absoluto (mg/L)',
      # #       x = 'Lag Days'
      # #    )
      # plot(lagdaysError)

      # ggsave(lagdaysError, filename = paste0(wd_exports_station, '/ssc_', station, '_lagdays_error.png'),
      #        width = 6, height = 5)
   }
}