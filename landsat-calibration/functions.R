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

generatePredictedHistoricalSerieByStation <- function(landsat_serie, vazao_data, max_ssc_prediction_by_year){
   stations_predicted <- data.frame(landsat_serie[, .(station_nm), .(station_nm)])[,1]
   
   for(station in stations_predicted){
      predictedSSC_station <- landsat_serie[station_nm==station]
      vazao_data_station <- vazao_data[EstacaoCodigo==unique(predictedSSC_station$site_no)]
      max_ssc_prediction_by_year_station <- filter(max_ssc_prediction_by_year,station_nm == station)
      
      wd_exports_station <- paste0(wd_exports, station, '/')

      if(!dir.exists(wd_exports_station)){
         dir.create(wd_exports_station)
      }

      # write_csv(station_summary, paste0(wd_exports_station, station,'_station_summary.csv'))

      # create data
      # landsat_dt <- predictedSSC_station$landsat_dt
      # ssc_prediction <- 10^predictedSSC_station$ssc_prediction
      # data <- data.frame(landsat_dt,ssc_prediction, name=predictedSSC_station$station_nm)
      # data <-data[order(data$landsat_dt),]
      # color <-  cbPalette[which(stations_predicted == station)]
      # 

      predictplot <-ggplot() +
         
         geom_line(data=predictedSSC_station, aes(x=landsat_dt, y=10^ssc_prediction), size=0.5, color="#000000") +
         geom_point(data=predictedSSC_station, aes(x=landsat_dt, y=10^ssc_prediction, colour=sensor), 
                    size=1.5) +
         stat_smooth(data=predictedSSC_station, aes(x=landsat_dt, y=10^ssc_prediction) , color="#66bd63", method="lm",
                     linetype = "dashed", se=F, size=1.2, formula = y~x)+
         stat_poly_eq(data=predictedSSC_station, aes(x=landsat_dt, y=10^ssc_prediction),
                      label.x.npc = 0.025, label.y.npc = 0.95, colour="#66bd63",
                      formula = y~x, parse = TRUE, size = 4) +

         geom_point(data=max_ssc_prediction_by_year_station, aes(x=landsat_dt,y=10^ssc_prediction),
                        size=2, color="#d73027")+
         stat_smooth(data=max_ssc_prediction_by_year_station, aes(x=landsat_dt,y=10^ssc_prediction),
                     formula = y~x, method="lm", se=F, size=1.2, color="#a50026", linetype = "dashed")+
         stat_poly_eq(data=max_ssc_prediction_by_year_station, aes(x=landsat_dt,y=10^ssc_prediction),
                      label.x.npc = 0.025, label.y.npc = 0.9,
                      formula = y~x, parse = TRUE, size = 4, color="#a50026") +

         scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                      limits = as.Date(c("1984-01-01","2021-01-01")), expand = c(0, 0))+
         scale_y_continuous(limits=c(0, 900), breaks = seq(0, 2000, by = 100), expand=c(0,0))+
         scale_color_manual(values = c("#2171b5", "#08306b"))+
         ylab("CSS Estimada (mg/L)")+
         xlab("Ponto do Grego (66926000)")+
         theme_clean()+
         theme(
            legend.position="none",
            axis.text =  element_text(size=10),
            axis.title = element_text(size=12, face = 'bold'),
            axis.title.y.right = element_text(color="#999999"),
            axis.title.y.left = element_text(color="#000000"),
            axis.text.x=element_text(angle=60, hjust=1),
            plot.background = element_blank())

      vazaoplot <-ggplot() +
         geom_point(aes(x=vazao_data_station$Data, y=vazao_data_station$Media), color="#999999")+
         geom_line(aes(x=vazao_data_station$Data, y=vazao_data_station$Media), size=0.5, color="#999999") +
         
         scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                      limits = as.Date(c("1984-01-01","2021-01-01")), expand = c(0, 0))+
         scale_y_continuous(limits=c(0, 400), breaks = seq(0, 3000, by = 50), expand=c(0,0))+
         ylab("Vazão Média (m³/s)")+
         xlab("Ponto do Grego (66926000)")+
         theme_clean()+
         theme(
            axis.text =  element_text(size=10),
            axis.title = element_text(size=12, face = 'bold'),
            axis.title.y.right = element_text(color="#999999"),
            axis.title.y.left = element_text(color="#000000"),
            axis.text.x=element_text(angle=60, hjust=1),
            plot.background = element_blank())
      
      plot(ggarrange(predictplot, vazaoplot, nrow=2, ncol=1))
      
      ggsave(predictplot, filename = paste0(wd_exports_station, '/ssc_', station, '.png'), 
             width = 10, height = 6) 
      ggsave(vazaoplot, filename = paste0(wd_exports_station, '/vazao_', station, '.png'), 
             width = 10, height = 6) 
      break;
      # write.xlsx(predictedSSC_station[,c("station_nm", "sensor", "ssc_prediction", "landsat_dt")], 
      #            paste0(wd_exports, station,'.xlsx') ,'predicoes.xlsx')
      # write.xlsx(max_ssc_prediction_by_year_station[,c("station_nm", "sensor", "ssc_prediction", "landsat_dt")], 
      #            paste0(wd_exports, station,'.xlsx'),'predicoes_maximas_anuais',  append=TRUE)
      # write.xlsx(cota_data_station, paste0(wd_exports, station,'.xlsx'), "cotas",  append=TRUE)

   }
}
