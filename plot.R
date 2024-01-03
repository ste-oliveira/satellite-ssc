# Plot number of satellite samples per site as a histogram
n_sat_samples_histogram <- function(n_sat_samples){ 
  ggplot(n_sat_samples, aes(x = N_samples)) + 
  geom_histogram(color = 'black', lwd = 0.25, binwidth = 100) +
  geom_vline(
    data = n_sat_samples[,.(N_samples = mean(N_samples))], 
    aes(xintercept = N_samples), color = 'orange', lty = 'dashed') + 
  geom_text(
    data = n_sat_samples[,.(N_samples = mean(N_samples))], 
    aes(label = paste0('mean = ',round(N_samples), ' samples'), x = N_samples + 40, y = 120),
    hjust = 0, size = 3) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = 'Number of cloud-free satellite samples/site',
    y = 'Number of sites'
  )#
}

### PLOT DATA SET IN SITU ####
plotDataSetInsitu <- function(dataset) { 
  means.long<-melt(dataset[,.(insitu, match, station_nm)],id.vars="station_nm")
  
  Grouped
  n_sat_samples_histogram <-ggplot(means.long, aes(fill=variable, y=value, x=station_nm)) + 
    geom_bar(position="dodge", stat="identity", width=0.8)+
    geom_text(aes(label=value), position = position_dodge(width = 1),
              vjust = -0.5, size = 3, colour="#777777")+
    theme_clean()+
    scale_y_continuous(breaks = seq(0, 150, by = 10), limits = c(0, 150))+
    scale_fill_manual(values = c("#1f78b4", "#b2df8a"), name = "Legenda", labels = c("In Situ", "Match"),
                      guide = guide_legend(reverse = FALSE))+
    theme(
      legend.position = c(.9, .85),
      legend.key.size = unit(5, 'mm'),
      legend.title = element_text(size=10), #change legend title font size
      legend.text = element_text(size=8),
      plot.background = element_blank(),
      axis.text.x = element_text(angle = 60, hjust =1, size=11),
      axis.text.y = element_text(size=10),
      axis.title = element_text(size=11))+
    labs(
      x = NULL,
      y = 'Quantidade de Amostras'
    )
  
  plot(n_sat_samples_histogram)
  
  # Save satellite images/site histogram
  ggsave(n_sat_samples_histogram, filename = paste0(wd_figures,'n_sat_samples_histogram.png'), width = 6, height = 4)
  
} 


### PLOT DATA SET IN SITU ####
plotDataSetInsitu1 <- function(dataset) { 
  means.long<-melt(dataset[,.(insitu, match, station_nm)],id.vars="station_nm")
  
  n_sat_samples_histogram <-ggplot(means.long, aes(fill=variable, y=value, x=station_nm)) + 
    geom_bar(position="dodge", stat="identity", width=0.8)+
    geom_text(aes(label=value), position = position_dodge(width = 1),
              vjust = -0.5, size = 3, colour="#777777")+
    theme_clean()+
    scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100))+
    scale_fill_manual(values = c("#1f78b4", "#b2df8a"), name = "Legenda", labels = c("In Situ", "Match"),
                      guide = guide_legend(reverse = FALSE))+
    theme(
      legend.position = c(.9, .85),
      legend.key.size = unit(5, 'mm'),
      legend.title = element_text(size=10), #change legend title font size
      legend.text = element_text(size=8),
      plot.background = element_blank(),
      axis.text.x = element_text(angle = 60, hjust =1, size=11),
      axis.text.y = element_text(size=10),
      axis.title = element_text(size=11))+
    labs(
      x = NULL,
      y = 'Quantidade de Amostras'
    )
  
  plot(n_sat_samples_histogram)
  
  # Save satellite images/site histogram
  ggsave(n_sat_samples_histogram, filename = paste0(wd_figures,'n_sat_samples_histogram.png'), width = 6, height = 4)
  
} 

#### PLOT DATA SET IMAGE ####
plotDataSetImage <- function(dataset) { 
  means.long<-melt(dataset[,.(image_landsat5, image_landsat7, station_nm)],id.vars="station_nm")
  
  # Grouped
  n_sat_samples_histogram <-ggplot(means.long, aes(fill=variable, y=value, x=station_nm)) + 
    geom_bar(position="dodge", stat="identity", width=0.8)+
    geom_text(aes(label=value), position = position_dodge(width = 1),
              vjust = -0.5, size = 3, colour="#777777")+
    theme_clean()+
    scale_y_continuous(breaks = seq(0, 300, by = 50), limits = c(0, 300))+
    scale_fill_manual(values = c("#08519c", "#6baed6"), name = "Legenda", labels = c("Landsat 5", "Landsat 7"),
                      guide = guide_legend(reverse = FALSE))+
    theme(
      legend.position = c(.9, .85),
      legend.key.size = unit(5, 'mm'),
      legend.title = element_text(size=10), #change legend title font size
      legend.text = element_text(size=8),
      plot.background = element_blank(),
      axis.text.x = element_text(angle = 60, hjust =1, size=11),
      axis.text.y = element_text(size=10),
      axis.title = element_text(size=11))+
    labs(
      x = NULL,
      y = 'Quantidade de Amostras'
    )
  
  plot(n_sat_samples_histogram)
  
  # Save satellite images/site histogram
  ggsave(n_sat_samples_histogram, filename = paste0(wd_figures,'n_sat_samples_histogram.png'), width = 6, height = 4)
  
} 

#### PLOT DATA SET BY STATION ####
plotDataSetByStation <- function(ls_sr_insitu_data) { 
  dataset_plot <- ggplot(data=ls_sr_insitu_data)+
    geom_boxplot(aes(x = as.factor(station_nm), y = ConcentracaoMatSuspensao, fill=station_nm))+
    scale_y_continuous(breaks = seq(0, 1200, by = 100), limits = c(0, 1200))+
    scale_fill_manual(values=as.vector(stepped(16)))+
    theme_clean()+ 
    theme(
      legend.position = 'none',
      plot.background = element_blank(),
      axis.text.x = element_text(angle = 60, hjust =1, size=11),
      axis.text.y = element_text(size=10),
      axis.title = element_text(size=11))+
    labs(
      x = NULL,
      y = 'Concentra??o de Sedimentos (mg/L)'
    )
  
  ggsave(dataset_plot, filename = paste0(wd_exports,'insitu_by_station.png'), width = 8, height = 6)
  
} 

#### PLOT ERROR LM OUTLIERS ####
plotErrorLMOutliers <- function(ls_sr_insitu_data, r_squared, rmse){
  errro_lm_outliers_plot <-ggplot(data = ls_sr_insitu_data)+
    geom_abline(intercept = 0, slope = 1, color = '#2166ac', size=1)+
    geom_point(aes(y=pred, x = log10_SSC_mgL, fill=diff<0.79,  colour=diff<0.8), pch = 21, size = 5)+
    geom_text(aes(x = -0.23, y = 3, label = r_squared), hjust = 0, vjust = 0, size=5, color="#555555")+
    geom_text(aes(x = -0.23, y = 2.8, label = rmse), hjust = 0, vjust = 0, size=5, color="#555555")+
    scale_y_continuous(breaks = seq(-0.4, 3.2, by = 0.2), limits = c(-0.4, 3.2), expand = expansion(add = c(0,0)))+
    scale_x_continuous(breaks = seq(-0.4, 3.2, by = 0.2), limits = c(-0.4, 3.2), expand = expansion(add = c(0,0)))+
    scale_fill_manual(values=c("#d6604d", alpha("#abdda4", 0.5)))+
    scale_colour_manual(values=c("#d6604d", alpha("#abdda4", 0.5)))+
    theme_clean()+
    theme(
      axis.text = element_text(size=12),
      axis.title = element_text(size=14),
      plot.background = element_blank(),
      panel.grid.major.y = element_blank(), 
      legend.title = element_blank(),
      legend.position = 'none'
    )+
    labs(
      x = 'In Situ SSC (log(mg/L))',
      y = "Estimated SSC (log(mg/L))"
    ) 
  plot(errro_lm_outliers_plot)
  ggsave(errro_lm_outliers_plot, filename = paste0(wd_exports, 'error_lm_outliers_plot.png'), width = 6, height = 6)
  
  
}

#### PLOT ERROR LM ####
plotErrorLM <- function(ls_sr_insitu_data, r_squared, rmse){
  errro_lm_plot <- ggplot(data = ls_sr_insitu_data)+
    geom_abline(intercept = 0, slope = 1, color = '#2166ac', size=1)+
    geom_point(aes(x=log10_SSC_mgL, y = pred), fill="#abdda4", color="#1b7837", alpha=0.7, pch = 21, size = 5)+
    geom_text(aes(x = -0.23, y = 3, label = r_squared), hjust = 0, vjust = 0, size=5, color="#555555")+
    geom_text(aes(x = -0.23, y = 2.8, label = rmse), hjust = 0, vjust = 0, size=5, color="#555555")+
    scale_y_continuous(breaks = seq(-0.4, 3.2, by = 0.2), limits = c(-0.4, 3.2), expand = expansion(add = c(0,0)))+
    scale_x_continuous(breaks = seq(-0.4, 3.2, by = 0.2), limits = c(-0.4, 3.2), expand = expansion(add = c(0,0)))+
    theme_clean()+
    theme(
      axis.text = element_text(size=12),
      axis.title = element_text(size=14),
      plot.background = element_blank(),
      panel.grid.major.y = element_blank(), 
      legend.title = element_blank(),
      legend.position = 'none'
    )+
    labs(
      x = 'In Situ SSC (log(mg/L))',
      y = "Estimated SSC (log(mg/L))"
    )
  
  plot(errro_lm_plot)
  
  ggsave(errro_lm_plot, filename = paste0(wd_exports, 'error_lm_plot.png'), width = 6, height = 6)
  
}

generatePredictedHistoricalSerieByStation <- function(landsat_serie, vazao_data, vazao_serie_decomposta, landsat_serie_decomposta){
  stations_predicted <- data.frame(landsat_serie[, .(station_nm), .(station_nm)])[,1]
  
  for(station in stations_predicted){
    
    predictedSSC_station <- landsat_serie[station_nm==station]
    vazao_data_station <- vazao_data[EstacaoCodigo==unique(predictedSSC_station$site_no)]
    # max_ssc_prediction_by_year_station <- filter(max_ssc_prediction_by_year,station_nm == station)
    
    wd_exports_station <- paste0(wd_exports, station, '/')
    
    if(!dir.exists(wd_exports_station)){
      dir.create(wd_exports_station)
    }
    
    predictplot <-ggplot() + 
      
      geom_line(data=predictedSSC_station, aes(x=landsat_dt, y=10^ssc_prediction), size=0.5, color="#888888") +
      geom_point(data=predictedSSC_station, aes(x=landsat_dt, y=10^ssc_prediction, colour=sensor), size=2) +
      geom_line(data=landsat_serie_decomposta, aes(x=landsat_dt, y=trend,  colour="SSC Linear Regression"), size=1.2)+
      
      scale_y_continuous(limits=c(0, 1800), breaks = seq(0, 1800, by = 200), expand=c(0,0))+
      scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                   limits = as.Date(c("1984-01-01","2021-01-01")), expand = c(0, 0))+
      
      scale_color_manual(name="Legenda", values = c("Landsat 5"="#66c2a4", "Landsat 7"="#41ae76", "SSC Linear Regression"="#cb181d"),
                         guide = guide_legend(override.aes = list(linetype = c(0, 0, 1), size = c(4, 4, 1.2), shape=c(16,16,NA))))+
      
      ylab("Estimated SSC (mg/L)")+
      theme_clean()+
      theme(
        legend.position = "top",
        legend.margin = margin(0, 0,0,0,"cm"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.background = element_rect(color = NA),
        panel.border = element_blank(),
        axis.text =  element_text(size=10),
        axis.title = element_text(size=12, face = 'bold'),
        axis.title.y.right = element_text(color="#000000"),
        axis.title.y.left = element_text(color="#000000"),
        # axis.title.x.bottom = element_blank(),
        axis.text.x=element_text(angle=60, hjust=1),
        plot.background = element_blank())
    
    
    vazaoplot <-ggplot() + 
      geom_area(aes(x=vazao_data_station$Data, y=vazao_data_station$Media, fill="Mean Monthly Discharge") ) +
      geom_line(data=vazao_serie_decomposta, aes(x=data, y=trend,  colour="Discharge Linear Regression"), size=1.2)+
      
      scale_y_continuous(limits=c(0, 1200), breaks = seq(0, 1200, by = 100), expand=c(0,0))+
      scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                   limits = as.Date(c("1984-01-01","2021-01-01")), expand = c(0, 0))+
      
      scale_color_manual(name="Legenda", values = c("Discharge Linear Regression"="#6a51a3"),
                         guide = guide_legend(override.aes = list(linetype = c(1), size = c(1.2), shape=c(NA))))+

      scale_fill_manual(name="Legenda", values = c("Mean Monthly Discharge"="#9ecae1"))+
      
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

    plot(vazaoplot)
    break;
    
  }
}
  
generateLinearPredictedHistoricalSerieByStation <- function(landsat_serie, vazao_data, vazao_serie_decomposta, landsat_serie_decomposta){
  stations_predicted <- data.frame(landsat_serie[, .(station_nm), .(station_nm)])[,1]
  
  for(station in stations_predicted){
    
    predictedSSC_station <- landsat_serie[station_nm==station]
    vazao_data_station <- vazao_data[EstacaoCodigo==unique(predictedSSC_station$site_no)]
    # max_ssc_prediction_by_year_station <- filter(max_ssc_prediction_by_year,station_nm == station)
    
    wd_exports_station <- paste0(wd_exports, station, '/')
    
    if(!dir.exists(wd_exports_station)){
      dir.create(wd_exports_station)
    }
    
    # predictplot <-ggplot() + 
    #   
    #   geom_line(data=predictedSSC_station, aes(x=landsat_dt, y=10^ssc_prediction), size=0.5, color="#888888") +
    #   geom_point(data=predictedSSC_station, aes(x=landsat_dt, y=10^ssc_prediction, colour=sensor), size=2) +
    #   geom_line(data=landsat_serie_decomposta, aes(x=landsat_dt, y=trend,  colour="SSC Linear Regression"), size=1.2)+
    #   
    #   scale_y_continuous(limits=c(0, 1800), breaks = seq(0, 1800, by = 200), expand=c(0,0))+
    #   scale_x_date(date_labels = "%Y", date_breaks = "1 year",
    #                limits = as.Date(c("1984-01-01","2021-01-01")), expand = c(0, 0))+
    #   
    #   scale_color_manual(name="Legenda", values = c("Landsat 5"="#66c2a4", "Landsat 7"="#41ae76", "SSC Linear Regression"="#cb181d"),
    #                      guide = guide_legend(override.aes = list(linetype = c(0, 0, 1), size = c(4, 4, 1.2), shape=c(16,16,NA))))+
    #   
    #   ylab("Estimated SSC (mg/L)")+
    #   theme_clean()+
    #   theme(
    #     legend.position = "top",
    #     legend.margin = margin(0, 0,0,0,"cm"),
    #     legend.key = element_blank(),
    #     legend.title = element_blank(),
    #     legend.key.width = unit(1, "cm"),
    #     legend.background = element_rect(color = NA),
    #     panel.border = element_blank(),
    #     axis.text =  element_text(size=10),
    #     axis.title = element_text(size=12, face = 'bold'),
    #     axis.title.y.right = element_text(color="#000000"),
    #     axis.title.y.left = element_text(color="#000000"),
    #     # axis.title.x.bottom = element_blank(),
    #     axis.text.x=element_text(angle=60, hjust=1),
    #     plot.background = element_blank())
    
    
    vazaoplot <-ggplot() + 
      geom_area(aes(x=vazao_data_station$Data, y=vazao_data_station$anual, fill="Mean Monthly Discharge") ) +
      #geom_line(data=vazao_serie_decomposta, aes(x=data, y=trend,  colour="Discharge Linear Regression"), size=1.2)+
      
      scale_y_continuous(limits=c(0, 1200), breaks = seq(0, 1200, by = 100), expand=c(0,0))+
      scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                   limits = as.Date(c("1984-01-01","2021-01-01")), expand = c(0, 0))+
      
      # scale_color_manual(name="Legenda", values = c("Discharge Linear Regression"="#6a51a3"),
      #                    guide = guide_legend(override.aes = list(linetype = c(1), size = c(1.2), shape=c(NA))))+
      
      scale_fill_manual(name="Legenda", values = c("Mean Monthly Discharge"="#9ecae1"))+
      
      ylab("Mean Discharge (m³/s)")+
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
    
    plot(vazaoplot)
    break;
  }
}

generateBoxPlotPredictedHistoricalSerieByStation <- function(landsat_serie, vazao_data, max_ssc_prediction_by_year, ssc_vazao, pluviometria_data){
  
    predictedSSC_station <- landsat_serie
    vazao_data_station <- vazao_data[EstacaoCodigo==unique(predictedSSC_station$site_no)]
    
    predictboxplot <-ggplot(data=predictedSSC_station) + 
      geom_boxplot(aes(x=as.factor(landsat_dt_year), y=media_ssc_prediction), fill="#a6dba0", color="#1b7837")+
      scale_y_continuous(limits=c(0, 2000), breaks = seq(0, 2000, by = 200), expand=c(0,0))+
      
      ylab("Estimated SSC (mg/L)")+
      theme_clean()+
      theme(
        legend.position = "top",
        legend.margin = margin(0, 0,0,0,"cm"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.background = element_rect(color = NA),
        panel.border = element_blank(),
        axis.text =  element_text(size=12),
        axis.title = element_text(size=14, face = 'bold'),
        axis.title.y.right = element_text(color="#000000"),
        axis.title.y.left = element_text(color="#000000"),
        axis.title.x.bottom = element_blank(),
        axis.text.x=element_text(angle=60, hjust=1),
        plot.background = element_blank())
    
    
    predictpointplot <-ggplot(data=predictedSSC_station) +
      geom_point(aes(x=landsat_dt, y=media_ssc_prediction), size=4, color="#bababa") +
      geom_point(data=max_ssc_prediction_by_year, aes(x=landsat_dt, y=10^ssc_prediction), size=5, color="#878787") +
      
      stat_smooth(aes(x=landsat_dt, y=media_ssc_prediction), color="#f46d43" , method="lm", se=F, size=1.5, formula = y~x)+
      stat_smooth(data=max_ssc_prediction_by_year, aes(x=landsat_dt, y=10^ssc_prediction),  color="#d73027" , method="lm", se=F, size=2, formula = y~x)+
      
      scale_y_continuous(limits=c(0, 2000), breaks = seq(0, 2000, by = 200), expand=c(0,0))+
      scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                     limits = as.Date(c("1984-01-01","2021-01-01")), expand = c(0, 0))+
      
      ylab("Estimated SSC (mg/L)")+
      theme_clean()+
      theme(
        legend.position = "top",
        legend.margin = margin(0, 0,0,0,"cm"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.background = element_rect(color = NA),
        panel.border = element_blank(),
        axis.text =  element_text(size=12),
        axis.title = element_text(size=14, face = 'bold'),
        axis.title.y.right = element_text(color="#000000"),
        axis.title.y.left = element_text(color="#000000"),
        axis.title.x.bottom = element_blank(),
        axis.text.x=element_text(angle=60, hjust=1),
        plot.background = element_blank())
    
    
    vazaoplot <-ggplot(data=vazao_data) + 
      geom_boxplot(aes(x=as.factor(year), y=Media), fill="#c2a5cf", color="#762a83")+
      scale_y_continuous(limits=c(0, 1100), breaks = seq(0, 1000, by = 100), expand=c(0,0))+
      
      ylab("Discharge (m³/s)")+
      theme_clean()+
      theme(
        legend.position = "top",
        legend.margin = margin(0, 0,0,0,"cm"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.background = element_rect(color = NA),
        panel.border = element_blank(),
        axis.text =  element_text(size=12),
        axis.title = element_text(size=14, face = 'bold'),
        axis.title.y.right = element_text(color="#000000"),
        axis.title.y.left = element_text(color="#000000"),
        axis.title.x.bottom = element_blank(),
        axis.text.x=element_text(angle=60, hjust=1),
        plot.background = element_blank())
    
    precipitacaoplot <-ggplot(data=pluviometria_data) + 
      geom_boxplot(aes(x=as.factor(year), y=Total), fill="#c2a5cf", color="#762a83")+
      scale_y_continuous(limits=c(0, 600), breaks = seq(0, 1000, by = 100), expand=c(0,0))+
      
      ylab("Precipitation (mm)")+
      theme_clean()+
      theme(
        legend.position = "top",
        legend.margin = margin(0, 0,0,0,"cm"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.background = element_rect(color = NA),
        panel.border = element_blank(),
        axis.text =  element_text(size=12),
        axis.title = element_text(size=14, face = 'bold'),
        axis.title.y.right = element_text(color="#000000"),
        axis.title.y.left = element_text(color="#000000"),
        axis.title.x.bottom = element_blank(),
        axis.text.x=element_text(angle=60, hjust=1),
        plot.background = element_blank())
    
    ssc_vazao <- ggplot(data=ssc_vazao) + 
      geom_point(aes(x=Media, y=log(media_ssc_prediction), colour="blue") ) +
      stat_smooth(aes(x=Media, y=log(media_ssc_prediction), colour="red",) , method="lm", se=F, size=1.5, formula = y~x)+
      
      
      stat_poly_eq(aes(x=Media, y=log(media_ssc_prediction)),
                   label.x.npc = 0.025, label.y.npc = 0.9, colour="#FF0000",
                   formula = y~x, parse = TRUE, size = 4) +
      stat_correlation(aes(x=Media, y=log(media_ssc_prediction)),
                       label.x = 0.025, label.y = 0.95, colour="#FF0000", parse = TRUE, size = 4, small.r = TRUE) +
      # geom_line(data=vazao_serie_decomposta, aes(x=data, y=trend,  colour="green"), size=1.2)+
      
      # scale_y_continuous(limits=c(0, 700), breaks = seq(0, 700, by = 50), expand=c(0,0))+
      # scale_x_continuous(limits=c(1966, 2021), breaks = seq(1966, 2021, by = 1), expand=c(0,0))+
      # scale_x_date(date_labels = "%Y", date_breaks = "1 year",
      #              limits = as.Date(c("1984-01-01","2021-01-01")), expand = c(0, 0))+
      
      # scale_color_manual(name="Legenda", values = c("Discharge Linear Regression"="#6a51a3"),
      #                    guide = guide_legend(override.aes = list(linetype = c(1), size = c(1.2), shape=c(NA))))+
      
      ylab("Log (Estimated SSC (mg/L))")+
      xlab("Discharge (m³/s)")+
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
    
    plot(precipitacaoplot)
}

generatePredictedHistoricalSerie <- function(landsat_serie, vazao_data, max_ssc_prediction_by_year, pluviometria_data, ssc_discharge_serie){
  stations_predicted <- data.frame(landsat_serie[, .(station_nm), .(station_nm)])[,1]
  
  for(station in stations_predicted){
    predictedSSC_station <- landsat_serie[station_nm==station]
    vazao_data_station <- vazao_data[EstacaoCodigo==unique(predictedSSC_station$site_no)]
    max_ssc_prediction_by_year_station <- filter(max_ssc_prediction_by_year,station_nm == station)
    
    wd_exports_station <- paste0(wd_exports, station, '/')
    
    if(!dir.exists(wd_exports_station)){
      dir.create(wd_exports_station)
    }
    
    pluviometria_cSm <- pass.filt(y=pluviometria_data$Total, W=0.01, type="low", method="ChebyshevI")
    
    pluviometria_data_1974 <- pluviometria_data[Data<"1974-01-01"]
    pluviometria_data_2021 <- pluviometria_data[Data>"1976-01-01"]
    
    precipitacaoplot <-ggplot() +
      geom_line(aes(x=pluviometria_data_1974$Data, y=pluviometria_data_1974$Total, color="Rainfall"), size=1) +
      geom_line(aes(x=pluviometria_data_2021$Data, y=pluviometria_data_2021$Total, color="Rainfall"), size=1) +
      
      geom_line(aes(x=pluviometria_data$Data, y=pluviometria_cSm, color="Rainfall Low Pass Filtered"), size=1.5) +
      
      stat_smooth(data=pluviometria_data_1974, aes(x=Data, y=Total, color="Rainfall Linear Regression"),
                  formula = y~x, method="lm", se=F, size=1.2, linetype = "dashed")+
      stat_poly_eq(data=pluviometria_data_1974, aes(x=Data, y=Total, label=paste("(1966-1974)",..rr.label.., sep = "~~~")),
                   label.x.npc = 0.015, label.y.npc = 1, colour="#08306b",
                   formula = y~x, parse = TRUE, size = 4) +
      
      stat_smooth(data=pluviometria_data_2021, aes(x=Data, y=Total, color="Rainfall Linear Regression"),
                  formula = y~x, method="lm", se=F, size=1.2, linetype = "dashed")+
      stat_poly_eq(data=pluviometria_data_2021, aes(x=Data, y=Total, label=paste("(1976-2021)",..rr.label.., sep = "~~~")),
                   label.x.npc = 0.015, label.y.npc = 0.9, colour="#08306b",
                   formula = y~x, parse = TRUE, size = 4) +

      scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                   limits = as.Date(c("1966-01-01","2021-01-01")), expand = c(0, 0))+
      scale_y_continuous(limits=c(0, 600), breaks = seq(0, 600, by = 100), expand=c(0,0))+
      
      scale_color_manual(name=NULL, values = c("Rainfall"="#9ecae1", "Rainfall Low Pass Filtered"="#2171b5", 
                                               "Rainfall Linear Regression"="#08306b"),
                         guide = guide_legend(override.aes = list(linetype = c(1, 2, 2), size = c(1.2, 1.2, 1.2))))+
      
      ylab("Monthly Rainfall \n (mm)") +
      custom_theme +
      theme( axis.title.x=element_blank())
    
    
    vazao_bSm <- pass.filt(y=vazao_data$Media, W=0.01, type="low", method="Butterworth")
    vazao_cSm <- pass.filt(y=vazao_data$Media, W=0.01, type="low", method="ChebyshevI")
   
    vazaoplot <-ggplot() +
      geom_line(aes(x=vazao_data_station$Data, y=vazao_data_station$Media, color="Discharge"), size=1) +
      geom_line(aes(x=vazao_data_station$Data, y=vazao_cSm, color="Discharge Low Pass Filtered"), size=1.5) +
      
      stat_smooth(data=vazao_data_station, aes(x=Data, y=Media, colour="Discharge Linear Regression"),
                  formula = y~x, method="lm", se=F, size=1.2, linetype = "dashed")+
      stat_poly_eq(data=vazao_data_station, aes(x=Data, y=Media),
                                label.x.npc = 0.015, label.y.npc = 0.97, colour="#3f007d",
                                formula = y~x, parse = TRUE, size = 4) +
      
      scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                   limits = as.Date(c("1966-01-01","2021-01-01")), expand = c(0, 0))+
      scale_y_continuous(limits=c(0, 1200), breaks = seq(0, 1200, by = 200), expand=c(0,0))+
      
      scale_color_manual(name=NULL, values = c("Discharge"="#bcbddc", "Discharge Low Pass Filtered"="#6a51a3", 
                                               "Discharge Linear Regression"="#3f007d"),
                         guide = guide_legend(override.aes = list(linetype = c(1, 2, 2), size = c(1.2, 1.2, 1.2))))+
      
      ylab("Monthly Discharge \n (m³/s)")+
      custom_theme +
      theme( axis.title.x=element_blank())
    
    
    
    ssc_cSm <- pass.filt(y=10^predictedSSC_station$ssc_prediction, W=0.01, type="low", method="ChebyshevI")
    
    predictplot <-ggplot() +
      geom_line(data=predictedSSC_station, aes(x=landsat_dt, y=10^ssc_prediction), size=1, color="#fee8c8") +
      geom_point(data=predictedSSC_station, aes(x=landsat_dt, y=10^ssc_prediction, colour=sensor), size=1) +
      
      geom_line(aes(x=predictedSSC_station$landsat_dt, y=ssc_cSm, color="SSC Low Pass Filtered"), size=1.5) +
      
      stat_smooth(data=predictedSSC_station, aes(x=landsat_dt, y=10^ssc_prediction, color="SSC Linear Regression"), method="lm",
                  linetype = "dashed", se=F, size=1.2, formula = y~x)+
      stat_poly_eq(data=predictedSSC_station, aes(x=landsat_dt, y=10^ssc_prediction, label=paste("(SSC)",..rr.label.., sep = "~~~")),
                   label.x.npc = 0.015, label.y.npc = 1, colour="#b30000",
                   formula = y~x, parse = TRUE, size = 4) +
      
      geom_point(data=max_ssc_prediction_by_year_station, aes(x=landsat_dt,y=10^ssc_prediction, color="SSC Annual Max"), size=2)+
      stat_smooth(data=max_ssc_prediction_by_year_station, aes(x=landsat_dt,y=10^ssc_prediction, color="SSC Annual Max Linear Regression"),
                  formula = y~x, method="lm", se=F, size=1.2, linetype = "dashed")+
      stat_poly_eq(data=max_ssc_prediction_by_year_station, 
                   aes(x=landsat_dt,y=10^ssc_prediction,label=paste("(SSC~~~Annual~~~Max)",..rr.label.., sep = "~~~")),
                     label.x.npc = 0.015, label.y.npc = 0.9,
                     formula = y~x, parse = TRUE, size = 4, color="#fb6a4a") +
       
      scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                   limits = as.Date(c("1966-01-01","2021-01-01")), expand = c(0, 0))+
      scale_y_continuous(limits=c(0, 2400), breaks = seq(0, 2400, by = 400), expand=c(0,0))+
      
      scale_color_manual(name=NULL, values = c("Landsat 5"="#fdbb84", "Landsat 7"="#fc8d59", "SSC Linear Regression"="#b30000", 
                                               "SSC Low Pass Filtered"="#ef6548", "SSC Annual Max"="#ef3b2c", 
                                               "SSC Annual Max Linear Regression"="#fb6a4a"),
                         guide = guide_legend(override.aes = list(linetype = c(0, 0, 2, 2, 0, 2), size = c(2, 2, 1.2, 1.2, 3, 1.2),
                                                                  shape=c(16, 16, NA, NA, 16, NA))))+
      ylab("Estimated SSC \n (mg/L)")+
      custom_theme +
      theme( axis.title.x=element_blank())
    

    ssc_discharge_cSm <- pass.filt(y=ssc_discharge_serie$Descarga, W=0.01, type="low", method="ChebyshevI")
    
    ssc_discharge_plot <-ggplot() +
      geom_line(aes(x=ssc_discharge_serie$Data, y=ssc_discharge_serie$Descarga, color="SSD"), size=1) +
      
      geom_line(aes(x=ssc_discharge_serie$Data, y=ssc_discharge_cSm, colour="SSD Low Pass Filtered"), size=1.5) +
      stat_smooth(data=ssc_discharge_serie, aes(x=Data, y=Descarga, colour="SSD Linear Regression"),
                  formula = y~x, method="lm", se=F, size=1.2, linetype = "dashed")+
      stat_poly_eq(data=ssc_discharge_serie, aes(x=Data, y=Descarga),
                   label.x.npc = 0.015, label.y.npc = 0.97,
                   formula = y~x, parse = TRUE, size = 4, color="#7f2704") +
      
      scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                   limits = as.Date(c("1966-01-01","2021-01-01")), expand = c(0, 0))+
      scale_y_continuous(limits=c(0, 100000), breaks = seq(0, 100000, by = 20000), expand=c(0,0))+
      
      scale_color_manual(name=NULL, values = c("SSD"="#fd8d3c", "SSD Low Pass Filtered"="#d94801", 
                                               "SSD Linear Regression"="#7f2704"),
                         guide = guide_legend(override.aes = list(linetype = c(1, 2, 2), size = c(1.2, 1.2, 1.2))))+
      
      ylab("SSD \n (ton/day)") +
      custom_theme +
      theme( axis.title.x=element_blank())
    
    plotarrange <- ggarrange(precipitacaoplot, vazaoplot, predictplot, ssc_discharge_plot, nrow=4, ncol=1,
                             labels = c("A", "B", "C", "D"), font.label = list(size = 18))
    ggsave(plotarrange, filename = paste0(wd_figures,'plotarrange.tiff'), width = 11, height = 14)
    
    
    break;
    
  }
}

generateSSCvsDischargePlot <- function(ssc_discharge){
  ssc_dischargeplot <- ggplot() + 
    geom_point(data=ssc_discharge, aes(x=Media, y=media_ssc_prediction),  fill="#99d8c9", color="#006d2c", alpha=0.7, pch = 21, size = 5 ) +
    stat_smooth(data=ssc_discharge, aes(x=Media, y=media_ssc_prediction), color="#e31a1c", method="lm", se=F, size=1.5, formula = y~x)+
    
    scale_y_continuous(limits=c(0, 2000), breaks = seq(0, 2000, by = 200), expand=c(0,0))+
    scale_x_continuous(limits=c(0, 1200), breaks = seq(0, 1200, by = 100), expand=c(0,0))+
    
    stat_poly_eq(data=ssc_discharge, aes(x=Media, y=media_ssc_prediction),
                 label.x.npc = 0.985, label.y.npc = 0.975, colour="#e31a1c",
                 formula = y~x, parse = TRUE, size = 4) +
    stat_correlation(data=ssc_discharge, aes(x=Media, y=media_ssc_prediction),
                     label.x = 0.985, label.y = 0.925, colour="#e31a1c", parse = TRUE, size = 4, small.r = TRUE) +
    
    ylab("Estimated SSC (mg/L)")+
    xlab("Monthly Discharge (m³/s)")+
    custom_theme
  
    ggsave(ssc_dischargeplot, filename = paste0(wd_figures,'ssc_dischargeplot.tiff'), width = 6, height = 6)
  
  
}


generateSSCvsRainfallPlot <- function(ssc_rainfall){
  ssc_rainfallplot <- ggplot() + 
    geom_point(data=ssc_rainfall, aes(x=Total, y=media_ssc_prediction),  fill="#99d8c9", color="#006d2c", alpha=0.7, pch = 21, size = 5 ) +
    stat_smooth(data=ssc_rainfall, aes(x=Total, y=media_ssc_prediction), color="#e31a1c", method="lm", se=F, size=1.5, formula = y~x)+
    
    scale_y_continuous(limits=c(0, 2000), breaks = seq(0, 2000, by = 200), expand=c(0,0))+
    scale_x_continuous(limits=c(0, 400), breaks = seq(0, 400, by = 50), expand=c(0,0))+
    
    stat_poly_eq(data=ssc_rainfall, aes(x=Total, y=media_ssc_prediction),
                 label.x.npc = 0.985, label.y.npc = 0.975, colour="#e31a1c",
                 formula = y~x, parse = TRUE, size = 4) +
    stat_correlation(data=ssc_rainfall, aes(x=Total, y=media_ssc_prediction),
                     label.x = 0.985, label.y = 0.925, colour="#e31a1c", parse = TRUE, size = 4, small.r = TRUE) +
    
    ylab("Estimated SSC (mg/L)")+
    xlab("Monthly Rainfall (mm)")+
    custom_theme
  
  ggsave(ssc_rainfallplot, filename = paste0(wd_figures,'ssc_rainfallplot.tiff'), width = 6, height = 6)
  
}


generateSSCvsSSDPlot <- function(ssc_ssd){
  ssc_ssdplot <- ggplot() + 
    geom_point(data=ssc_ssd, aes(x=Descarga, y=media_ssc_prediction),  fill="#99d8c9", color="#006d2c", alpha=0.7, pch = 21, size = 5 ) +
    stat_smooth(data=ssc_ssd, aes(x=Descarga, y=media_ssc_prediction), color="#e31a1c", method="lm", se=F, size=1.5, formula = y~x)+
    
    scale_y_continuous(limits=c(0, 2000), breaks = seq(0, 2000, by = 200), expand=c(0,0))+
    scale_x_continuous(limits=c(0, 80000), breaks = seq(0, 80000, by = 10000), expand=c(0,0))+
    
    stat_poly_eq(data=ssc_ssd, aes(x=Descarga, y=media_ssc_prediction),
                 label.x.npc = 0.985, label.y.npc = 0.975, colour="#e31a1c",
                 formula = y~x, parse = TRUE, size = 4) +
    stat_correlation(data=ssc_ssd, aes(x=Descarga, y=media_ssc_prediction),
                     label.x = 0.985, label.y = 0.925, colour="#e31a1c", parse = TRUE, size = 4, small.r = TRUE) +
    
    ylab("Estimated SSC (mg/L)")+
    xlab("SSD (ton/day)")+
    custom_theme
  
  ggsave(ssc_ssdplot, filename = paste0(wd_figures,'ssc_ssdplot.tiff'), width = 6, height = 6)
  
  
}

custom_theme <- theme_clean() +
  theme(
    text=element_text(family="Arial"),
    axis.text =  element_text(size=12),
    axis.title = element_text(size=14, face = 'bold'),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.text.x=element_text(angle=60, hjust=1),
    plot.background = element_rect(fill = "white", colour = "white"),
    legend.text = element_text(size=12),
    legend.key.height = unit(0.2, "cm"),
    legend.key.width = unit(1.2, "cm"),
    legend.justification= c(1, 1),
    legend.position = c(1, 1),
    legend.box = "horizontal",
    legend.direction = "horizontal")

