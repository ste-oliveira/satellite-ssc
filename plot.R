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
    geom_point(aes(x=pred, y = log10_SSC_mgL, fill=diff<0.79,  colour=diff<0.8), pch = 21, size = 5)+
    geom_text(aes(x = -0.23, y = 3, label = r_squared), hjust = 0, vjust = 0, size=5, color="#555555")+
    geom_text(aes(x = -0.23, y = 2.8, label = rmse), hjust = 0, vjust = 0, size=5, color="#555555")+
    scale_y_continuous(breaks = seq(-0.4, 3.2, by = 0.2), limits = c(-0.4, 3.2), expand = expansion(add = c(0,0)))+
    scale_x_continuous(breaks = seq(-0.4, 3.2, by = 0.2), limits = c(-0.4, 3.2), expand = expansion(add = c(0,0)))+
    scale_fill_manual(values=c("#d6604d", alpha("#abdda4", 0.5)))+
    scale_colour_manual(values=c("#d6604d", alpha("#abdda4", 0.5)))+
    theme_clean()+
    theme(
      axis.text = element_text(size=10),
      axis.title = element_text(size=12),
      plot.background = element_blank(),
      panel.grid.major.y = element_blank(), 
      legend.title = element_blank(),
      legend.position = 'none'
    )+
    labs(
      x = "CSS Estimado (log(mg/L))",
      y = 'CSS In Situ (log(mg/L))'
    ) 
  plot(errro_lm_outliers_plot)
  ggsave(errro_lm_outliers_plot, filename = paste0(wd_exports, 'error_lm_outliers_plot.png'), width = 6, height = 6)
  
  
}

#### PLOT ERROR LM ####
plotErrorLM <- function(ls_sr_insitu_data, r_squared, rmse){
  errro_lm_plot <- ggplot(data = ls_sr_insitu_data)+
    geom_abline(intercept = 0, slope = 1, color = '#2166ac', size=1)+
    geom_point(aes(x=pred, y = log10_SSC_mgL), fill="#abdda4", color="#1b7837", alpha=0.7, pch = 21, size = 5)+
    geom_text(aes(x = -0.23, y = 3, label = r_squared), hjust = 0, vjust = 0, size=5, color="#555555")+
    geom_text(aes(x = -0.23, y = 2.8, label = rmse), hjust = 0, vjust = 0, size=5, color="#555555")+
    scale_y_continuous(breaks = seq(-0.4, 3.2, by = 0.2), limits = c(-0.4, 3.2), expand = expansion(add = c(0,0)))+
    scale_x_continuous(breaks = seq(-0.4, 3.2, by = 0.2), limits = c(-0.4, 3.2), expand = expansion(add = c(0,0)))+
    theme_clean()+
    theme(
      axis.text = element_text(size=10),
      axis.title = element_text(size=12),
      plot.background = element_blank(),
      panel.grid.major.y = element_blank(), 
      legend.title = element_blank(),
      legend.position = 'none'
    )+
    labs(
      x = "CSS Estimado (log(mg/L))",
      y = 'CSS In Situ (log(mg/L))'
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
      geom_line(data=landsat_serie_decomposta, aes(x=landsat_dt, y=trend,  colour="Tendência CSS"), size=1.2)+
      
      scale_y_continuous(limits=c(0, 1800), breaks = seq(0, 1800, by = 200), expand=c(0,0))+
      scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                   limits = as.Date(c("1984-01-01","2021-01-01")), expand = c(0, 0))+
      
      scale_color_manual(name="Legenda", values = c("Landsat 5"="#66c2a4", "Landsat 7"="#41ae76", "Tendência CSS"="#cb181d"),
                         guide = guide_legend(override.aes = list(linetype = c(0, 0, 1), size = c(4, 4, 1.2), shape=c(16,16,NA))))+
      
      ylab("CSS Estimada (mg/L)")+
      xlab("Coxim (66870000)")+
      theme_clean()+
      theme(
        legend.position = "right",
        legend.margin = margin(0, 0,0,0,"cm"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.background = element_rect(color = NA),
        axis.text =  element_text(size=10),
        axis.title = element_text(size=12, face = 'bold'),
        axis.title.y.right = element_text(color="#000000"),
        axis.title.y.left = element_text(color="#000000"),
        axis.title.x.bottom = element_blank(),
        axis.text.x=element_text(angle=60, hjust=1),
        plot.background = element_blank())
    
    
    vazaoplot <-ggplot() + 
      geom_area(aes(x=vazao_data_station$Data, y=vazao_data_station$Media, fill="Vazão") ) +
      geom_line(data=vazao_serie_decomposta, aes(x=data, y=trend,  colour="Tendência Vazão"), size=1.2)+
      
      scale_y_continuous(limits=c(0, 1200), breaks = seq(0, 1200, by = 100), expand=c(0,0))+
      scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                   limits = as.Date(c("1984-01-01","2021-01-01")), expand = c(0, 0))+
      
      scale_fill_manual(name="Legenda", values = c( "Vazão"="#ddddff"))+
      scale_color_manual(name="Legenda", values = c("Tendência Vazão"="#6a51a3"),
                         guide = guide_legend(override.aes = list(linetype = c(1), size = c(1.2), shape=c(NA))))+
      
      ylab("Vazão Média (m³/s)")+
      xlab("Coxim (66870000)")+
      theme_clean()+
      theme(
        legend.position = "right",
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
    
    plot(ggarrange(predictplot, vazaoplot, nrow=2, ncol=1))

    break;
    
  }
}
