
plotDataSetInsitu <- function(dataset) { 
   means.long<-melt(dataset[,.(insitu, match, station_nm)],id.vars="station_nm")

   # Grouped
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

plotDataSetImage <- function(dataset) { 
   means.long<-melt(dataset[,.(image_landsat5, image_landsat7, station_nm)],id.vars="station_nm")
   
   # Grouped
   n_sat_samples_histogram <-ggplot(means.long, aes(fill=variable, y=value, x=station_nm)) + 
      geom_bar(position="dodge", stat="identity", width=0.8)+
      geom_text(aes(label=value), position = position_dodge(width = 1),
                vjust = -0.5, size = 3, colour="#777777")+
      theme_clean()+
      scale_y_continuous(breaks = seq(0, 600, by = 50), limits = c(0, 600))+
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
         y = 'Concentração de Sedimentos (mg/L)'
      )
   
   ggsave(dataset_plot, filename = paste0(wd_exports,'insitu_by_station.png'), width = 8, height = 6)
   
} 


plotClusterSSCCategoryColor <- function(ssc_category_color){
   # Create true-color and false-color plots of 'typical' river color for each SSC category at each cluster group
   for(j in 1:2){
      color_sel <- c('true_color','false_color')[j]
      # raster_color_types <- c(geom_raster(aes(fill = rgb(B3_median/3000,B2_median/3000,B1_median/3000))), # true color
      #                         geom_raster(aes(fill = rgb(B4_median/4000,B3_median/4000,B2_median/4000))) # false color))
      # data.table version
      raster_color_types <- c(geom_raster(aes(fill = rgb(B3/3000,B2/3000,B1/3000))), # true color
                              geom_raster(aes(fill = rgb(B4/4000,B3/4000,B2/4000))) # false color)
      )
      
      cluster_ssc_category_color_plot <- 
         ggplot(ssc_category_color, aes(x = as.factor(cluster_sel), y = ssc_category)) +
         # ggplot(ssc_category_color, aes(x = reorder(paste0(cluster_sel, ' ', site_no), cluster_sel), y = ssc_category)) + # for by site
         raster_color_types[j] +
         scale_fill_identity() +
         theme_clean() + 
         theme(
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.background = element_blank()
         )+
         geom_text( aes(label =paste(ssc_category,"(mg/L)")), color="gray", size=2.5)
      # scale_x_continuous(expansion(add = c(0,0))) + 
      # scale_y_discrete(expansion(mult = c(0,0))) +
      # theme(axis.text.x = element_text(angle = 90)) + 
      
      if(j == 1){
         cluster_ssc_category_color_plot <- cluster_ssc_category_color_plot + labs(
            y = 'Concentração de Sedimentos (mg/L)',
            x = 'Cor Típica do Rio (Cor Verdadeira)'
         )
      }else{
         cluster_ssc_category_color_plot <- cluster_ssc_category_color_plot + labs(
            y = 'Concentração de Sedimentos (mg/L)',
            x = 'Cor Típica do Rio (Falsa Cor)'
         )
      }
      
      ggsave(cluster_ssc_category_color_plot, filename = paste0(wd_figures, cluster_col_name,'_ssc_category_color_plot_',color_sel,'.pdf'),
             width = 3, height = 3.4, useDingbats = F)
      ggsave(cluster_ssc_category_color_plot, filename = paste0(wd_figures, cluster_col_name,'_ssc_category_color_plot_',color_sel,'.png'),
             width = 3, height = 3.4)
      if(j == 1){
         ssc_cluster_color_plot_list[[i]] <- cluster_ssc_category_color_plot
      }else{
         ssc_cluster_false_color_plot_list[[i]] <- cluster_ssc_category_color_plot
      }
   }
   
}

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
         x = "SSC Estimado (log(mg/L))",
         y = 'SSC In Situ (log(mg/L))'
      )
    ggsave(errro_lm_outliers_plot, filename = paste0(wd_exports, 'error_lm_outliers_plot.png'), width = 6, height = 6)
   
   
}

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
         x = "SSC Estimado (log(mg/L))",
         y = 'SSC In Situ (log(mg/L))'
      )
   
   ggsave(errro_lm_plot, filename = paste0(wd_exports, 'error_lm_plot.png'), width = 6, height = 6)
   
   
}

plotRelativeError <- function(ssc_model_cl_iterate_rerr){
   # Prepare relative error for plotting
   rel_error_annotate <- data.frame(rel_error = 
                                       paste0('Erro Relativo = ', 
                                              round(ssc_model_cl_iterate_rerr[,.(mape_gl_ind,mape_cl_ind,mape_st_ind)],2))[2]) %>% 
      mutate(holdout25 = c('holdout'), SSC_mgL = 50, pred = 650)
   
   
   #rsme <- paste0('RSME= ', round(Metrics::rmse(10^ssc_model_cl_iterate_pred[,log10_SSC_mgL] ,10^ssc_model_cl_iterate_pred[,pred_cl ]),2))
   #r2 <- paste0('R2= ', round(R2_Score(10^ssc_model_cl_iterate_pred[,pred_cl], 10^ssc_model_cl_iterate_pred[,log10_SSC_mgL]),2))
  
    rsme <- paste0('RSME= ', round(Metrics::rmse(ssc_model_cl_iterate_pred[,log10_SSC_mgL] ,ssc_model_cl_iterate_pred[,pred_cl ]),2))
    r2 <- paste0('R2= ', round(R2_Score(ssc_model_cl_iterate_pred[,pred_cl], ssc_model_cl_iterate_pred[,log10_SSC_mgL]),2))
   # 
   # Plot actual vs. predicted for holdout. Annotate with RMSE.
   ssc_cluster_iterate_plot_holdout <- get_sscPlot(ssc_model_cl_iterate_pred,"byCluster",'no','no') +
      geom_text(aes(x = 50, y = 650, label = rsme), hjust = 0, vjust = 0, size=5, color="#000000")+
      geom_text(aes(x = 50, y = 620, label = r2), hjust = 0, vjust = 0, size=5, color="#000000")
   
   # SAVE FIGURE
   # ggsave(ssc_cluster_iterate_plot_holdout, filename = paste0('ssc_', cluster_col_name, '_iterate_plot_holdout.pdf'), useDingbats = F, 
   #        width = 6, height = 7)
   ggsave(ssc_cluster_iterate_plot_holdout, filename = paste0(wd_exports, 'ssc_', cluster_col_name, '_iterate_plot_holdout.png'), 
          width = 7, height = 7)
}


# Display log axes labels nicely
fancy_scientific <- function(l) { 
   # turn in to character string in scientific notation 
   l <- log10(l)
   # return(parse(text=paste("'Discharge [m'", "^3* s", "^-1 ", "*']'", sep="")))
   return(parse(text = paste("10^",as.character(l),sep = "")))
} 


#### THEMES AND PLOTTING PARAMETERS ####
cbPalette <- c("#0000FF", "#02b31a", "#FF0000")
breaks <- 10^(-10:10)
minor_breaks <- rep(5, 21)*(10^rep(-10:10, each=9))

theme_evan <- theme_clean() +
   theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linetype = 'dashed',color = 'grey70'),
      panel.grid.major.x = element_blank(),
      # panel.grid = element_blank(),
      legend.position = 'none',
      panel.border = element_rect(size = 0.5),
      text = element_text(size=8),
      axis.text = element_text(size = 8), 
      plot.title = element_text(size = 9)
   )

theme_evan_facet <- theme_clean() +
   theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      # panel.grid = element_blank(),
      # legend.position = 'none',
      panel.border = element_rect(size = 0.5),
      strip.background = element_rect(fill = 'white'),
      text = element_text(size=8),
      axis.text = element_text(size = 8), 
      plot.title = element_text(size = 9)
   )

season_facet <- theme_evan_facet + theme(
   legend.position = 'none', 
   strip.background = element_blank(),
   strip.text = element_text(hjust = 0, margin = margin(0,0,0,0, unit = 'pt'))
)


