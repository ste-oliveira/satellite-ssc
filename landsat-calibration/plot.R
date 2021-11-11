
plotDataSet <- function(dataset) { 
   means.long<-melt(dataset[,.(image, insitu, match, site_no)],id.vars="site_no")
   
   # Grouped
   n_sat_samples_histogram <-ggplot(means.long, aes(fill=variable, y=value, x=reorder(site_no, -value))) + 
      geom_bar(position="dodge", stat="identity", width=0.5, size=0.2, colour="black")+
      theme_clean()+
      scale_y_continuous(breaks = seq(0, 700, by = 50))+
      scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a"), name = "Legenda", labels = c("Imagem Landsat 5/7", "Dados In Situ", "Match"),
                        guide = guide_legend(reverse = FALSE))+
      theme(
         legend.position = c(.85, .85),
         legend.key.size = unit(5, 'mm'),
         legend.title = element_text(size=10), #change legend title font size
         legend.text = element_text(size=8),
         plot.background = element_blank())+
      labs(
         x = NULL,
         y = 'Quantidade de Amostras'
      )
   
   plot(n_sat_samples_histogram)
   
   # Save satellite images/site histogram
   ggsave(n_sat_samples_histogram, filename = paste0(wd_figures,'n_sat_samples_histogram.png'), width = 6, height = 4)
   
   return(dataset)
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


