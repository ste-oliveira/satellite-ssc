
plotDataSet <- function(ls_raw_1, taquari_insitu_raw, ls_insitu_raw) { 
   # Plot number of satellite samples per site as a histogram
   # Renan - Plotar número de amostras por estação
   imagens <-  ls_raw_1[landsat_dt>="2005-01-01" & landsat_dt<="2019-12-31",.(image = .N), by = .(station_nm)][order(station_nm)]
   insitu <- taquari_insitu_raw[order(station_nm),.(insitu = .N), by = .(station_nm)]
   match <- ls_insitu_raw[order(station_nm),.(match = .N), by = .(station_nm)]
   
   dataset <- cbind(imagens, insitu[,c(2)], match[,c(2)])
   means.long<-melt(dataset,id.vars="station_nm")
   
   # Grouped
   n_sat_samples_histogram <-ggplot(means.long, aes(fill=variable, y=value, x=reorder(station_nm, -value))) + 
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
   
   # Save satellite images/site histogram
   ggsave(n_sat_samples_histogram, filename = paste0(wd_figures,'n_sat_samples_histogram.png'), width = 6, height = 4)
   
   return(dataset)
} 
