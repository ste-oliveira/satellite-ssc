
plotDataSet <- function(dataset) { 
   means.long<-melt(dataset[,.(image, insitu, match, station_nm)],id.vars="station_nm")
   
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
   
   plot(n_sat_samples_histogram)
   
   # Save satellite images/site histogram
   ggsave(n_sat_samples_histogram, filename = paste0(wd_figures,'n_sat_samples_histogram.png'), width = 6, height = 4)
   
   return(dataset)
} 


# Display log axes labels nicely
fancy_scientific <- function(l) { 
   # turn in to character string in scientific notation 
   l <- log10(l)
   # return(parse(text=paste("'Discharge [m'", "^3* s", "^-1 ", "*']'", sep="")))
   return(parse(text = paste("10^",as.character(l),sep = "")))
} 


#### THEMES AND PLOTTING PARAMETERS ####

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


