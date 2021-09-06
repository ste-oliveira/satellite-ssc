runClusterAnalysis <- function(site_band_scaling_all, clusters, vars){
   
   #Renan - Diminui a qtd de variaveis utilizadas
   vis_nir_bands <- c('B1','B2','B3','B4','B2.B1')
   site_band_scaling_all <- scale(site_band_quantiles_all[,..vis_nir_bands])
   cluster_var_combinations <- Map(as.data.frame, sapply(seq_along(vis_nir_bands), function(k) t(combn(vis_nir_bands,k))))
   
   #Renan - Diminui o loop de 4 para 3 para funcionar com a quantidade de variaveis
   for(i in 3:length(vis_nir_bands)){
      cluster_var_k_sel <- cluster_var_combinations[[i]]
      for(k in 1:nrow(cluster_var_k_sel)){
         print(paste0(i, " ", k))
         cluster_var_sel <- c(as.matrix(cluster_var_k_sel[k,]))
         
         cluster_var_label <- paste(cluster_var_sel, collapse = "_")
         #Renan - Alterei os parÃ¢metros do nbclust para rodar com a qdt de registros disponiveis min.nc e max.nc
         ccc_result <- data.table(cbind(cluster_var_label, i, c(1:5), NbClust(site_band_scaling_all[,cluster_var_sel],
                                                                              min.nc=2, max.nc=4, index="ccc", method="kmeans")$All.index))
         print(ccc_result)
         colnames(ccc_result) <- c('variables','nvars','nclusters','ccc')
         if(k == 1 & i == 3){
            ccc_master <- ccc_result
         }else{
            ccc_master <- rbind(ccc_master, ccc_result)
         }
         if(k%%100 == 0){
            print(ccc_result)
         }
      }
   }
   
   ccc_analysis <- ccc_master[,':='(nvars = as.numeric(nvars), nclusters = as.numeric(nclusters), ccc = as.numeric(ccc))]
   
   #Renan - Alterei o parametro da melhor configuracao para o kmeans, para retornar algum dados 
   #Renan - Desabilitar o clustering para usar somente a estacao coxim
   ccc_best <- ccc_analysis[nclusters == clusters & nvars < vars][, .(mean_ccc = mean(ccc, na.rm = T)), by = variables][order(-mean_ccc)]
   
   #### Nao precisamos desse grafico por enquanto
   # ccc_plot <- ggplot(ccc_analysis, aes(x = factor(nclusters), y = ccc, color = factor(nvars))) + 
   #      geom_boxplot() +
   #      # geom_point() + 
   #      # scale_color_fivethirtyeight() +
   #      season_facet + 
   #      theme(legend.position = 'right') + 
   #      labs(
   #           x = 'Number of clusters',
   #           y = 'Cubic clustering criterion',
   #           color = 'Number of variables'
   #      )
   # 
   # ggsave(ccc_plot, filename = paste0(wd_exports,'ccc_optimize_plot.png'), width = 7, height = 7)
   
   # Calculate k-means cluster based on all regressors at all sites
   # # Using raw band and band ratio values
   # Select colors for plotting
   #cl_colors <- brewer.pal(name = 'Paired',n=12)
   
   # Select variables to use for clustering
   # clustering_vars <- c('B1','B4','B2.B1','B3.B1', 'B4.B3.B1')
   # Renan - Selecionar variaveis
   clustering_vars <- unlist(strsplit(as.character(ccc_best[1,'variables']),'_')) # based on optimal cluster vars from ccc analysis
   
   return(clustering_vars)
}

runKMeansCluster <- function(site_band_quantiles_all, clustering_vars, n_centers){
   
   # Calculate k-means cluster based on all regressors at all sites
   # # Using raw band and band ratio values
   site_band_scaling <- scale(site_band_quantiles_all[,..clustering_vars])
   clusters_calculated <- kmeans(site_band_scaling, centers = n_centers, nstart = 1000, iter.max = 10000)
   
   clusters_calculated_list[[i]] <- clusters_calculated
   # , algorithm = 'MacQueen'
   
   # Compute cluster centers
   cluster_centers <- clusters_calculated$centers
   
   # Assign cluster to each site
   site_band_quantiles_all$cluster <- clusters_calculated$cluster
   clustered_sites <- site_band_quantiles_all[,.(landsat_dt,cluster)]
   
   write_csv(site_band_quantiles_all,paste0(wd_exports,'site_band_quantiles_n',i,'.csv'))
   
   return(clustered_sites)
}
