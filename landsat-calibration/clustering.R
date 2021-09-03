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
   
   return(ccc_best)
}
