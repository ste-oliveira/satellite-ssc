runClusterAnalysis <- function(site_band_scaling_all, clusters, vars){
   
   #Renan - Diminui a qtd de variaveis utilizadas
   # vis_nir_bands <- c('B1','B2','B3','B4','B2.B1','B3.B1','B4.B1','B3.B2','B4.B2','B4.B3','B4.B3.B1')
   vis_nir_bands <- c('B1','B2','B3')
   site_band_scaling_all <- scale(site_band_quantiles_all[,..vis_nir_bands])
   cluster_var_combinations <- Map(as.data.frame, sapply(seq_along(vis_nir_bands), function(k) t(combn(vis_nir_bands,k))))
   
   #Renan - Diminui o loop de 4 para 3 para funcionar com a quantidade de variaveis
   for(i in 2:length(vis_nir_bands)){
      cluster_var_k_sel <- cluster_var_combinations[[i]]
      for(k in 1:nrow(cluster_var_k_sel)){
         print(paste0(i, " ", k))
         cluster_var_sel <- c(as.matrix(cluster_var_k_sel[k,]))
         
         cluster_var_label <- paste(cluster_var_sel, collapse = "_")
         #Renan - Alterei os parÃ¢metros do nbclust para rodar com a qdt de registros disponiveis min.nc e max.nc
         ccc_result <- data.table(cbind(cluster_var_label, i, c(2:4), NbClust(site_band_scaling_all[,cluster_var_sel],
                                                                              min.nc=2, max.nc=4, index="ccc", method="kmeans")$All.index))
         print(ccc_result)
         colnames(ccc_result) <- c('variables','nvars','nclusters','ccc')
         if(k == 1 & i == 2){
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
   return(ccc_analysis)
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
   clustered_sites <- site_band_quantiles_all[,.(site_no,cluster)]
   
   write_csv(site_band_quantiles_all,paste0(wd_exports,'site_band_quantiles_n',i,'.csv'))
   
   return(clustered_sites)
}


# GENERATE CLUSTERING FUNCTION BASED ON OPTIMAL CLUSTER BREAKDOWN #
getCluster <- function(df,clustering_vars,n_centers, kmeans_object){
   # Compute band median at each site for clustering variables
   site_band_quantiles_all <- df[
      # n_insitu_samples_bySite][N_insitu_samples > 12
      ,.(N_samples = .N,
         B1 = median(B1),
         B2 = median(B2),
         B3 = median(B3),
         B4 = median(B4),
         # B5 = median(B5),
         # B7 = median(B7),
         B2.B1 = median(B2.B1),
         B3.B1 = median(B3.B1),
         B4.B1 = median(B4.B1),
         B3.B2 = median(B3.B2),
         B4.B2 = median(B4.B2),
         B4.B3 = median(B4.B3),
         B4.B3.B1 = median(B4.B3/B1)), 
      by = .(station_nm,site_no)]
   
   site_band_quantile_scaled <- scale(site_band_quantiles_all[,..clustering_vars], 
                                      center = attributes(site_band_scaling)$`scaled:center`, 
                                      scale = attributes(site_band_scaling)$`scaled:scale`)
   
   closest.cluster <- function(x) {
      cluster.dist <- apply(kmeans_object$centers, 1, function(y) sqrt(sum((x-y)^2)))
      return(which.min(cluster.dist)[1])
   }
   
   site_band_quantiles_all$cluster <- apply(site_band_quantile_scaled, 1, closest.cluster)
   
   df_cluster <- merge(df,site_band_quantiles_all[,c('site_no','cluster')], by = 'site_no')
   df_cluster$cluster_sel <- df_cluster$cluster
   
   return(df_cluster)
   
}
