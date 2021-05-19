
#### FUNCTIONS ####
# Generates random holdout set for each cluster
getHoldout <- function(datatable){
     setDT(datatable)
     rows <- c()
     for(cl_sel in unique(datatable$cluster_sel)){
          cluster_sub <- which(datatable[,cluster_sel] == cl_sel)
          rows <- c(rows,sample(cluster_sub,round(length(cluster_sub)*0.25)))
          # print(length(rows))
     }
     
     datatable[,holdout25:='in'][rows,holdout25:='holdout']
     return(datatable)
     
} 

getModels_lasso <- function(data, regressors){
     # for testing
     # data <- ls_insitu_cl_sample
     lm_data <- na.omit(setDT(data)[,
                                    ':='(pred_gl = NA,res_gl = NA,pred_cl = NA,
                                         res_cl = NA, pred_st = NA, res_st = NA,
                                         ssc_subset = cluster_sel
                                    )][!is.na(ssc_subset)], cols = c('log10_SSC_mgL',regressors))
     # clusters
     subsets <- unique(lm_data$ssc_subset)
     
     n_clusters <- length(subsets)
     cluster_funs <- list(rep(NA,length(subsets)))
     site_funs <- list(rep(NA,length(subsets)))
     
     
     for(i in subsets){ # for individual cluster models
          regressors_sel <- regressors[-which(regressors == 'site_no')]
          lm_data_lm <- lm_data[ssc_subset == i] # only chooses sites within cluster
          # lm_data_hold <- lm_data_lm[-which(lm_data_lm$site_no %in% holdout_sts),] # for eliminating certain sites
          lm_data_hold <- lm_data_lm[which(lm_data_lm$holdout25 == 'in'),]
          glm_y <- as.matrix(lm_data_hold[,log10_SSC_mgL])
          glm_x <- as.matrix(lm_data_hold[,..regressors_sel])
          
          ssc_lm <- cv.glmnet(x = glm_x, y = glm_y, family = 'gaussian', type.measure = "mse", nfolds = 10)
          cv.opt <- coef(ssc_lm, s = "lambda.1se")
          coef_ex <- cbind(rownames(cv.opt),as.numeric(cv.opt))
          colnames(coef_ex) <- c('variable', 'value')
          
          write.table(coef_ex, sep = ",", file = paste0(wd_exports,'cluster_ncl',n_clusters,'_', i,'_lasso_fit_coeff.csv'), row.names = F)
          glm_x <- NA
          glm_y <- NA
          
          cluster_funs[[i]] <- ssc_lm
          glm_pred <- predict(ssc_lm, newx = as.matrix(lm_data_lm[,..regressors_sel]), s = "lambda.1se")
          lm_data$pred_cl[which(lm_data$ssc_subset == i)] <- glm_pred
          
          # lm_data$res_cl[which(lm_data$ssc_subset == i)] <- resid(ssc_lm)
     }
     
     lm_data$ssc_subset <- lm_data$cluster_sel # sites
     lm_data$site_code <- lm_data$site_no
     subsets <- unique(lm_data$ssc_subset)
     for(i in subsets){ # for individual cluster models
          
          regressors_sel <- regressors
          lm_data_lm <- subset(lm_data, ssc_subset == i) # only chooses sites within cluster
          # lm_data_hold <- lm_data_lm[-which(lm_data_lm$site_no %in% holdout_sts),] # for eliminating certain sites
          # lm_data_hold <- lm_data_lm[which(lm_data_lm$holdout25 == 'in'),]
          lm_data_hold <- lm_data_lm # need to make sure all sites are included in lm_data_hold in order to use holdout
          
          glm_y <- as.matrix(lm_data_hold[,log10_SSC_mgL])
          glm_x <- model.matrix( ~ ., lm_data_hold[,..regressors])
          
          ssc_lm <- cv.glmnet(x = glm_x,y = glm_y, family = 'gaussian', type.measure = "mse", nfolds = 10)
          cv.opt <- coef(ssc_lm, s = "lambda.1se")
          coef_ex <- cbind(rownames(cv.opt),as.numeric(cv.opt))
          colnames(coef_ex) <- c('variable', 'value')
          
          write.table(coef_ex, sep = ",", file = paste0(wd_exports, 'cluster_ncl',n_clusters,'_', i,'site_lasso_fit_coeff.csv'), row.names = F)
          
          site_funs[[i]] <- ssc_lm
          glm_x <- NA
          lm_data_hold <- NA
          
          glm_x_new <- model.matrix( ~ ., lm_data_lm[,..regressors])
          glm_pred <- predict(ssc_lm, newx = glm_x_new, s = "lambda.1se")
          lm_data$pred_st[which(lm_data$ssc_subset == i)] <- glm_pred
          # lm_data$res_cl[which(lm_data$ssc_subset == i)] <- resid(ssc_lm)
     }
     lm_data$ssc_subset <- 1
     subsets <- unique(lm_data$ssc_subset) # global - only one value for subset
     for(i in subsets){ # for individual models
          regressors_sel <- regressors[-which(regressors == 'site_no')]
          lm_data_lm <- lm_data[ssc_subset == i] # only chooses sites within cluster
          # lm_data_hold <- lm_data_lm[-which(lm_data_lm$site_no %in% holdout_sts),] # for eliminating certain sites
          lm_data_hold <- lm_data[which(lm_data$holdout25 == 'in'),]
          glm_y <- as.matrix(lm_data_hold[,log10_SSC_mgL])
          glm_x <- as.matrix(lm_data_hold[,..regressors_sel])
          
          ssc_lm <- cv.glmnet(x = glm_x,y = glm_y, family = 'gaussian', type.measure = "mse", nfolds = 10)
          cv.opt <- coef(ssc_lm, s = "lambda.1se")
          coef_ex <- cbind(rownames(cv.opt),as.numeric(cv.opt))
          colnames(coef_ex) <- c('variable', 'value')
          
          write.table(coef_ex, sep = ",", file = paste0(wd_exports,'global_ncl',n_clusters,'_', i, i,'_lasso_fit_coeff.csv'), row.names = F)
          
          glm_pred <- predict(ssc_lm, newx = as.matrix(lm_data_lm[,..regressors_sel]), s = "lambda.1se")
          lm_data$pred_gl[which(lm_data$ssc_subset == i)] <- glm_pred
     }
     return(list(lm_data, cluster_funs, site_funs))
}

# Calculate model relative error and station bias (following Morley, 2018)
# Version that generates plots
getErrorBias <- function(dt, subset_name){
     # test
     # dt <- ls_sample_models[[1]]
     # subset_name <- 'ncl5_sample_500perstn'
     # Error computed following Morley et. al, 2018
     # Measures of Model Performance Based On the Log Accuracy Ratio
     
     rel_error <- dt[holdout25 == 'holdout',.(
          mape_gl_ind = (10^median(abs(log10(10^pred_gl/10^log10_SSC_mgL)), na.rm = T)-1),
          mape_cl_ind = (10^median(abs(log10(10^pred_cl/10^log10_SSC_mgL)), na.rm = T)-1),
          mape_st_ind = (10^median(abs(log10(10^pred_st/10^log10_SSC_mgL)), na.rm = T)-1)
     )]

     # For plotting: get percentage breaks for vertical dashed lines
     percent_error_dt <- data.table(error_breaks = c(0.1,0.5,1,2), 
                                    error_labels = c('< 10 %', '< 50 %', 
                                                     '< 100 %', '< 200 %'))
     
     # Plot histogram of individual errors
     error_histogram <- ggarrange(
          # ggplot(ls_sample_models[[1]][holdout25 == 'holdout'],
          ggplot(dt[holdout25 == 'holdout'],
                 aes(y = (10^abs(log10(10^pred_cl/10^log10_SSC_mgL)))-1)) + 
               geom_boxplot() + 
               scale_y_log10(limits = c(0.001, 200), labels = fancy_scientific) + 
               season_facet +
               theme(axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     panel.border = element_blank(),
                     axis.line = element_blank(),
                     axis.title = element_blank()
               ) +
               rotate(), 
          ggplot(dt[holdout25 == 'holdout'], 
                 aes(x = (10^abs(log10(10^pred_cl/10^log10_SSC_mgL)))-1)) +
               geom_histogram(binwidth = 0.25) + 
               geom_vline(xintercept = c(0.1,0.5,1,2), color = 'navy', lty = 'dashed') +
               geom_text(data = percent_error_dt, aes(x = error_breaks, y = nrow(dt)/20,
                                                      label = error_labels),
                         color = 'navy', angle = 90, vjust = -0.2, hjust = 0, size = 2.5) +
               scale_x_log10(limits = c(0.001, 200), labels = fancy_scientific) +
               scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
               season_facet +
               labs(x = 'Relative error',
                    y = 'Count'),
          nrow = 2,
          align = 'v', heights = c(0.25,2)) 
     
    
     ggsave(error_histogram, filename = paste0(wd_exports,'rel_error_hist_', subset_name, '.pdf'), width = 4, height = 4, useDingbats = F)
     
     # Summarize data at each station
     station_summary_subset <- dt[holdout25 == 'holdout'][,.(
          log10_SSC_mgL = mean(log10_SSC_mgL, na.rm = T),
          N_samples = .N,
          #p63 = mean(p63, na.rm = T),
          num_pix = mean(num_pix, na.rm = T),
          pred_gl = mean(pred_gl, na.rm = T),
          pred_cl = mean(pred_cl, na.rm = T),
          pred_st = mean(pred_st, na.rm = T)),
          keyby = .(site_no, cluster_sel, Latitude, Longitude)
                    # drainage_area_km2, begin_date, end_date)
     ]
     
     # print(station_summary_subset)
     # Compute relative bias at every station; calculate median of all stations for global, cluster, station models
     # Following Morley et al., 2018
     # Renan - Removi filtro para retornar dados
     stn_rel_bias <- station_summary_subset[,':='(
          mape_gl_stn = (10^abs(log10(10^pred_gl/10^log10_SSC_mgL))-1),
          mape_cl_stn = (10^abs(log10(10^pred_cl/10^log10_SSC_mgL))-1),
          mape_st_stn = (10^abs(log10(10^pred_st/10^log10_SSC_mgL))-1),
          mape_gl_sign = ifelse(pred_gl > log10_SSC_mgL, 1, -1),
          mape_cl_sign = ifelse(pred_cl > log10_SSC_mgL, 1, -1),
          mape_st_sign = ifelse(pred_st > log10_SSC_mgL, 1, -1))
     ][,':='(
          mape_gl_sign = mape_gl_sign*mape_gl_stn,
          mape_cl_sign = mape_cl_sign*mape_cl_stn,
          mape_st_sign = mape_st_sign*mape_st_stn
     )]
     
     # print(stn_rel_bias)
     # Compute relative bias at every station
     median_rel_bias <- stn_rel_bias[,.(
          bias_gl = median(mape_gl_stn, na.rm = T),
          bias_cl = median(mape_cl_stn, na.rm = T),
          bias_st = median(mape_st_stn, na.rm = T)
     )
     ] 
     median_rel_bias_melt <- melt(median_rel_bias, measure.vars = c('bias_gl','bias_cl','bias_st'))[,
                                                                                                    model := factor(variable, levels = c('bias_gl','bias_cl','bias_st'),
                                                                                                                    labels = c('Base','Cluster','Station'), ordered = T)]
     
     station_bias_melt <- melt(stn_rel_bias, id.vars = c('site_no', 'cluster_sel', 'Latitude', 'Longitude'),
                               measure.vars = c('mape_gl_sign','mape_cl_sign','mape_st_sign'))[,
                                                                                               model := factor(variable, levels = c('mape_gl_sign','mape_cl_sign','mape_st_sign'),
                                                                                                               labels = c('Base','Cluster','Station'), ordered = T)]
     # print(station_bias_melt)
     # station_bias_cluster_plot <- ggplot(station_bias %>% melt(measure.vars = c('bias_gl','bias_cl','bias_st'))) +
     station_bias_cluster_plot <- ggplot(station_bias_melt) +
          geom_boxplot(aes(x = as.factor(cluster_sel), y = value, 
                           fill = as.factor(cluster_sel)), outlier.shape = NA) + 
          geom_text(data = median_rel_bias_melt,
                    aes(x = as.factor(1), y = 2.65, label = paste0('Median bias = ',round(value, 2))),
                    hjust = 0,vjust = 0, size = 2.5) +
          scale_fill_brewer(palette = 'Paired') +
          scale_y_continuous(lim = c(-2.75,2.75)) +
          facet_wrap(.~as.factor(model)) +
          season_facet +
          labs(
               x = 'River grouping',
               y = 'Relative bias (at-a-station)'
          )
     # SAVE FIGURE
     ggsave(station_bias_cluster_plot, filename = paste0(wd_exports,'station_bias_', subset_name, '.pdf'), 
            useDingbats = F, width = 5, height = 4)
     
     return(list(cbind(rel_error,median_rel_bias), station_bias_cluster_plot))
}

# Display log axes labels nicely
fancy_scientific <- function(l) { 
     # turn in to character string in scientific notation 
     l <- log10(l)
     # return(parse(text=paste("'Discharge [m'", "^3* s", "^-1 ", "*']'", sep="")))
     return(parse(text = paste("10^",as.character(l),sep = "")))
} 

# Plot results of SSC calibration, with several adjustable parameters
get_sscPlot <- function(ssc_data,ssc_title,density_yn, validation){
        ## Test data
        # ssc_data <- ssc_model_cl_iterate_pred
        # ssc_title <- "byCluster"
        # density_yn <- 'no'
        # validation <- 'yes'
        ## Start function here
        if(ssc_title == "byGlobal"){
                # ssc_data <- ssc_data[,pred := pred_gl]
                cl_colors <- rep('#1f78b4',5)
                ssc_plot <- ggplot(data = ssc_data,   
                                   aes(x = ConcentracaoMatSuspensao, y = 10^(pred_gl)))
        } else if(ssc_title == "byCluster"){
                ssc_plot <- ggplot(data = ssc_data,   
                                   aes(x = ConcentracaoMatSuspensao, y = 10^(pred_cl)))
        } else {
                ssc_plot <- ggplot(data = ssc_data,   
                                   aes(x = ConcentracaoMatSuspensao, y = 10^(pred_st)))
        }
        n_samples <- nrow(ssc_data)
        if(density_yn == "yes"){
                ssc_plot <- ssc_plot + 
                        stat_density_2d(aes(fill = stat(nlevel)), 
                                        # h = c(1,1),
                                        geom = "polygon") +
                        # facet_wrap(. ~ as.factor(X5_clusters_weighted)) +
                        scale_fill_distiller(palette = 'GnBu', type = 'seq', direction = 1)
        } else if(density_yn == 'overlay'){
                ssc_plot <- ssc_plot +
                        geom_point(aes(
                                # fill = as.factor(cluster_sel)
                                # ,color = as.factor(cluster_sel)), pch = 16,
                        ),color = 'black', pch = 16,
                        # color = "black", pch = 21, alpha = 0.8, # comment to remove outline
                        size = 1, alpha = min(1,1/(sqrt(n_samples*0.001)))) + 
                        stat_density_2d(aes(fill = stat(nlevel)), 
                                        # h = c(1,1),
                                        geom = "polygon", alpha = 0.3) +
                        # facet_wrap(. ~ as.factor(X5_clusters_weighted)) +
                        scale_fill_distiller(palette = 'GnBu', type = 'seq', direction = 1)
                # scale_color_brewer('Paired', type = 'qual') + scale_fill_brewer('Paired', type = 'qual')
                # scale_color_manual(values = cl_colors) + scale_fill_manual(values = cl_colors)
        }else{
                ssc_plot <- ssc_plot +
                        geom_point(aes(
                                # fill = as.factor(cluster_sel)
                                # ,color = as.factor(cluster_sel)), pch = 16,
                        ),color = 'black', pch = 16,
                        # color = "black", pch = 21, alpha = 0.8, # comment to remove outline
                        size = 1, alpha = min(1,1/(sqrt(n_samples*0.001))))
        }
        if(validation == 'yes'){
                ssc_plot <- ssc_plot + facet_wrap(.~factor(holdout25, levels = c('in','holdout'), ordered = T))
        }
        
        ssc_plot <- ssc_plot + 
                theme_bw() +
                geom_abline(intercept = 0, slope = 1, color = 'orange') +
                scale_y_log10(limits = c(1,50000), labels = fancy_scientific, breaks = breaks, minor_breaks = minor_breaks,
                              expand = expansion(add = c(0.7,0.3))) + 
                scale_x_log10(limits = c(1,50000), labels = fancy_scientific, breaks = breaks, minor_breaks = minor_breaks, 
                              expand = expansion(add = c(0.7,0.3))) +
                theme(
                        legend.title = element_blank(),
                        legend.position = 'none',
                        text = element_text(size=15),
                        panel.grid.minor = element_blank(),
                        axis.text = element_text(size = 12),
                        strip.text = element_blank()
                ) + 
                labs(
                        x = expression(paste(italic("in situ"), " SSC (mg/L)")),
                        y = "Satellite-estimated SSC (mg/L)"
                        # fill = "Cluster", 
                        # color = "Cluster"
                )
        return(ssc_plot)
}

#### THEMES AND PLOTTING PARAMETERS ####

breaks <- 10^(-10:10)
minor_breaks <- rep(5, 21)*(10^rep(-10:10, each=9))

theme_evan <- theme_bw() +
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

theme_evan_facet <- theme_bw() +
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

