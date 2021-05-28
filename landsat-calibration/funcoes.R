
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
          
          #Renan - Devemos escolher o k apropriado para nosso dataset. Por enquanto utilizaremos o método leave-one-out
          leave_one_out = nrow(lm_data_hold)
          ssc_lm <- cv.glmnet(x = glm_x, y = glm_y, family = 'gaussian', type.measure = "mse", nfolds = leave_one_out)
          cv.opt <- coef(ssc_lm, s = "lambda.min")
          
          coef_ex <- cbind(rownames(cv.opt),as.numeric(cv.opt))
          colnames(coef_ex) <- c('variable', 'value')
          
          write.table(coef_ex, sep = ",", file = paste0(wd_exports,'cluster_ncl',n_clusters,'_', i,'_lasso_fit_coeff.csv'), row.names = F)
          glm_x <- NA
          glm_y <- NA
          
          cluster_funs[[i]] <- ssc_lm
          glm_pred <- predict(ssc_lm, newx = as.matrix(lm_data_lm[,..regressors_sel]), type = "response", s = "lambda.min")
          lm_data$pred_cl[which(lm_data$ssc_subset == i)] <- glm_pred
          
          plot(ssc_lm)
          # lm_data$res_cl[which(lm_data$ssc_subset == i)] <- resid(ssc_lm)
     }
     
     # Renan - Manter apenas a primeira abordagem para o congresso
     # lm_data$ssc_subset <- lm_data$cluster_sel # sites
     # lm_data$site_code <- lm_data$site_no
     # subsets <- unique(lm_data$ssc_subset)
     # for(i in subsets){ # for individual cluster models
     #      
     #      regressors_sel <- regressors
     #      lm_data_lm <- subset(lm_data, ssc_subset == i) # only chooses sites within cluster
     #      # lm_data_hold <- lm_data_lm[-which(lm_data_lm$site_no %in% holdout_sts),] # for eliminating certain sites
     #      # lm_data_hold <- lm_data_lm[which(lm_data_lm$holdout25 == 'in'),]
     #      lm_data_hold <- lm_data_lm # need to make sure all sites are included in lm_data_hold in order to use holdout
     #      
     #      glm_y <- as.matrix(lm_data_hold[,log10_SSC_mgL])
     #      glm_x <- model.matrix( ~ ., lm_data_hold[,..regressors])
     #      
     #      ssc_lm <- cv.glmnet(x = glm_x,y = glm_y, family = 'gaussian', type.measure = "mse", nfolds = 10)
     #      cv.opt <- coef(ssc_lm, s = "lambda.1se")
     #      coef_ex <- cbind(rownames(cv.opt),as.numeric(cv.opt))
     #      colnames(coef_ex) <- c('variable', 'value')
     #      
     #      write.table(coef_ex, sep = ",", file = paste0(wd_exports, 'cluster_ncl',n_clusters,'_', i,'site_lasso_fit_coeff.csv'), row.names = F)
     #      
     #      site_funs[[i]] <- ssc_lm
     #      glm_x <- NA
     #      lm_data_hold <- NA
     #      
     #      glm_x_new <- model.matrix( ~ ., lm_data_lm[,..regressors])
     #      glm_pred <- predict(ssc_lm, newx = glm_x_new, s = "lambda.1se")
     #      lm_data$pred_st[which(lm_data$ssc_subset == i)] <- glm_pred
     #      # lm_data$res_cl[which(lm_data$ssc_subset == i)] <- resid(ssc_lm)
     # }
     # lm_data$ssc_subset <- 1
     # subsets <- unique(lm_data$ssc_subset) # global - only one value for subset
     # for(i in subsets){ # for individual models
     #      regressors_sel <- regressors[-which(regressors == 'site_no')]
     #      lm_data_lm <- lm_data[ssc_subset == i] # only chooses sites within cluster
     #      # lm_data_hold <- lm_data_lm[-which(lm_data_lm$site_no %in% holdout_sts),] # for eliminating certain sites
     #      lm_data_hold <- lm_data[which(lm_data$holdout25 == 'in'),]
     #      glm_y <- as.matrix(lm_data_hold[,log10_SSC_mgL])
     #      glm_x <- as.matrix(lm_data_hold[,..regressors_sel])
     #      
     #      ssc_lm <- cv.glmnet(x = glm_x,y = glm_y, family = 'gaussian', type.measure = "mse", nfolds = 10)
     #      cv.opt <- coef(ssc_lm, s = "lambda.1se")
     #      coef_ex <- cbind(rownames(cv.opt),as.numeric(cv.opt))
     #      colnames(coef_ex) <- c('variable', 'value')
     #      
     #      write.table(coef_ex, sep = ",", file = paste0(wd_exports,'global_ncl',n_clusters,'_', i, i,'_lasso_fit_coeff.csv'), row.names = F)
     #      
     #      glm_pred <- predict(ssc_lm, newx = as.matrix(lm_data_lm[,..regressors_sel]), s = "lambda.1se")
     #      lm_data$pred_gl[which(lm_data$ssc_subset == i)] <- glm_pred
     # }
     return(list(lm_data, cluster_funs, site_funs, ssc_lm))
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
              theme_clean()+ 
              geom_boxplot(size=0.3, colour="#ff7f00", fill="#fdbf6f") + 
              scale_y_continuous(limits = c(0, 2)) +
              theme(axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    panel.border = element_blank(),
                    axis.line.x = element_blank(),
                    axis.line.y = element_blank(),
                    axis.title = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  plot.background = element_blank()
              ) +
               rotate(), 
          ggplot(dt[holdout25 == 'holdout'], 
                 aes(x = (10^abs(log10(10^pred_cl/10^log10_SSC_mgL)))-1)) +
              theme_clean()+
              theme(panel.grid.major.y = element_blank(),
                    panel.grid.major.x = element_blank(),
                    plot.background = element_blank())+  
               geom_histogram(binwidth = 0.1, size=0.2, colour="#e31a1c", fill="#fb9a99") + 
               geom_vline(xintercept = c(0.1,0.5,1,2), color = '#6a3d9a', lty = 'dashed') +
               geom_text(data = percent_error_dt, aes(x = error_breaks, y = nrow(dt)/20, label = error_labels),
                         color = '#6a3d9a', angle = 90, vjust = -0.2, hjust = 0, size = 2.5) +
               scale_y_continuous(expand = expansion(mult = c(0, 0.2)), breaks = seq(0, 10, by = 1)) +
               scale_x_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 0.1)) +
               labs(x = 'Erro Relativo',
                    y = 'Quantidade de Erros'),
                    nrow = 2, align = 'v', heights = c(0.25,2))
     
     ggsave(error_histogram, filename = paste0(wd_exports,'rel_error_hist_', subset_name, '.png'), width = 7, height = 4)
     
     # Summarize data at each station
     station_summary_subset <- dt[holdout25 == 'holdout'][,.(
          log10_SSC_mgL = log10_SSC_mgL, na.rm = T,
          cluster_sel = cluster_sel,
          #N_samples = .N,
          #p63 = mean(p63, na.rm = T),
          num_pix = num_pix,
          pred_gl = pred_gl,
          pred_cl = pred_cl,
          pred_st = pred_st)
          #keyby = .(site_no, cluster_sel, Latitude, Longitude)
                    # drainage_area_km2, begin_date, end_date)
     ]
     
     medianSSC <- median(10^station_summary_subset$log10_SSC_mgL)
     medianpre <- median(10^station_summary_subset$pred_cl)
     
     # print(station_summary_subset)
     # Compute relative bias at every station; calculate median of all stations for global, cluster, station models
     # Following Morley et al., 2018
     # Renan - Removi filtro para retornar dados
     stn_rel_bias <- station_summary_subset[,':='(
          mape_gl_stn = (10^abs(log10(10^pred_gl/10^log10_SSC_mgL))-1),
          mape_cl_stn =  (10^pred_cl - 10^log10_SSC_mgL)/10^log10_SSC_mgL,
          mape_st_stn = (10^abs(log10(10^pred_st/10^log10_SSC_mgL))-1),
          mape_gl_sign = ifelse(pred_gl > log10_SSC_mgL, 1, -1),
          mape_cl_sign = ifelse(pred_cl > log10_SSC_mgL, 1, -1),
          mape_st_sign = ifelse(pred_st > log10_SSC_mgL, 1, -1))
     ][,':='(
          mape_gl_sign = mape_gl_sign*mape_gl_stn,
          mape_cl_sign = mape_cl_stn,
          mape_st_sign = mape_st_sign*mape_st_stn
     )]
    
     sim <-stn_rel_bias$pred_cl
     obs <- stn_rel_bias$log10_SSC_mgL
     
     # view((medianpre - medianSSC)/medianSSC)
     # view(bias(sim, obs, type = 'standardized'))
     # view(median(bias(sim, obs, type = 'standardized')))
     # 
     # print(stn_rel_bias)
     # Compute relative bias at every station
     median_rel_bias <- stn_rel_bias[,.(
          bias_gl = median(mape_gl_stn, na.rm = T),
          bias_cl = median(mape_cl_stn, na.rm = T),
          bias_st = median(mape_st_stn, na.rm = T)
     )
     ] 
     # median_rel_bias_melt <- melt(median_rel_bias, measure.vars = c('bias_cl'))[,model := factor(variable, 
     #                            levels = c('bias_cl'), labels = c('Base'), ordered = T)]
     # 
     # station_bias_melt <- melt(stn_rel_bias, id.vars = c('cluster_sel'),
     #                           measure.vars = c('mape_cl_sign'))[, model := factor(variable, levels = c('mape_cl_sign'),
     #                            labels = c('Base'), ordered = T)]
     # print(station_bias_melt)
     # station_bias_cluster_plot <- ggplot(station_bias %>% melt(measure.vars = c('bias_gl','bias_cl','bias_st'))) +
     station_bias_cluster_plot <- ggplot(stn_rel_bias) +
          geom_boxplot(aes(x = as.factor(cluster_sel), y = mape_cl_sign), width=0.3, size=0.3, colour="#e31a1c", fill="#fb9a99") + 
          # geom_jitter(aes(x = as.factor(cluster_sel), y = mape_cl_sign, 
          #                 fill = as.factor(cluster_sel)), width = 0.2) +
          geom_text(data = median_rel_bias,
                    aes(x = as.factor(1), y = 1.5, label = paste0('Viés Mediano = ',round(bias_cl, 2))),
                    hjust = 1,vjust = 0, size = 3, color="#b00000") +
          scale_fill_brewer(palette = 'Paired') +
          scale_y_continuous(lim = c(-1.5,1.5), breaks = seq(-1.5,1.5, by=0.2)) +
          theme_clean()+
          theme(panel.grid.major.y = element_blank(),
               panel.grid.major.x = element_blank(),
               plot.background = element_blank(),
               axis.text.x = element_blank(),
               axis.ticks.x = element_blank())+
          labs(
               x = 'River grouping',
               y = 'Viés Relativo'
          )
     # SAVE FIGURE
     ggsave(station_bias_cluster_plot, filename = paste0(wd_exports,'station_bias_', subset_name, '.png'),  width = 3, height = 4)
     
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
                                   aes(x = 10^(log10_SSC_mgL), y = 10^(pred_gl))) +
                geom_abline(intercept = 0, slope = 1, color = '#1f78b4')
                    
        } else if(ssc_title == "byCluster"){
                ssc_plot <- ggplot(data = ssc_data,   
                                   aes(x = 10^(log10_SSC_mgL), y = 10^(pred_cl))) +
                    geom_abline(intercept = 0, slope = 1, color = '#1f78b4')
        } else {
                ssc_plot <- ggplot(data = ssc_data,   
                                   aes(x = 10^(log10_SSC_mgL), y = 10^(pred_st))) +
                    geom_abline(intercept = 0, slope = 1, color = '#1f78b4')
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
                        ),color = '#e31a1c', fill="#fb9a99", pch = 21,
                        # color = "black", pch = 21, alpha = 0.8, # comment to remove outline
                        size = 4, alpha = min(1,1/(sqrt(n_samples*0.001))))
        }
        if(validation == 'yes'){
                ssc_plot <- ssc_plot + facet_wrap(.~factor(holdout25, levels = c('in','holdout'), ordered = T))
        }
        
        ssc_plot <- ssc_plot + 
                theme_clean() +
                scale_y_continuous(limits = c(0,700),
                              expand = expansion(add = c(0.7,0.3)),
                              breaks = seq(0, 700, by = 50)) + 
                scale_x_continuous(limits = c(0,700), 
                              expand = expansion(add = c(0.7,0.3)),
                              breaks = seq(0, 700, by = 50)) +
                theme(
                        legend.title = element_blank(),
                        legend.position = 'none',
                        axis.text = element_text(size=12),
                        axis.title = element_text(size=14),
                        panel.grid.minor = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.grid.major.x = element_blank(),
                        plot.background = element_blank()
                ) + 
                labs(
                        x = expression(paste("Concentração de Sedimentos ", italic("In Situ"), " (mg/L)")),
                        y = "Concentração de Sedimentos Estimada por Satélite (mg/L)"
                        # fill = "Cluster", 
                        # color = "Cluster"
                )
        return(ssc_plot)
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

