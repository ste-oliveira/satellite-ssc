#### CROSS VALIDATION MODEL####

## Passo:  validacao cruzada ##

#### CROSS VALIDATION MODEL ####

## Define training control
set.seed(1)

train.control <- trainControl(method = "cv", number = 10, repeats = 100)

## Train the model
model_lm <- train(Vendas ~ Publicidade, data = dados, 
                  method = "lm", trControl = train.control)

extract_eq(model_lm$finalModel, use_coefs = TRUE)

dados$pred <- predict(model_lm, dados)
dados$diff <- dados[, .(diff=abs((pred-Vendas)))]

## Summarize the results
print(model_lm)
print(model_lm$finalModel)
summary(model_lm$finalModel)

r_squared <- paste0('R2= ', round(model_lm$results$Rsquared, 2))
rmse <- paste0('RMSE= ', round(model_lm$results$RMSE, 2))


#### PLOT ERROR LM OUTLIERS ####
plotErrorLMOutliers <- function(ls_sr_insitu_data, r_squared, rmse){
  errro_lm_outliers_plot <-ggplot(data = ls_sr_insitu_data)+
    geom_abline(intercept = 0, slope = 1, color = '#2166ac', size=1)+
    geom_point(aes(x=pred, y = Vendas, fill=diff<0.79,  colour=diff<0.8), pch = 21, size = 5)+
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
    geom_point(aes(x=pred, y = Vendas), fill="#abdda4", color="#1b7837", alpha=0.7, pch = 21, size = 5)+
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



plotErrorLMOutliers(dados, r_squared, rmse)
plotErrorLM(dados, r_squared, rmse)

