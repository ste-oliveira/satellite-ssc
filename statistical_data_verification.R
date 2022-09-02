###### verificacao de dados estatisticos ######

## Relacao Linear entre VD e a VI:
### VD : Vendas
### VI: Publicidade

plot(ls_sr_insitu_data$index_montaigner_2014, ls_sr_insitu_data$ConcentracaoMatSuspensao)

## Construcao do modelo
## antes do til variavel dependente e depois do til variavel independente

model <- lm(ConcentracaoMatSuspensao ~ index_montaigner_2014, ls_sr_insitu_data)


## Analise grafica
par(mfrow=c(2,2))
plot(model)

## Interpretacao: https://data.library.virginia.edu/diagnostic-plots/
par(mfrow=c(1,1))

## Normalidade dos residuos:
shapiro.test(model$residuals)

## Outliers nos residuos:
summary(rstandard((model)))

## Independencia dos residuos (Durbin-Watson)
durbinWatsonTest(model)

## Homocedasticidade (Breusch-Pagan)
bptest(model)

## Analise do modelo
summary(model)

##### Grafico de dispersao ####

ggplot(data = ls_sr_insitu_data, mapping = aes(x = index_montaigner_2014, y = ConcentracaoMatSuspensao)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "red") + 
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label..,
                                          sep = "*plain(\",\")~~")),
                        label.x = 0, label.y = 400) +
  theme_classic()

