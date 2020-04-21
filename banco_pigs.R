install.packages("fma")
install.packages("expsmooth")


library(ggplot2)
library(ggfortify)
library(forecast)
library(urca)
library(astsa)
library(fma)
library(expsmooth)

pigs
# O banco já vem em formato de séries tempoais com inicio em 
# Janeiro de 1980 e final em agosto de 1995 pigs referente ao
# Numero total mensal de porcos abatidos em Victoria na Austrália.

autoplot(pigs)
ggseasonplot(pigs)

# Não muito claro, porém observando bem podemos ver que a série
# pigs há uma sazonalidade anual, que a série  não é estacionária,
# pois o eixo do y passeia bem entre 60000 abatimentos a mais
# de 100000.

summary(pigs)
# O mínimo 33873 para o primeiro quartil 79080 a diferença é
# gritante, mais que o dobro devido ao mínimo ser um ponto bem 
# diferente do comportamento normal da série e o primeiro 
# quartil já ser um valor em que faz parte da massa de dados 
# da série tendo uma diferença de 45207, isso também é possível
# ver porque a diferença do primeiro quartil para o segundo é 
# de apenas 12582 e essa diferença se mantem não muito distante
# para o terceiro quartil e do terceiro quartil até o máximo. Portando
# o ponto de mínimo seria um bom ponto para pesquisa o que aconteceu
# no pico decrescente em março de 1980.

ndiffs(pigs)
# Ao aplicar a função ndiffs no banco, resultará quantas vezes será necessário 
# diferenciar a série para torna la estacionária, ou seja, retirar a tendência 
# presente nela, que normalmente é 0, 1 ou 2, para o banco de numero total de
# porcos abatidos resultou em apenas uma diferenciação já resolve o problema de tendência.
nsdiffs(pigs)
# Ao aplicar a função nsdiffs no banco, resultará quantas vezes será necessário 
# diferenciar a série na componente sazonal para torna la estacionária, ou seja,
# retirar a tendência presente da componente sazonal, que normalmente é 0, 1 ou
# 2 também, para o banco de numero total de porcos abatidos resultou
# em nenhuma diferenciação na componente sazonal.

# Estimando a ordem do modelo pelos critérios de seleção de modelo
arima_aic_c <- auto.arima(pigs, ic = "aic") # e auto.arima(pigs, ic = "aicc")
# ARIMA(1,1,1)(2,0,1)

arima_bic <- auto.arima(pigs, ic = "bic")
# ARIMA(2,1,0)(1,0,0)

# Compare os dois modelos d_arima1 e d_arima2 via seleção de modelos 
# no caso eu iria observar o s residuos, aic, bic, aicc dos dois modelos
# e ver numa tabelinha qual modelo obteve menores resiudos.
# tipo.

arima_aic_c$aic
arima_bic$aic

arima_aic_c$aicc
arima_bic$aicc

arima_aic_c$bic
arima_bic$bic

# O modelo que teve o menor valor nos critérios de aic, aicc, bic foi
# arima_bic ARIMA(2,1,0)x(1,0,0)

pigs

library(forecast)
d_sarima1 <- sarima(pigs, 2,1,0, 2,0,0,12)
d_sarima2 <- sarima(pigs, 2,1,0, 1,0,0,12)
fit.arima1 <- arima(pigs, order = c(2, 1, 0), seasonal = list(order = c(2, 0, 0)))
fit.arima2 <- arima(pigs, order = c(2, 1, 0), seasonal = list(order = c(1, 0, 0)))

# Essa análise de resíudos serve para os dois modelos.
# Talvez tenham dois pontos logo no início que estejam fora do intervalo  -3 e 3.
# residuos esperado que esteja entre 3 e -3, 
# que todos os lags esteja dentro do intervalo de confiança dizendo que os resiudos são
# sao nao correlacionados

# os residuos seguem normalidade, no gráfico do QQ Plot dos residuos os pontos
# estão dispostos como uma retas mesmo.

# e no terceiro graifco que da o resultado do teste de Ljung Box, que todos os pontos
# estejam acima da linha pontilhada, ou seja rejeita-se a hipotese de que os residuos 
# não estejam bem ajustados.

sarima1
d_sarima1
# os residuos seguem normalidade
# e no terceiro graifco que da o resultado do teste de Ljung Box, que todos os pontos
# estejam acima da linha pontilhada, ou seja rejeita-se a hipotese de que os residuos 
# não estejam bem ajustados

d_sarima1
# d_sarima1 ao nível de significancia de 5%, temos que todos os parametros são
# são siginificativos (atenção para D (Fi grande)m (autoregressivo na parte sazonal))

d_sarima2
# d_sarima2 um modelo mais compacto independente do nível nominal, temos que todos
# os parametros são são siginificativos com p-valores bem baixos, todos p < 0e4.



######################################################
# METODOS DE PREVISAO
######################################################

d_forecasts1 <- forecast(arima_aic_c, level = 95, h = 12)
autoplot(d_forecasts1)
d_forecasts2 <- forecast(arima_bic, level = 95, h = 12)
autoplot(d_forecasts2)

# Métodos de previsão ingenuos.

# Apenas observando:
# método de média para previsão.
meanf(pigs, h = 12)
# naive
naive(pigs, h = 12)
snaive(pigs, h = 12)
rwf(pigs, h = 12, drift = TRUE)

# Janeiro de 1980 e final em agosto de 1995 pigs referente ao
# Numero total mensal de porcos abatidos em Victoria na Austrália.

# Plotando as previsões sem retirar as observações

pigs_I <- window(pigs, start = 1980, end = c(1995, 12))
autoplot(pigs) +
  autolayer(meanf(pigs, h = 8), series = "Média", PI = FALSE) +
  autolayer(naive(pigs, h = 8), series = "Naive", PI = FALSE) +
  autolayer(snaive(pigs, h = 8), series = "Naive Sazonal", PI = FALSE) +
  autolayer(rwf(pigs, h = 8, drift = TRUE), series = "Naive com drift", PI = FALSE) +
  ggtitle("N Total de passageiros de companhias aéres interenacionais") + 
  guides(colour = guide_legend("Forecast"))

# Observando o grafico, a previsão pela média, e o naive, são constntes, demonstrando
# claramente uma previsão bem distante do comportamento natural da série, e mesmo 
# o método naive com drift não sendo uma constante podendo notar um certo crescimento
# ainda sim teve um comportamento bem distante da série, o melhor método ingênuo foi o 
# naive sazonal, que conseguiu captar o comportamento da série com as caracteristicas
# fortes sazonais. 

# Plotando as previsões retirando as ultimas 12 observações
pigs_I12 <- window(pigs, start = 1980, end = c(1994, 8))

pigsfit1 <- meanf(pigs_I12, h = 12)
# naive
pigsfit2 <- naive(pigs_I12, h = 12)
pigsfit3 <- snaive(pigs_I12, h = 12)
pigsfit4 <- rwf(pigs_I12, h = 12, drift = TRUE)


autoplot(window(pigs, start = 1980)) +
  autolayer(pigsfit1, series = "Média", PI = FALSE) +
  autolayer(pigsfit2, series = "Naive", PI = FALSE) +
  autolayer(pigsfit3, series = "Naive Sazonal", PI = FALSE) +
  autolayer(pigsfit4, series = "Naive com drift", PI = FALSE) +
  ggtitle("N Total de passageiros de companhias aéres interenacionais") + 
  guides(colour = guide_legend("Forecast"))

# A mesma interpretação anterior.
# mas não capitou a tendência crescente.

# Observando os erros/ residuos

#Pegando apenas o ultimo ano
pigs3 <- window(pigs, start = c(1994,9))

accuracy(pigsfit1$mean, pigs3)[1, c(3,2,5)]
accuracy(pigsfit2$mean, pigs3)[1, c(3,2,5)]
accuracy(pigsfit3$mean, pigs3)[1, c(3,2,5)]
accuracy(pigsfit4$mean, pigs3)[1, c(3,2,5)]

# O terceiro modelo teve os menores valores nas três medidas
# de erro, como esperado que foi visto no gráfico que o método
# do naive sazonal tinha tido a melhor desempenho, o ajuste com 
# pior desempenho foi o da média. 

##############################
# Alisamento exponencial
#############################

air <- window(pigs, start = 1949)
fc <- holt(air, h = 5)
fc$model

fc <- holt(air, h = 12)
autoplot(air) +
  autolayer(fc, series = "AE, Holt", PI = FALSE)
guides(colour = guide_legend(title = "Forecast"))

# Péssimo desempenho.

# Alisamento exponencial, aditivo e multiplicativo
air
fit1 <- hw(pigs_I12, seasonal = "additive", h = 12)
fit2 <- hw(pigs_I12, seasonal = "multiplicative", h = 12)

autoplot(air) + 
  autolayer(fit1, series = "HW - Aditivo", PI = FALSE) + 
  autolayer(fit2, series = "HW - Multiplicativo", PI = FALSE) + 
  ggtitle("N Total de passageiros de companhias aéres interenacionais") + 
  guides(colour = guide_legend("Forecast"))

# Aparentemente a previsão a 12 passos usando o algoritmo de Holt Winter
# foi o melhor, mas vamos fazer uma comparação mais assidua utilizando as 
# mesmas medidas de erros rmse, mae e mape, para averiguar qual
# método ou algoritmo de previsão foi melhor.

accuracy(pigsfit1, pigs3)[1, c(3,2,5)]
accuracy(pigsfit2, pigs3)[1, c(3,2,5)]
accuracy(pigsfit3, pigs3)[1, c(3,2,5)]
accuracy(pigsfit4, pigs3)[1, c(3,2,5)]

accuracy(fit1, pigs3)[1, c(3,2,5)]
accuracy(fit2, pigs3)[1, c(3,2,5)]

# Bom, já podemos observar que os dois algoritmos de previsão
# alisamento exponencial de Holt Winter tanto o aditivo quanto o 
# multiplicativo tiveram melhos desempenho que todos os métodos naive
# mas o multiplicativo ainda se destacou mais sendo o melhor metodo de 
# previsão a 12 passos para o numero de passageiros internacionais da linha
# aerea tendo MAE = 7.533284,     RMSE   9.949946,    MAPE 2.997750,






##########################################################################
# Plotando as previsões retirando as ultimas 6 observações
##########################################################################

pigs_I12 <- window(pigs, start = 1949, end = c(1960, 6))

pigsfit1 <- meanf(pigs_I12, h = 6)
# naive
pigsfit2 <- naive(pigs_I12, h = 6)
pigsfit3 <- snaive(pigs_I12, h = 6)
pigsfit4 <- rwf(pigs_I12, h = 6, drift = TRUE)


autoplot(window(pigs, start = 1949)) +
  autolayer(pigsfit1, series = "Média", PI = FALSE) +
  autolayer(pigsfit2, series = "Naive", PI = FALSE) +
  autolayer(pigsfit3, series = "Naive Sazonal", PI = FALSE) +
  autolayer(pigsfit4, series = "Naive com drift", PI = FALSE) +
  ggtitle("N Total de passageiros de companhias aéres interenacionais") + 
  guides(colour = guide_legend("Forecast"))

# A mesma interpretação anterior.
# mas não capitou a tendência crescente.

# Observando os erros/ residuos

#Pegando apenas o ultimo ano
pigs3 <- window(pigs, start = 1960)

accuracy(pigsfit1, pigs3)[1, c(3,2,5)]
accuracy(pigsfit2, pigs3)[1, c(3,2,5)]
accuracy(pigsfit3, pigs3)[1, c(3,2,5)]
accuracy(pigsfit4, pigs3)[1, c(3,2,5)]

# O mesmo desempenho que para h = 12.
# O terceiro modelo teve os menores valores nas três medidas
# de erro, como esperado que foi visto no gráfico que o método
# do naive sazonal tinha tido a melhor desempenho, o ajuste com 
# pior desempenho foi o da média. 

##############################
# Alisamento exponencial
#############################

air <- window(pigs, start = 1949)

fc <- holt(air, h = 6)
autoplot(air) +
  autolayer(fc, series = "AE, Holt", PI = FALSE) + 
  guides(colour = guide_legend(title = "Forecast"))

# Péssimo desempenho.

# Alisamento exponencial, aditivo e multiplicativo
air
fit1 <- hw(pigs_I12, seasonal = "additive", h = 6)
fit2 <- hw(pigs_I12, seasonal = "multiplicative", h = 6)

autoplot(air) + 
  autolayer(fit1, series = "HW - Aditivo", PI = FALSE) + 
  autolayer(fit2, series = "HW - Multiplicativo", PI = FALSE) + 
  ggtitle("N Total de passageiros de companhias aéres interenacionais") + 
  guides(colour = guide_legend("Forecast"))

# Aparentemente a previsão a 6 passos usando o algoritmo de Holt Winter
# foi muito bom, muito bom mesmo, mas vamos fazer uma comparação mais assidua utilizando as 
# mesmas medidas de erros rmse, mae e mape, para averiguar qual
# método ou algoritmo de previsão foi melhor.

accuracy(pigsfit1, pigs3)[1, c(3,2,5)]
accuracy(pigsfit2, pigs3)[1, c(3,2,5)]
accuracy(pigsfit3, pigs3)[1, c(3,2,5)]
accuracy(pigsfit4, pigs3)[1, c(3,2,5)]

accuracy(fit1, pigs3)[1, c(3,2,5)]
accuracy(fit2, pigs3)[1, c(3,2,5)]

# Bom, já podemos observar que os dois algoritmos de previsão
# alisamento exponencial de Holt Winter tanto o aditivo quanto o 
# multiplicativo tiveram melhos desempenho que todos os métodos naive
# mas o multiplicativo ainda se destacou mais sendo o melhor metodo de 
# previsão a 12 passos para o numero de passageiros internacionais da linha
# aerea tendo MAE = 7.779553,     RMSE = 10.651927,    MAPE 2.972715,


##########################################################################
# Plotando as previsões retirando as ultimas 3 observações
##########################################################################

pigs_I12 <- window(pigs, start = 1949, end = c(1960, 3))

pigsfit1 <- meanf(pigs_I12, h = 3)
# naive
pigsfit2 <- naive(pigs_I12, h = 3)
pigsfit3 <- snaive(pigs_I12, h = 3)
pigsfit4 <- rwf(pigs_I12, h = 3, drift = TRUE)


autoplot(window(pigs, start = 1949)) +
  autolayer(pigsfit1, series = "Média", PI = FALSE) +
  autolayer(pigsfit2, series = "Naive", PI = FALSE) +
  autolayer(pigsfit3, series = "Naive Sazonal", PI = FALSE) +
  autolayer(pigsfit4, series = "Naive com drift", PI = FALSE) +
  ggtitle("N Total de passageiros de companhias aéres interenacionais") + 
  guides(colour = guide_legend("Forecast"))

# A mesma interpretação anterior.
# mas não capitou a tendência crescente.

# Observando os erros/ residuos

#Pegando apenas o ultimo ano
pigs3 <- window(pigs, start = 1960)

accuracy(pigsfit1, pigs3)[1, c(3,2,5)]
accuracy(pigsfit2, pigs3)[1, c(3,2,5)]
accuracy(pigsfit3, pigs3)[1, c(3,2,5)]
accuracy(pigsfit4, pigs3)[1, c(3,2,5)]

# O mesmo desempenho que para h = 12.
# O terceiro modelo teve os menores valores nas três medidas
# de erro, como esperado que foi visto no gráfico que o método
# do naive sazonal tinha tido a melhor desempenho, o ajuste com 
# pior desempenho foi o da média. 

##############################
# Alisamento exponencial
#############################

air <- window(pigs, start = 1949)

fc <- holt(air, h = 6)
autoplot(air) +
  autolayer(fc, series = "AE, Holt", PI = FALSE) + 
  guides(colour = guide_legend(title = "Forecast"))

# Péssimo desempenho.

# Alisamento exponencial, aditivo e multiplicativo
air
fit1 <- hw(pigs_I12, seasonal = "additive", h = 6)
fit2 <- hw(pigs_I12, seasonal = "multiplicative", h = 6)

autoplot(air) + 
  autolayer(fit1, series = "HW - Aditivo", PI = FALSE) + 
  autolayer(fit2, series = "HW - Multiplicativo", PI = FALSE) + 
  ggtitle("N Total de passageiros de companhias aéres interenacionais") + 
  guides(colour = guide_legend("Forecast"))

# Aparentemente a previsão a 6 passos usando o algoritmo de Holt Winter
# foi muito bom, muito bom mesmo, mas vamos fazer uma comparação mais assidua utilizando as 
# mesmas medidas de erros rmse, mae e mape, para averiguar qual
# método ou algoritmo de previsão foi melhor.

accuracy(pigsfit1, pigs3)[1, c(3,2,5)]
accuracy(pigsfit2, pigs3)[1, c(3,2,5)]
accuracy(pigsfit3, pigs3)[1, c(3,2,5)]
accuracy(pigsfit4, pigs3)[1, c(3,2,5)]

accuracy(fit1, pigs3)[1, c(3,2,5)]
accuracy(fit2, pigs3)[1, c(3,2,5)]

# Bom, já podemos observar que os dois algoritmos de previsão
# alisamento exponencial de Holt Winter tanto o aditivo quanto o 
# multiplicativo tiveram melhos desempenho que todos os métodos naive
# mas o multiplicativo ainda se destacou mais sendo o melhor metodo de 
# previsão a 12 passos para o numero de passageiros internacionais da linha
# aerea tendo MAE = 7.779553, RMSE = 10.651927, MAPE 2.972715,

# ajustando o modelo com uma variavel dummy para a observação 
# colocando uma variável explicativa
x <- sarima(pigs, 2,1,0, 1,0,0,12, xreg = x)

ggseasonplot(pigs)



