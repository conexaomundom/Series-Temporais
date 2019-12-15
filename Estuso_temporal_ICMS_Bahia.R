rm(list=ls())
install.packages("forecast")

rmse <- function(y,y_hat){ sqrt(mean((y - y_hat)^2)) }
rae <- function(y, y_hat){ sum(abs(y_hat - y)) / sum(abs(mean(y) - y)) }
rrse <- function(y, y_hat){ sum((y_hat - y)^2) / sum((mean(y) - y)^2) }
mae <- function(y, y_hat){ sum( abs(y - y_hat)) /length(y) }


###
# Pegando os dados, arrumando as variáveis
ficheiro = file.choose()
bd = read.table(file = ficheiro, header = FALSE, dec=".")
attach(bd)
icms_bahia <- bd
attach(icms_bahia)
igpdi <- icms_bahia$V1
icms_ba <- icms_bahia$V2
####

###
# Análise Descritiva Sem considerar a inflação
summary(icms_ba)
boxplot(icms_ba, main = "ICMS mensal, \n Janeiro de 1995 |-| Agosto de 2019, Bahia", ylab = "ICMS")
# Maximo oBSERVAÇÃO 288
max(icms_ba)
which(icms_ba == max(icms_ba))
# Mínimo OBSERVAÇÃO 3
min(icms_ba) 
which(icms_ba == min(icms_ba))
# A mediana nesses anos do ICMS da Bahia foi
median(icms_ba)

###
# Análise Descritiva Considerando a inflação
tam <- length(igpdi)
tam_icms <- length(icms_ba)

igpdi_d <- igpdi/igpdi[tam]
icms_d <- icms_ba / igpdi_d
summary(icms_d)
boxplot(cbind(icms_ba, icms_d), main = "ICMS mensal, \n Janeiro de 1995 |-| Agosto de 2019, Bahia", ylab = "ICMS")
###
# Podemos verificar que a serie deflacionada possui seus valores mais elevados
# alem de possuir um outlier no m?s ___ do ano ___, alem de possuir a media e a 
# mediana mais elevada assim diminuindo a amplitude da serie.
###
# Colocando o ICMS da Bahia deflacionado em formato de série temporal
library(tseries)
icmsba_d.ts = ts(icms_d, start = c(1995,1), frequency = 12)
# Plot da série e plot do decompose
plot(icmsba_d.ts)
plot(decompose(icmsba_d.ts))
# Vemos no grafico uma clara tendencia de ordem crescente, tendencia essa
# que se torna bastante visivel apartir dos anos 2000, n?o demostrando nenhuma
# grande mudan?a nesta tendencia desde ent?o, alem disso podemos ver uma sazionalidade
# no periodo anual e tambem nos meses dos anos, assim apresentando uma clara sazionalidade 
# em todo o deccorer da serie.

# ACF e PACF
acf(icmsba_d.ts)
pacf(icmsba_d.ts)
# Observa-se que todos os lag do ACF est?o fora do intervalo de confian?a assim indicando
# uma possibilidade elevada de n?o ser representado por um modelo AR, alem disso o PACF
# ap?s o terceiro lag demonstra uma queda exponencial, se aproximando do zero e se tornando
# oscilat?rio ap?s isso indicaria uma poss?vel representa??o de um modelo MA(2), mas pela exist?ncia
# de sazonalidade e da tend?ncia, est? serie possivelmete se encaixaria melhor em um modelo SARIMA,
# mas como n?o fomos apresentados para estes modelos n?o podemos analisar com maiores detalhes esta 
# possibilidade.
library(forecast)
auto.arima(icmsba_d.ts)
# Pela fun??o auto.arima temos indicios que o melhor modelo para representar esta serie
# seria o modelo MA(2) com uma diferencia??o, e que possui sazionalidade na serie assim sendo
# o modelo seria um SMA(2)

# Pegando a tendencia para 
icmsba_d_STL.ts <- stl(icmsba_d.ts, "periodic")
plot(icmsba_d.ts)
lines(icmsba_d_STL.ts$time.series[, "trend"], lwd = 1.4, col = "red")

# Verificamos em vermelho a tendencia da serie temporal, precisaremos destes valore para alguns possiveis
# ajusates para esta serie temporal, alem disso observa-se claramente a tendencia novamente, desta vez
# ainda mais facilmente por estar sobreposta a propria serie temporal.

###
# Teste de estacionaridade
library(tseries)
adf.test(icmsba_d.ts, alternative = "stationary")

# Ao nivel de significancia de 5%, ou seja, considerando a probabilidade do erro tipo I,
# probabilidade de rejeitar H0 dado que H0 ? verdadeiro, igual a 5%, temos que a hip?tese nula
# de n?o estacionaridade deve ser rejeitada, dado que p-valor, que representa a probabilidade de
# cometer este erro tipo I considerando os dados amostrais ? de aproximadamente 0,01.
# Assim temos que esta serie temporal possui estacionaridade.


###
####

# Previsão 
library(forecast)
a <- seq(285,296)
treino_icmsba_d.ts <- ts(icmsba_d.ts[-a], start = c(1995,1), frequency = 12)
teste_icmsba_d.ts <- ts(icmsba_d.ts[a], start = c(2018,8), frequency = 12)
t0 <- forecast(treino_icmsba_d.ts, h=12, level = c(95,99))
plot(forecast(t0, h=12, level = c(95,99)),shadecols = c( "#9ECAE1", "#6BAED6"), fcol = "darkblue", flwd = 1)
# Por conta da sazionalidade podemos prever por bastante tempo.  

rmse0 <- rmse(t0$mean,teste_icmsba_d.ts)
rae0 <- rae(t0$mean,teste_icmsba_d.ts)
mae0 <- mae(t0$mean,teste_icmsba_d.ts)
rrse0 <- rrse(t0$mean,teste_icmsba_d.ts)


####
# Primeira forma de retirar a tendência tomando uma diferença
####
dif_icmsba_d.ts <- diff(icmsba_d.ts)
plot(dif_icmsba_d.ts)
plot(decompose(dif_icmsba_d.ts))
# Vemos no grafico que a tendencia foi removida, assim apresentando aleatoriedade em seus dados
# observanfo a sazionalidade desta serie, temos que ela permanece proxima a da serie de dados original
# assim temos que a diferencia??o conseguiu remover a tendencia da serie

# ACF e PACF
acf(dif_icmsba_d.ts)
pacf(dif_icmsba_d.ts)
auto.arima(dif_icmsba_d.ts)
# Analisando o ACF e o PACF em conjundo gera-se uma duvida em qual modelo melhor se aplica para esta serie
# pois o PACF mantem sua queda exponencial apois o terceiro lag, mas o ACF apesar de n?o ser t?o claro demostra
# possiveis sinais da possibilidade da existencia da um AR na serie, assim gerando duvidas entre um modelo ARMA e um modelo MA
# lembrando que pela existencia da sazionalidade teriamos que incluir um componente para esta sazionalidade no modelo
# Como a fun??o auto.arima temos indicios de que o modelo que melhor se aplica para esta serie temporal seria um 
# MA(2) com o componente se sazionalidade, como foi aplicada uma diferencia??o na serie original que tinha como
# um possivel modelo de melhor aplica??o um MA(2) com o componente sazional e uma diferencia??o ent?o este resultado
# seria o esperado.

###
# Teste de estacionaridade
library(tseries)
adf.test(dif_icmsba_d.ts, alternative = "stationary")
# Novamente considerando a probabilidade do erro tipo I, probabilidade de rejeitar H0 dado que H0
# ? verdadeiro, igual a 5%, temos que a hip?tese nula de n?o estacionaridade deve ser rejeitada, 
# dado que p-valor, que representa a probabilidade de cometer este erro tipo I considerando os dados
# amostrais ? de aproximadamente 0,01.
# Assim temos que esta serie temporal continua a possuir estacionaridade mesmo apos a aplica??o de uma diferencia??o.

###

# Previsão 
library(forecast)
a <- seq(284,295)
treino_dif_icmsba_d.ts <- ts(dif_icmsba_d.ts[-a], start = c(1995,1), frequency = 12)
teste_dif_icmsba_d.ts <- ts(dif_icmsba_d.ts[a], start = c(2018,7), frequency = 12)
t1 <- forecast(treino_dif_icmsba_d.ts, h=12, level = c(95,99))
plot(forecast(t1, h=12, level = c(95,99)),shadecols = c( "#9ECAE1", "#6BAED6"), fcol = "darkblue", flwd = 1)
# Por conta da sazionalidade podemos prever por bastante tempo.  

rmse1 <- rmse(t1$mean,teste_dif_icmsba_d.ts)
rae1 <- rae(t1$mean,teste_icmsba_d.ts)
mae1 <- mae(t1$mean,teste_dif_icmsba_d.ts)
rrse1 <- rrse(t1$mean,teste_dif_icmsba_d.ts)


####
# Segunda forma de retirar a tendência taxa de crescimento, log
####
log_icmsba_d.ts <- log(icmsba_d.ts)
plot(log_icmsba_d.ts)
plot(decompose(log_icmsba_d.ts))
# Vemos no grafico que a tendencia de ordem crescente se mantem, tendencia essa
# que se mantem bastante visivel apartir dos anos 2000, n?o demostrando nenhuma
# grande mudan?a em compara??o com a serie sem a aplica??o do log, alem disso podemos
# observar uma altera??o na sazionalidade desta serie, mas mantendo-se sazional
# assim n?o possuindo uma altera??o significativa nestes aspectos da serie temporal original.

# ACF e PACF
acf(log_icmsba_d.ts)
pacf(log_icmsba_d.ts)
# Novamente observa-se que todos os lag do ACF est?o fora do intervalo de confian?a indicando
# uma possibilidade elevada de n?o ser representado por um modelo AR, alem disso o PACF novamente
# ap?s o terceiro lag demonstra uma queda exponencial, indicando de forma ainda mais direta uma
# poss?vel representa??o de um modelo MA(2), mas por se manter a exist?ncia de sazonalidade e da
# tend?ncia, est? serie possivelmete se encaixaria melhor em um modelo SARIMA.


# Teste de estacionaridade
library(tseries)
adf.test(log_icmsba_d.ts, alternative = "stationary")

# Novamente considerando a probabilidade do erro tipo I, probabilidade de rejeitar H0 dado que H0
# ? verdadeiro, igual a 5%, temos que a hip?tese nula de n?o estacionaridade deve ser rejeitada, 
# dado que p-valor, que representa a probabilidade de cometer este erro tipo I considerando os dados
# amostrais ? de aproximadamente 0,01.
# Assim temos que esta serie temporal continua a possuir estacionaridade mesmo apos a aplica??o do log.


###

####

# Previsão 
library(forecast)
a <- seq(288,296)
treino_log_icmsba_d.ts <- ts(log_icmsba_d.ts[-a], start = c(1995,1), frequency = 12)
teste_log_icmsba_d.ts <- ts(log_icmsba_d.ts[a], start = c(2018,8), frequency = 12)
t2 <- forecast(treino_log_icmsba_d.ts, h=12, level = c(95,99))
plot(forecast(t2, h=12, level = c(95,99)),shadecols = c( "#9ECAE1", "#6BAED6"), fcol = "darkblue", flwd = 1)
# Por conta da sazionalidade podemos prever por bastante tempo.  

rmse2 <- rmse(t2$mean,teste_log_icmsba_d.ts)
rae2 <- rae(t2$mean,teste_log_icmsba_d.ts)
mae2 <- mae(t2$mean,teste_log_icmsba_d.ts)
rrse2 <- rrse(t2$mean,teste_log_icmsba_d.ts)


####
# Terceira forma de serie removendo a tendencia

icms.sem.tend.ts <- icmsba_d.ts - icmsba_d_STL.ts$time.series[, "trend"]
plot(icms.sem.tend.ts)
plot(decompose(icms.sem.tend.ts))
# Vemos no grafico que a tendencia foi removida, assim apresentando aleatoriedade em seus dados
# observanfo a sazionalidade desta serie, temos que ela permanece proxima a da serie de dados original
# mas como o principal objetivo desta transforma??o ? o de remover a tendencia, ele foi visivelmente alcan?ado.

# ACF e PACF
acf(icms.sem.tend.ts)
pacf(icms.sem.tend.ts)
# Apartir do ACF e do PACF n?o somos capazes de termos uma ideia de qual seria o modelo que 
# melhor representaria esta serie temporal, sendo assim nescessario uma analise mais aprofundada
# para poder determinarmos qual seria o modelo que melhor representaria esta serie temporal.

# Teste de estacionaridade
library(tseries)
adf.test(icms.sem.tend.ts)
# Novamente considerando a probabilidade do erro tipo I, probabilidade de rejeitar H0 dado que H0
# ? verdadeiro, igual a 5%, temos que a hip?tese nula de n?o estacionaridade deve ser rejeitada, 
# dado que p-valor, que representa a probabilidade de cometer este erro tipo I considerando os dados
# amostrais ? de aproximadamente 0,01.
# Assim temos que esta serie temporal continua a possuir estacionaridade mesmo apos remo??o de sua tendencia.



# Previsão 
library(forecast)
a <- seq(288,296)
treino_icms.sem.tend.ts <- ts(icms.sem.tend.ts[-a], start = c(1995,1), frequency = 12)
teste_icms.sem.tend.ts <- ts(icms.sem.tend.ts[a], start = c(2018,8), frequency = 12)
t3 <- forecast(treino_icms.sem.tend.ts, h=12, level = c(95,99))
plot(forecast(t2, h=12, level = c(95,99)),shadecols = c( "#9ECAE1", "#6BAED6"), fcol = "darkblue", flwd = 1)
# Por conta da sazionalidade podemos prever por bastante tempo.  

rmse3 <- rmse(t3$mean,teste_icms.sem.tend.ts)
rae3 <- rae(t3$mean,teste_icms.sem.tend.ts)
mae3 <- mae(t3$mean,teste_icms.sem.tend.ts)
rrse3 <- rrse(t3$mean,teste_icms.sem.tend.ts)

####
# Avaliando o melhor modelo de previsão

mRMSE <- c(rmse0, rmse1, rmse2, rmse3)
mrae <- c(rae0, rae1, rae2, rae3)
mmae <- c(mae0, mae1, mae2, mae3)
mrrse <- c(rrse0, rrse1, rrse2, rrse3)

which(mRMSE == min(mRMSE))
which(mrae == min(mrae))
which(mmae == min(mmae))
which(mrrse == min(mrrse))

# Melhor modelo para previsão é o log
par(mfrow = c(2,2))
plot(mRMSE, pch = 19, col = c("yellow", "red", "green", "blue"), ylim = c(0, 3e8),
    xlab = "Série Original, Série diferenciada, \n Log da Série, Série - Trend", 
    ylab = " ",
    main = "Raiz do Erro \n Quadrático Médio")
plot(mrae, pch = 19, col = c("yellow", "red", "green", "blue"),  ylim = c(0, 50),
     xlab = "Série Original, Série diferenciada, \n Log da Série, Série - Trend",
     ylab = " ",
     main = "Raiz do Erro \n Absoluto")
plot(mmae, pch = 19, col = c("yellow", "red", "green", "blue"),  ylim = c(0, 2e8),
     xlab = "Série Original, Série diferenciada, \n Log da Série, Série - Trend",
     main = "Erro Médio Absoluto")
plot(mrrse, pch = 19, col = c("yellow", "red", "green", "blue"),  ylim = c(0, 22),
     xlab = "Série Original, Série diferenciada, \n Log da Série, Série - Trend", 
     ylab = " ",
     main = "Erro \n quadrático relativo")
  
  
# Observando o plot das medidas utilizadas para avaliar qual série 
# possui erro de previsão menor, usando o método de suavização exponencial
# para as quatro medidas avaliadas Raiz do Erro 
