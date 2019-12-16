rm(list=ls())


# Pegando o Banco de Dados ------------------------------------------------

ficheiro <- file.choose()
captacao_pre <- read.table(file = ficheiro, header = FALSE, dec=".")
attach(captacao_pre)
head(captacao_pre)

# Arrumando as Variaveis
datas <- captacao_pre$V1
captacao <- captacao_pre$V2


# Análise Descritiva já considerando a inflação ----------------------------
# da Captação Pré Fixada de Depósitos a Prazo de Pessoas Físicas de 02/01/1995 a 02/12/2019.

tam <- length(captacao)
captacao_d <- captacao/captacao[tam]
summary(captacao_d)

# Maximo obs 357 foi em 10/06/1996
max(captacao_d)
which(captacao_d == max(captacao_d))
datas[which(captacao_d == max(captacao_d))]

# Mínimo obs 1751 foi em 31/12/2001
min(captacao_d) 
which(captacao_d == min(captacao_d))
datas[which(captacao_d == min(captacao_d))]

# A mediana nesses anos do ICMS da Bahia foi
median(captacao_d)
mean(captacao_d)


# Utilizando calculos de matemática financeira  o ano possui 252 dias
# úteis e o mês tem aproximadamente 21 dias úteis, é aproximadamente
# porque a quantidade de dias úteis pode variar de ano para ano.
# Por esse motivo para colocar a variável captação em forma de 
# serie temporal o argumento frequency é colocado como 252.

library(tseries)
plot(ts(captacao_d, start = c(1995,1), frequency = 252))
# Observando algumas medidas básicas podemos ver que a série 
# Captação Pré Fixada de Depósitos a Prazo de Pessoas Físicas
# possui seus valores mais elevados em meados de 1996 alem de
# possuir um outlier em 10/06/1996, alem de possuir a media e a 
# mediana mais elevada influenciada por valores muito altos de 1995
# a 2000 e de 2000 em diante diminuindo a amplitude da serie,
# atingindo o seu mínimo em 31/12/2001. Por observar essa ocorrencia
# com a série antes de 2001 e depois de 2001 vamos fazer análises
# separadas.


# Formato de Série Temporal separado em dois períodos de tempo ------
# Separando a série em antes de 2001 e depois de 2001 ---------------------

a <- seq(1, 1501)
b <- seq(1502, 6254)

antes2001 <- ts(captacao_dts[a], start = c(1995,1), frequency = 252)
depois2001 <- ts(captacao_dts[b], start = c(1995,1), frequency = 252)
# Plot da série e plot do decompose

# Pegando a tendencia para Plotar a série com a tendência  ----------------
antes2001_STL.ts <- stl(antes2001, "periodic")
plot(antes2001, main = "Série da Captação de Depósitos feitos antes de 2001 \n com a tendência",
     xlab = "Tempo", ylab = "Captação por mil")
lines(antes2001_STL.ts$time.series[, "trend"], lwd = 1.4, col = "yellow")

# No cenário antes de 2001
# Vemos no grafico uma clara tendencia de ordem crescente de 95 a 96,
# demostrando mudança nesta tendencia de 96 ao final de 97,
# após 1997 é possível observar novamente um crescimento nos depósitos
# pré fixados a prazo e em amarelo está a tendencia da serie temporal,
# observa-se claramente a tendencia novamente, com mais facilidade por estar 
# sobreposta a propria serie temporal.

depois2001_STL.ts <- stl(depois2001, "periodic")
plot(depois2001, main = "Série da Captação de Depósitos feitos depois de 2001 \n com a tendência",
     xlab = "Tempo", ylab = "Captação por mil")
lines(depois2001_STL.ts$time.series[, "trend"], lwd = 1.4, col = "yellow")

# No cenário depois de 2001
# Vemos no grafico uma leve porém perceptível tendencia de ordem
# decrescente em toda a série de Captação pré fixada, demostrando
# nenhuma grande mudança nesta tendencia e em amarelo está a tendencia
# da serie temporal, observa-se claramente a tendencia pode até 
# oscilar, mas não muito durante o tempo, porém a inclinação
# descrente está presente.

plot(decompose(antes2001))
# Para o cenário antes de 2001
# A partir do plot do decompose é possível observar a série
# a tendência a sazonalidade que é perceptivel identificar
# picos mais altos e bem baixos em dezembro e janeiro dos anos
# observados e em quarto está o erro.

plot(decompose(depois2001))
# Para o cenário depois de 2001
# A partir do plot do decompose é possível observar a série
# a tendência, com muita mais clareza podemos identificar a
# tendencia completamente decrescente a sazonalidade que é 
# perceptivel e em quarto está o erro.


# ACF e PACF --------------------------------------------------------------
acf(antes2001, main = " Função de Autocorrelação de 02jan1995 a 29dez2001", ylab = "Autocorrelação")
acf(depois2001, main = " Função de Autocorrelação de 02jan2001 a 02dez2019", ylab = "Autocorrelação")

pacf(antes2001, main = "Função de Autocorrelação Parcial, antes de 2001", ylab = "Autocorrelação parcial")
pacf(depois2001, main = "Função de Autocorrelação Parcial, depois de 2001  \n ", ylab = "Autocorrelação parcial")

# Observa-se que todos os lag do ACF est?o fora do intervalo de confian?a assim indicando
# uma possibilidade elevada de n?o ser representado por um modelo AR, alem disso o PACF
# ap?s o terceiro lag demonstra uma queda exponencial, se aproximando do zero e se tornando
# oscilat?rio ap?s isso indicaria uma poss?vel representa??o de um modelo MA(2), mas pela exist?ncia
# de sazonalidade e da tend?ncia, est? serie possivelmete se encaixaria melhor em um modelo SARIMA,
# mas como n?o fomos apresentados para estes modelos n?o podemos analisar com maiores detalhes esta 
# possibilidade.

# Pela fun??o auto.arima temos indicios que o melhor modelo para representar esta serie
# seria o modelo MA(2) com uma diferencia??o, e que possui sazionalidade na serie assim sendo
# o modelo seria um SMA(2)


# Teste de estacionaridade ------------------------------------------------
library(tseries)
adf.test(antes2001, alternative = "stationary")
adf.test(depois2001, alternative = "stationary")

# Ao nivel de significancia de 5%, ou seja, considerando a probabilidade do erro tipo I,
# probabilidade de rejeitar H0 dado que H0 ? verdadeiro, igual a 5%, temos que a hip?tese nula
# de n?o estacionaridade deve ser rejeitada, dado que p-valor, que representa a probabilidade de
# cometer este erro tipo I considerando os dados amostrais ? de aproximadamente 0,01.
# Assim temos que esta serie temporal possui estacionaridade.



# Previsão, "Exponential Smoothing" ---------------------------------------

library(forecast)
a1 <- seq(1249, 1501)
treino_antes2001 <- ts(antes2001[-a1], start = c(1995,1), frequency = 252)
teste_antes2001 <- ts(antes2001[a1], start = c(2018,8), frequency = 252)
t00 <- forecast(treino_antes2001, h = 252, level = c(95,99))
plot(t00,shadecols = c( "green", "blue"), fcol = "darkblue", flwd = 1,
     main = "Previsão Exponential Smoothing para a captação, antes de 2001",
     xlab = "Tempo")

b1 <- seq(4502, 4753)
treino_depois2001 <- ts(depois2001[-b1], start = c(1995,1), frequency = 252)
teste_depois2001 <- ts(depois2001[b1], start = c(2018,8), frequency = 252)
t01 <- forecast(treino_depois2001, h = 252, level = c(95,99))
plot(t01,shadecols = c( "green", "blue"), fcol = "darkblue", flwd = 1,
     main = "Previsão Exponential Smoothing para captação, depois de 2001",
     xlab = "Tempo")






# Primeira forma de retirar a tendência tomando uma diferença -------------

dif_antes2001 <- diff(antes2001)
dif_depois2001 <- diff(depois2001)

# Pegando a tendencia para Plotar a série com a tendência  ----------------
dif_antes2001_STL.ts <- stl(dif_antes2001, "periodic")
plot(dif_antes2001, main = "Série da Captação de Depósitos diferenciado uma vez \n feitos antes de 2001 com a tendência")
lines(dif_antes2001_STL.ts$time.series[, "trend"], lwd = 1.4, col = "yellow")

dif_depois2001_STL.ts <- stl(dif_depois2001, "periodic")
plot(dif_depois2001, main = "Série da Captação de Depósitos diferenciado uma vez \n feitos depois de 2001 com a tendência")
lines(dif_depois2001_STL.ts$time.series[, "trend"], lwd = 1.4, col = "yellow")

# Verificamos em vermelho a tendencia da serie temporal, precisaremos destes valore para alguns possiveis
# ajusates para esta serie temporal, alem disso observa-se claramente a tendencia novamente, desta vez
# ainda mais facilmente por estar sobreposta a propria serie temporal.


plot(decompose(dif_antes2001))
plot(decompose(dif_depois2001))
# Vemos no grafico que a tendencia foi removida, assim apresentando aleatoriedade em seus dados
# observanfo a sazionalidade desta serie, temos que ela permanece proxima a da serie de dados original
# assim temos que a diferencia??o conseguiu remover a tendencia da serie
# Plot da série e plot do decompose



# ACF e PACF --------------------------------------------------------------
acf(dif_antes2001, main = " Função de Autocorrelação da série diferenciada \n uma vez, antes de 2001", ylab = "Autocorrelação")
acf(dif_depois2001, main = " Função de Autocorrelação da série diferenciada \n uma vez, depois de 2001  \n ", ylab = "Autocorrelação")

pacf(dif_antes2001, main = "Função de Autocorrelação Parcial da série diferenciada \n uma vez, antes de 2001", ylab = "Autocorrelação parcial")
pacf(dif_depois2001, main = "Função de Autocorrelação Parcial da série diferenciada \n uma vez, depois de 2001  \n ", ylab = "Autocorrelação parcial")


# Observa-se que todos os lag do ACF est?o fora do intervalo de confian?a assim indicando
# uma possibilidade elevada de n?o ser representado por um modelo AR, alem disso o PACF
# ap?s o terceiro lag demonstra uma queda exponencial, se aproximando do zero e se tornando
# oscilat?rio ap?s isso indicaria uma poss?vel representa??o de um modelo MA(2), mas pela exist?ncia
# de sazonalidade e da tend?ncia, est? serie possivelmete se encaixaria melhor em um modelo SARIMA,
# mas como n?o fomos apresentados para estes modelos n?o podemos analisar com maiores detalhes esta 
# possibilidade.

# Pela fun??o auto.arima temos indicios que o melhor modelo para representar esta serie
# seria o modelo MA(2) com uma diferencia??o, e que possui sazionalidade na serie assim sendo
# o modelo seria um SMA(2)

# Analisando o ACF e o PACF em conjundo gera-se uma duvida em qual modelo melhor se aplica para esta serie
# pois o PACF mantem sua queda exponencial apois o terceiro lag, mas o ACF apesar de n?o ser t?o claro demostra
# possiveis sinais da possibilidade da existencia da um AR na serie, assim gerando duvidas entre um modelo ARMA e um modelo MA
# lembrando que pela existencia da sazionalidade teriamos que incluir um componente para esta sazionalidade no modelo
# Como a fun??o auto.arima temos indicios de que o modelo que melhor se aplica para esta serie temporal seria um 
# MA(2) com o componente se sazionalidade, como foi aplicada uma diferencia??o na serie original que tinha como
# um possivel modelo de melhor aplica??o um MA(2) com o componente sazional e uma diferencia??o ent?o este resultado
# seria o esperado.


# Teste de estacionaridade ------------------------------------------------
library(tseries)
adf.test(dif_antes2001, alternative = "stationary")
adf.test(dif_depois2001, alternative = "stationary")

# Ao nivel de significancia de 5%, ou seja, considerando a probabilidade do erro tipo I,
# probabilidade de rejeitar H0 dado que H0 ? verdadeiro, igual a 5%, temos que a hip?tese nula
# de n?o estacionaridade deve ser rejeitada, dado que p-valor, que representa a probabilidade de
# cometer este erro tipo I considerando os dados amostrais ? de aproximadamente 0,01.
# Assim temos que esta serie temporal possui estacionaridade.


# Novamente considerando a probabilidade do erro tipo I, probabilidade de rejeitar H0 dado que H0
# ? verdadeiro, igual a 5%, temos que a hip?tese nula de n?o estacionaridade deve ser rejeitada, 
# dado que p-valor, que representa a probabilidade de cometer este erro tipo I considerando os dados
# amostrais ? de aproximadamente 0,01.
# Assim temos que esta serie temporal continua a possuir estacionaridade mesmo apos a aplica??o de uma diferencia??o.



# Previsão, "Exponential Smoothing" ---------------------------------------

library(forecast)
a1 <- seq(1249, 1501)
treino_dif_antes2001 <- ts(dif_antes2001[-a1], start = c(1995,1), frequency = 252)
teste_dif_antes2001 <- ts(dif_antes2001[a1], start = c(2018,8), frequency = 252)
t11 <- forecast(treino_dif_antes2001, h = 252, level = c(95,99))
plot(t11,shadecols = c( "green", "blue"), fcol = "darkblue", flwd = 1,
     main = "Previsão Exponential Smoothing para a Captação \n diferenciado uma vez, antes de 2001",
     xlab = "Tempo")

b1 <- seq(4502, 4753)
treino_dif_depois2001 <- ts(dif_depois2001[-b1], start = c(1995,1), frequency = 252)
teste_dif_depois2001 <- ts(dif_depois2001[b1], start = c(2018,8), frequency = 252)
t12 <- forecast(treino_dif_depois2001, h = 252, level = c(95,99))
plot(t12,shadecols = c( "green", "blue"), fcol = "darkblue", flwd = 1,
     main = "Previsão Exponential Smoothing para o ICMS \n diferenciado uma vez, depois de 2001",
     xlab = "Tempo")


# Segunda forma de retirar a tendência taxa de crescimento, log -----------

log_antes2001 <- log(antes2001)
log_depois2001 <- log(depois2001)

log_antes2001_STL.ts <- stl(log_antes2001, "periodic")
plot(log_antes2001, main = "Série da Taxa de Crescimento da Captação de Depósitos \n feitos antes de 2001 com a tendência")
lines(log_antes2001_STL.ts$time.series[, "trend"], lwd = 1.4, col = "yellow")

log_depois2001_STL.ts <- stl(log_depois2001, "periodic")
plot(log_depois2001, main = "Série da Taxa de Crescimento da Captação de Depósitos \n feitos depois de 2001 com a tendência")
lines(log_depois2001_STL.ts$time.series[, "trend"], lwd = 1.4, col = "yellow")

plot(decompose(log_antes2001))
plot(decompose(log_depois2001))
# Vemos no grafico que a tendencia de ordem crescente se mantem, tendencia essa
# que se mantem bastante visivel apartir dos anos 2001, n?o demostrando nenhuma
# grande mudan?a em compara??o com a serie sem a aplica??o do log, alem disso podemos
# observar u    ma altera??o na sazionalidade desta serie, mas mantendo-se sazional
# assim n?o possuindo uma altera??o significativa nestes aspectos da serie temporal original.

# ACF e PACF --------------------------------------------------------------
acf(log_antes2001, main = " Função de Autocorrelação da Taxa de Crescimento da Captação \n de Depósitos, antes de 2001", ylab = "Autocorrelação")
acf(log_depois2001, main = " Função de Autocorrelação da Taxa de Crescimento da Captação \n de Depósitos, depois de 2001", ylab = "Autocorrelação")

pacf(log_antes2001, main = "Função de Autocorrelação Parcial da Taxa de Crescimento da Captação \n de Depósitos, antes de 2001", ylab = "Autocorrelação parcial")
pacf(log_depois2001, main = "Função de Autocorrelação Parcial da Taxa de Crescimento da Captação \n de Depósitos, depois de 2001  \n ", ylab = "Autocorrelação parcial")

# Novamente observa-se que todos os lag do ACF est?o fora do intervalo de confian?a indicando
# uma possibilidade elevada de n?o ser representado por um modelo AR, alem disso o PACF novamente
# ap?s o terceiro lag demonstra uma queda exponencial, indicando de forma ainda mais direta uma
# poss?vel representa??o de um modelo MA(2), mas por se manter a exist?ncia de sazonalidade e da
# tend?ncia, est? serie possivelmete se encaixaria melhor em um modelo SARIMA.


# Teste de estacionaridade ------------------------------------------------
library(tseries)
adf.test(log_antes2001, alternative = "stationary")
adf.test(log_depois2001, alternative = "stationary")

# Novamente considerando a probabilidade do erro tipo I, probabilidade de rejeitar H0 dado que H0
# ? verdadeiro, igual a 5%, temos que a hip?tese nula de n?o estacionaridade deve ser rejeitada, 
# dado que p-valor, que representa a probabilidade de cometer este erro tipo I considerando os dados
# amostrais ? de aproximadamente 0,01.
# Assim temos que esta serie temporal continua a possuir estacionaridade mesmo apos a aplica??o do log.


# Previsão, "Exponential Smoothing" ---------------------------------------

library(forecast)
a1 <- seq(1249, 1501)
treino_log_antes2001 <- ts(log_antes2001[-a1], start = c(1995,1), frequency = 252)
teste_log_antes2001 <- ts(log_antes2001[a1], start = c(2018,8), frequency = 252)
t21 <- forecast(treino_log_antes2001, h = 252, level = c(95,99))
plot(t21,shadecols = c( "green", "blue"), fcol = "darkblue", flwd = 1,
     main = "Previsão Exponential Smoothing para a taxa de \n crescimento da Captação para antes de 2001",
     xlab = "Tempo")


b1 <- seq(4502, 4753)
treino_log_depois2001 <- ts(log_depois2001[-b1], start = c(1995,1), frequency = 252)
teste_log_depois2001 <- ts(log_depois2001[b1], start = c(2018,8), frequency = 252)
t22 <- forecast(treino_log_depois2001, h = 252, level = c(95,99))
plot(t22,shadecols = c( "green", "blue"), fcol = "darkblue", flwd = 1,
     main = "Previsão Exponential Smoothing para a taxa de \n crescimento da Captação para depois de 2001",
     xlab = "Tempo")




# Terceira forma de serie removendo a tendencia ---------------------------

antes2001_sem_tend_ts <- antes2001 - antes2001_STL.ts$time.series[, "trend"]
depois2001_sem_tend_ts <- depois2001 - depois2001_STL.ts$time.series[, "trend"]

sem_antes2001_STL.ts <- stl(antes2001_sem_tend_ts, "periodic")
plot(antes2001_sem_tend_ts, main = "Série da Taxa de Crescimento da Captação de Depósitos \n feitos antes de 2001 com a tendência")
lines(sem_antes2001_STL.ts$time.series[, "trend"], lwd = 1.4, col = "yellow")

sem_depois2001_STL.ts <- stl(depois2001_sem_tend_ts, "periodic")
plot(depois2001_sem_tend_ts, main = "Série da Taxa de Crescimento da Captação de Depósitos \n feitos depois de 2001 com a tendência")
lines(sem_depois2001_STL.ts$time.series[, "trend"], lwd = 1.4, col = "yellow")



plot(decompose(antes2001_sem_tend_ts))
plot(decompose(depois2001_sem_tend_ts))
# Vemos no grafico que a tendencia foi removida, assim apresentando aleatoriedade em seus dados
# observanfo a sazionalidade desta serie, temos que ela permanece proxima a da serie de dados original
# mas como o principal objetivo desta transforma??o ? o de remover a tendencia, ele foi visivelmente alcan?ado.

# ACF e PACF

acf(antes2001_sem_tend_ts, main = " Função de Autocorrelação do depósito sem a tendência \n, antes de 2001", ylab = "Autocorrelação")
acf(depois2001_sem_tend_ts, main = " Função de Autocorrelação do depósito sem a tendência \n, depois de 2001  \n ", ylab = "Autocorrelação")

pacf(antes2001_sem_tend_ts, main = "Função de Autocorrelação Parcial do depósito sem a tendência \n, antes de 2001", ylab = "Autocorrelação parcial")
pacf(depois2001_sem_tend_ts, main = "Função de Autocorrelação Parcial do depósito sem a tendência \n, depois de 2001  \n ", ylab = "Autocorrelação parcial")


# Apartir do ACF e do PACF n?o somos capazes de termos uma ideia de qual seria o modelo que 
# melhor representaria esta serie temporal, sendo assim nescessario uma analise mais aprofundada
# para poder determinarmos qual seria o modelo que melhor representaria esta serie temporal.

# Teste de estacionaridade
library(tseries)
adf.test(antes2001_sem_tend_ts)
adf.test(depois2001_sem_tend_ts)
# Novamente considerando a probabilidade do erro tipo I, probabilidade de rejeitar H0 dado que H0
# ? verdadeiro, igual a 5%, temos que a hip?tese nula de n?o estacionaridade deve ser rejeitada, 
# dado que p-valor, que representa a probabilidade de cometer este erro tipo I considerando os dados
# amostrais ? de aproximadamente 0,01.
# Assim temos que esta serie temporal continua a possuir estacionaridade mesmo apos remo??o de sua tendencia.

# Previsão, "Exponential Smoothing" ---------------------------------------

library(forecast)
a1 <- seq(1249, 1501)
treino_antes2001_sem_tend_ts <- ts(antes2001_sem_tend_ts[-a1], start = c(1995,1), frequency = 252)
teste_antes2001_sem_tend_ts <- ts(antes2001_sem_tend_ts[a1], start = c(2018,8), frequency = 252)
t31 <- forecast(treino_antes2001_sem_tend_ts, h = 252, level = c(95,99))
plot(t31,shadecols = c( "pink", "blue"), fcol = "darkblue", flwd = 1,
     main = "Previsão Exponential Smoothing para \n Captação sem a tendência",
     xlab = "Tempo")

b1 <- seq(4502, 4753)
treino_depois2001_sem_tend_ts <- ts(depois2001_sem_tend_ts[-b1], start = c(1995,1), frequency = 252)
teste_depois2001_sem_tend_ts <- ts(depois2001_sem_tend_ts[b1], start = c(2018,8), frequency = 252)
t42 <- forecast(treino_depois2001_sem_tend_ts, h = 252, level = c(95,99))
plot(t42,shadecols = c( "pink", "blue"), fcol = "darkblue", flwd = 1,
     main = "Previsão Exponential Smoothing para \n Captação sem a tendência",
     xlab = "Tempo")
# Por conta da sazionalidade podemos prever por bastante tempo.  


