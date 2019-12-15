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

# Matrizes que vao receber as méidas das estimativas de rood mean square error
# a quantidade dessas matrizes 

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

###
# Colocando o ICMS da Bahia deflacionado em formato de série temporal
library(tseries)
icmsba_d.ts = ts(icms_d, start = c(1995,1), frequency = 12)
# Plot da série e plot do decompose
plot(icmsba_d.ts)
plot(decompose(icmsba_d.ts))

# ACF e PACF
acf(icmsba_d.ts)
pacf(icmsba_d.ts)

# Pegando a tendencia para 
icmsba_d_STL.ts <- stl(icmsba_d.ts, "periodic")
plot(icmsba_d.ts)
lines(icmsba_d_STL.ts$time.series[, "trend"], lwd = 1.4, col = "red")

###
# Teste de estacionaridade
library(tseries)
adf.test(icmsba_d.ts, alternative = "stationary")
###

####

# Previsão 
library(forecast)
t0 <- forecast(icmsba_d.ts, h=12, level = c(95,99))
plot(forecast(icmsba_d.ts, h=12, level = c(95,99)),shadecols = c( "#9ECAE1", "#6BAED6"), fcol = "darkblue", flwd = 1)
#Por conta da sazionalidade podemos prever por bastante tempo.  

rmse0 <- RMSE(t0,mat_teste0)
rae0 <- rae(t0,mat_teste0)
mae0 <- mae(t0,mat_teste0)
rrse0 <- rrse(t0,mat_teste0)

####
# Primeira forma de retirar a tendência tomando uma diferença
####
dif_icmsba_d.ts <- diff(icmsba_d.ts)
plot(dif_icmsba_d.ts)
plot(decompose(dif_icmsba_d.ts))

# ACF e PACF
acf(dif_icmsba_d.ts)
pacf(dif_icmsba_d.ts)

# Pegando a tendencia para 
dif_icmsba_d_STL.ts <- stl(dif_icmsba_d.ts, "periodic")
plot(dif_icmsba_d.ts)
lines(dif_icmsba_d_STL.ts$time.series[, "trend"], lwd = 1.4, col = "red")

###
# Teste de estacionaridade
library(tseries)
adf.test(dif_icmsba_d.ts, alternative = "stationary")
###

# Previsão 
library(forecast)
t1 <- forecast(dif_icmsba_d.ts, h=12, level = c(95,99))
plot(forecast(dif_icmsba_d.ts, h=12, level = c(95,99)),shadecols = c( "#9ECAE1", "#6BAED6"), fcol = "darkblue", flwd = 1)
  #Por conta da sazionalidade podemos prever por bastante tempo.  
rmse1 <- rmse(t1,mat_teste1)
rae1 <- rae(t1,mat_teste1)
mae1 <- mae(t1,mat_teste1)
rrse1 <- rrse(t1,mat_teste1)

####



####
# Segunda forma de retirar a tendência taxa de crescimento, log
####
log_icmsba_d.ts <- log(icmsba_d.ts)
plot(log_icmsba_d.ts)
plot(decompose(log_icmsba_d.ts))

# ACF e PACF
acf(log_icmsba_d.ts)
pacf(log_icmsba_d.ts)

# Pegando a tendencia para 
log_icmsba_d_STL.ts <- stl(log_icmsba_d.ts, "periodic")
plot(log_icmsba_d.ts)
lines(log_icmsba_d_STL.ts$time.series[, "trend"], lwd = 1.4, col = "red")

###
# Teste de estacionaridade
library(tseries)
adf.test(log_icmsba_d.ts, alternative = "stationary")
###

# Previsão 
library(forecast)
t2 <- forecast(log_icmsba_d.ts, h=12, level = c(95,99))
plot(forecast(log_icmsba_d.ts, h=12, level = c(95,99)),shadecols = c( "#9ECAE1", "#6BAED6"), fcol = "darkblue", flwd = 1)
#Por conta da sazionalidade podemos prever por bastante tempo.  

rmse2 <- RMSE(t2,mat_teste2)
rae2 <- rae(t2,mat_teste2)
mae2 <- mae(t2,mat_teste2)
rrse2 <- rrse(t2,mat_teste2)

####

####
# Terceira forma de serie removendo a tendencia
icms.sem.tend <- icmsba_d.ts - icmsba_d_STL.ts$time.series[, "trend"]
plot(icms.sem.tend)

plot(decompose(icms.sem.tend))

# ACF e PACF
acf(icms.sem.tend)
pacf(icms.sem.tend)

# Pegando a tendencia para 
icms.sem.tend_STL.ts <- stl(icms.sem.tend, "periodic")
plot(icms.sem.tend)
lines(icms.sem.tend_STL.ts$time.series[, "trend"], lwd = 1.4, col = "red")

###
# Teste de estacionaridade
library(tseries)
adf.test(icms.sem.tend, alternative = "stationary")
###

# Previsão 
library(forecast)
t3 <- forecast(icms.sem.tend, h=12, level = c(95,99))
plot(forecast(icms.sem.tend, h=12, level = c(95,99)),shadecols = c( "#9ECAE1", "#6BAED6"), fcol = "darkblue", flwd = 1)
#Por conta da sazionalidade podemos prever por bastante tempo.  

rmse3 <- RMSE(t3,mat_teste3)
rae3 <- rae(t3,mat_teste3)
mae3 <- mae(t3,mat_teste3)
rrse3 <- rrse(t3,mat_teste3)


####
# Avaliando o melhor modelo de previsão

mRMSE < -c(rmse0, rmse1, rmse2, rmse3)
mrae <- c(rae0, rae1, rae2, rae3)
mmae <- c(mae0, mae1, mae2, mae3)
mrrse <- c(rrse0, rrse1, rrse2, rrse3)

which(mRMSE == min(mRMSE))
which(mrae == min(mrae))
which(mmae == min(mmae))
which(mrrse == min(mmrse))
