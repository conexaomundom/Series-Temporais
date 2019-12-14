rm(list=ls())
install.packages("forecast")

ficheiro = file.choose()
bd = read.table(file = ficheiro, header = FALSE, dec=".")
attach(bd)
icms_bahia <- bd
attach(icms_bahia)
igpdi <- icms_bahia$V1
icms_ba <- icms_bahia$V2


# Sem considerar a inflação
summary(icms_ba)


boxplot(icms_ba, main = "ICMS mensal, \n Janeiro de 1995 |-| Agosto de 2019, Bahia",
                ylab = "ICMS")
# Maximo oBSERVAÇÃO 288
max(icms_ba)
which(icms_ba == max(icms_ba))
# Mínimo OBSERVAÇÃO 3
min(icms_ba)
which(icms_ba == min(icms_ba))

# A média nesses anos do ICMS da Bahia foi
median(icms_ba)


# IPEIA DADOS - IGP-DI - geral indice ago, 1944 = 100, mensal. 1994-2019.10
# ICMS ATE AGOSTO DE 2019
#
# VARIACAO QUE TEVE, IDENTIFICAR EM QUE MES ACONTECEU O MAXIMO, EM QUE MES ACONTECEU O MINIMO, ANALISAR,
# MAIS INFORMA??ES
# DESVIO PADRAO NA PARTE DESCRITIVA
# FATOS ECONOMICOS MES QUE HOUVE DIFERENTE VALORES
# CAPTULO 3 MORETIN

# Considerando a inflação
tam <- length(igpdi)
tam_icms <- length(icms_ba)

igpdi_d <- igpdi/igpdi[tam]
icms_d <- icms_ba / igpdi_d

boxplot(cbind(icms_ba, icms_d), main = "ICMS mensal, \n Janeiro de 1995 |-| Agosto de 2019, Bahia",
        ylab = "ICMS")

library(tseries)
icmsba_d.ts = ts(icms_d, start = c(1995,1), frequency = 12)
plot(icmsba_d.ts)
plot(decompose(icmsda_d.ts))

plot(icmsba_d.ts)
lines(icmsba_d_STL.ts$time.series[, "trend"], lwd = 1.4, col = "blue")

# ACF e PACF
acf(icmsba_d.ts)
pacf(icmsba_d.ts)

# Teste de estacionaridade
library(tseries)
adf.test(icmsba_d.ts, alternative = "stationary")

# AULA 04/12/2019 ---------------------------------------------------------

#EXERCIO 21 MORETIN

icmsba_d_STL.ts <- stl(icmsba_d.ts, "periodic")
plot(icmsba_d_STL.ts)

icmsba_d_STL.ts$time.series[,"trend"]

# Segunda forma de retirar a tendência taxa de crescimento, log
#

# Terceira forma de serie removendo a tendencia
icms.sem.tend <- icmsba_d.ts - icmsba_d_STL.ts$time.series[, "trend"]
plot(icms.sem.tend)
adf.test(icms.sem.tend)


# Previs?o
ARIMAC <- auto.arima(icmsba_d.ts) 
library(forecast)
plot(forecast(icmsba_d.ts, h=12, level = c(95,99)),shadecols = c( "#9ECAE1", "#6BAED6"), fcol = "darkblue", flwd = 1)
plot(forecast(icms.sem.tend, h=12, level = c(95,99)),shadecols = c( "#9ECAE1", "#6BAED6"), fcol = "darkblue", flwd = 1)
#Por conta da sazionalidade podemos prever por bastante tempo.