library(ggplot2)
library(ggfortify)
library(forecast)
library(urca)
library(astsa)
library(fma)
library(expsmooth)

?uselec
autoplot(uselec)
ggseasonplot(uselec)

summary(uselec)

ndiffs(uselec)
nsdiffs(uselec)

modelo1 <- auto.arima(uselec, ic = "aic")
modelo3 <- auto.arima(uselec, ic = "bic")


d_sarima1 <- sarima(uselec, 1,0,2, 0,1,1, 12)
d_sarima2 <- sarima(uselec, 1,0,0, 1,0,0, 12)
d_sarima3 <- sarima(uselec, 1,1,2, 0,1,1, 12)
d_sarima4 <- sarima(uselec, 1,1,0, 1,0,0, 12)


