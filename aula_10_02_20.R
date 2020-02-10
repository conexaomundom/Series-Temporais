install.packages("ggplot2")
install.packages("ggfortify")
install.packages("forecast")
install.packages("urca")
install.packages("astsa")


library(ggplot2)
library(ggfortify)
library(forecast)
library(urca)
library(astsa)

autoplot(AirPassengers)
summary(AirPassengers)
AirPassengers

autoplot(AirPassengers, col = "purple", linetype = 6)
?autoplot

# Ajuste e diagnostico
sarima(AirPassengers, 2,1,1,0,1,0,12)

meanf(AirPassengers, 1)
naive(AirPassengers, 1)
summary(AirPassengers, 1)
snaive(AirPassengers, 1)
rwf(AirPassengers, 1 ,drift = TRUE)


ndiffs(AirPassengers)
nsdiffs(AirPassengers)

dados <- auto.arima(AirPassengers)
plot(dados)
