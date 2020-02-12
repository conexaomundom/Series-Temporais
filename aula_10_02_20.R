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




ndiffs(AirPassengers)
nsdiffs(AirPassengers)

dados <- auto.arima(AirPassengers)
plot(dados)


d_forecasts <- forecast(dados, level = 95, h = 50)
autoplot(d_forecasts)


###############
# METODOS DE PREVISAO
#############
# media, 1, significa um passo a frente.
meanf(AirPassengers, 1)

# naive 
naive(AirPassengers, 1)
summary(AirPassengers, 1)
snaive(AirPassengers, 1)
rwf(AirPassengers, 1)

# drift
rwf(AirPassengers, 1 ,drift = TRUE)

AirPassengers_F <- window(AirPassengers, start = 1949, end = c(1960,12))
autoplot(AirPassengers_F) +
  autolayer(meanf(AirPassengers_F, h =11), series = "mean", PI = FALSE) +
  autolayer(naive(AirPassengers_F, h =11), series = "naive", PI = FALSE) +
  autolayer(snaive(AirPassengers_F, h =11), series = "Naive Sazonal", PI = FALSE) +
  autolayer(rwf(AirPassengers_F, h =11, drift = TRUE), series = "Naive com drift", PI = FALSE) +
  ggtitle("Total mensal de passageiros de companhias aéreas internacionais") +
  guides(colour = guide_legend(title = "Forecast"))


# Previsão excluindo as 12 ultimas observações

AirPassenger2 <- window(AirPassengers, start = 1949, end = c(1959,12))
length(AirPassenger2)

AirPassengersfit1 <- meanf(AirPassenger2, h = 12)
AirPassengersfit2 <- rwf(AirPassenger2, h = 12)
AirPassengersfit3 <- rwf(AirPassenger2, h = 12, drift = TRUE)
AirPassengersfit4 <- snaive(AirPassenger2, h = 12)

autoplot(window(AirPassengers, start = 1949)) + 
  autolayer(AirPassengersfit1, series = "Mean", PI = FALSE) +
  autolayer(AirPassengersfit2, series = "Naive", PI = FALSE) +
  autolayer(AirPassengersfit3, series = "Naive com drift", PI = FALSE) +
  autolayer(AirPassengersfit4, series = "Naive com sazonalidade", PI = FALSE) +
  ggtitle("Previsão para o número totl de passageiros") + 
  guides(colour = guide_legend(title = "PrevisãO"))
  
AirPassenger3 <- window(AirPassengers, start = 1960)
accuracy(AirPassengersfit1, AirPassenger3)[1, c(3,2,5)]

accuracy(AirPassengersfit2, AirPassenger3)[1, c(3,2,5)]

accuracy(AirPassengersfit3, AirPassenger3)[1, c(3,2,5)]

accuracy(AirPassengersfit4, AirPassenger3)[1, c(3,2,5)]
# Quem ganhou foi o terceiro modelo.  

##############################
# Alisamento exponencial
#############################

air <- window(AirPassengers, start = 1949)
fc <- holt(air, h = 5)
fc$model


fc <- holt(air, h = 15)
autoplot(air) +
  autolayer(fc, series = "AE, Holt", PI = FALSE)
  guides(colou = guide_legend(title = "Forecast"))
  
  autoplot(fc)

# Alisamento expoenncial aditivo e multiplicativo
air <- window(AirPassengers, start = 1949)
fit1 <- hw(AirPassenger2, seasonal = "additive", h = 12)
fit2 <- hw(AirPassenger2, seasonal = "multiplicative", h = 12)
  
autoplot(air) +
  autolayer(fit1, series = "HW - aditivo", PI = FALSE) +
  autolayer(fit2, series = "HW - multiplicativo", PI = FALSE) +
  guides(colou = guide_legend(title = "Forecast"))

# Vamos comparar as previsões - Métodos (mena, naive, drift) e AE

AirPassenger3 <- window(AirPassengers, start = 1960)


accuracy(fit1, AirPassenger3)[1, c(3,2,5)]
accuracy(fit2, AirPassenger3)[1, c(3,2,5)]
accuracy(AirPassengersfit1, AirPassenger3)[1, c(3,2,5)]
accuracy(AirPassengersfit2, AirPassenger3)[1, c(3,2,5)]
accuracy(AirPassengersfit3, AirPassenger3)[1, c(3,2,5)]
accuracy(AirPassengersfit4, AirPassenger3)[1, c(3,2,5)]


