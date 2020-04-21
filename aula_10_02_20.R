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


######################################################
# METODOS DE PREVISAO
####################################################
# media, 1, significa um passo a frente.#############
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

############################################################################
# Outro banco já #usdeaths
library(fpp)

usdeaths
autoplot(usdeaths)
ggseasonplot(usdeaths)

ggAcf(usdeaths)

ndiffs(usdeaths) # A série não precisa ser diferenciada d = 0.
nsdiffs(usdeaths) # serie precisa ser diferenciada uma vez D= 1
# Deacordo com o resultado obtido 

# criterios de seleção do modelo AIC, AICC, BIC para selecionar o melhor modelo
# trabalhando com os erros, ou seja o modelo com menor erro será o selecionado.

#  Se eu utilizar a função ndiffs e resultar que não precisa de diferenciação
# e quando eu utilizar a função auto.arima e deu que d era diferente de 0 eu vou ter que
# utilizar o modelo feito pela função auto.arima e o modelo gerado novamente com auto.arima em que d = 0
# fazendo auto.arima(data, d = 0)
# e no fim fazer analise de residuo para comparar os dois possiveis modelos acima.

sarima1 <- auto.arima(usdeaths)
sarima2 <- auto.arima(usdeaths, d = 0)

sarima1
sarima2


ajuste1 <- sarima(usdeaths, 0,1,1,0,1,1,12)
ajuste2 <- sarima(usdeaths, 1,0,1,0,1,1,12) 


serieT <- window(usdeaths, start = 1973, end = c(1977,12))
ultimos_valores <- window(usdeaths, start = 1978)

length(serieT)
length(ultimos_valores)


# Previsão excluindo as 12 ultimas observações

usdeaths1 <- meanf(serieT, h = 12)
usdeaths2 <- rwf(serieT, h = 12)
usdeaths3 <- rwf(serieT, h = 12, drift = TRUE)
usdeaths4 <- snaive(serieT, h = 12)

autoplot(window(ultimos_valores)) + 
  autolayer(usdeaths1, series = "Mean", PI = FALSE) +
  autolayer(usdeaths2, series = "Naive", PI = FALSE) +
  autolayer(usdeaths3, series = "Naive com drift", PI = FALSE) +
  autolayer(usdeaths4, series = "Naive com sazonalidade", PI = FALSE) +
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


# Outro banco já #pigs
install.packages("fma")
install.packages("expsmooth")

library(fma)
library(expsmooth)

auto.arima(pigs)
summary(pigs)


?pigs
autoplot(pigs, col = "red", linetype = "dashed")
# Podemos ver que a serie não é estacionária.
# Possivelmente há sazonalidade.
# Pico decrescente em março de 1980.
ndiffs(pigs)
# Vimos que d = 1, (ndifs)
nsdiffs(pigs)
# vimos que D = 0
# Dado que D = 0, podemos afirmar que a série não é sazonal
# diz penas que a série nao precisa ser difereenciada na componente sazonal 

# o fato de ndiffs retornar 1 e nsdiffs retornar 0 siginifica que a ordem do modelo
# será algo como sarima(p,1,q)x(P,0,Q)


# pelos critérios de seleção AIC e AICC
d_arima1 <- auto.arima(pigs, ic = "aic")
d_arima2 <- auto.arima(pigs, ic = "bic")
# Compare os dois modelos d_arima1 e d_arima2 via seleção de modelos 
# no caso eu iria observar o s residuos, aic, bic, aicc dos dois modelos
# e ver numa tabelinha qual modelo obteve menores resiudos.
# tipo.

arima1 <- c(d_arima1$aic, d_arima1$aicc, d_arima1$bic)
arima2 <- c(d_arima2$aic, d_arima2$aicc, d_arima2$bic)
matriz <- rbind(arima1, arima2)

library(forecast)
d_sarima1 <- sarima(pigs, 2,1,0, 2,0,0,12)
d_sarima2 <- sarima(pigs, 2,1,0, 1,0,0,12)
d_sarima1
# d_sarima1 ao nível de significancia de 5%, temos que todos os parametros são
# são siginificativos (atenção para D (Fi grande) (autoregressivo na parte sazonal))
# que é significante a 5%, mas passa bem raspando.

d_sarima2
# d_sarima2 um modelo mais compacto independente do nível nominal, temos que todos
# os parametros são são siginificativos com p-valores bem baixos, todos p < 0e4.



# residuos esperado que esteja entre 3 e -3, 
# que todos os lags esteja dentro do intervalo de confiança dizendo que os resiudos são
# sao nao correlacionados

# os residuos seguem normalidade
# e no terceiro graifco que da o resultado do teste de Ljung Box, que todos os pontos
# estejam acima da linha pontilhada, ou seja rejeita-se a hipotese de que os residuos 
# não estejam bem ajustados

# ajustando o modelo com uma variavel dummy para a observação 
# colocando uma variável explicativa
x <- sarima(pigs, 2,1,0, 1,0,0,12, xreg = x)

ggseasonplot(pigs)