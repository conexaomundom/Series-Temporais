install.packages("xlsx")
install.packages("forecast")
install.packages("fpp")
install.packages("rstan")

library(xlsx)


banco <- read.xlsx("/home/marinaconexaomundo/Documentos/R_previsoes/banco_estoque.xlsx", "Página1")
nrow(banco)
names(banco)

# Graficos dos dados

serie_x1 <-ts(banco[2], start = c(2017, 1), frequency = 12)
  
pdf("serie_x1.pdf")
autoplot(serie_x1)
dev.off()

# PLot da sazonalidade
pdf("sazonal_x1.pdf")
ggseasonplot(serie_x1) 
# ggseasonplot(serie_x1, polar = T)  
dev.off()

# PLot da DEMANDA SAZONAL
pdf("demanda_sazonal_x1.pdf")
ggseasonplot(serie_x1) 
# ggseasonplot(serie_x1, polar = T)  
dev.off()

## REALMENTE fazendo previsões com suavização expoenncial
# PLot da DEMANDA SAZONAL
pdf("suavi_exp_x1.pdf")
autoplot(ses(serie_x1, h = 6))
dev.off()

# APARENTEMENTE NAO É UM BOM MÉTODO PARA O CASO
## Funcao Holt suavização exponencial simples estima o modelo de HOlt
# Que considera a tendencia
#
# previsao_holt_x1 <- holt(serie_x1, h = 6)
# pdf("previsao_holt_x1.pdf")
# plot(previsao_holt_x1)
# dev.off()

# Modelo de suavização exponencial com tendencia e saznalidade
# Holt-Winters
# Metodo aditivo e multiplicativo

# Holt-Winters Aditivo

previsao_aditivo_x1 <- hw(serie_x1, seasonal = "additive", PI = FALSE, h = 6)
 # previsao_mutiplicative_x1 <- hw(serie_x1, seasonal = "multiplicative", PI = FALSE)

pdf("HoltWinter_Aditivo.pdf")
autoplot(serie_x1, ylab = "Demanda de 20L água mineral sem gás") +
  autolayer(previsao_aditivo_x1, series = "HW Add.") 
#  autolayer(previsao_mutiplicative_x1, series = "HW Mult.")
dev.off()

# Suavização expoenncial simples no R
library(forecast)
library(fpp)

# Estimando modelos ARIMA 

modelo_arima <- forecast::auto.arima(serie_x1)

pdf("modeloArima_x1.pdf")
autoplot(forecast(modelo_arima), h = 6)
dev.off()

# O modelo "Naive"
# Nesse modelo, pressupôe que o futuro repetirá o passado, logo
# a previsão é correspondente ao último valor observado.

previsao_naive_x1 <- naive(serie_x1)





