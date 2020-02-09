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
dev.off()

# Suavização expoenncial simples no R
library(forecast)
library(fpp)


# Plotando a serie temporal

lista <- list()
lista[[1]] <- ts(banco[ ,2], start = c(2017, 1), frequency = 12)
plot(lista[[1]])

# TESTE 1 suavização exponencial

ses(lista[[1]])

modelo1 <- ets(lista[[1]], model = "ANN")
modelo1      

# PLotando modelo1 temos:

plot(forecast(modelo1, h = 12)) # UMA mierda!!


