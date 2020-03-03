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

# O banco já vem em formato de series temporais com inico em 
# Janeiro de 1949 e final em dezembro de 1960, contentendo
# 144 observações mensais do numero total de passageiros
# internacionais na casa dos mil durante os 11 anos estudados 
# da linha aérea Box & Jenkins.
AirPassengers

autoplot(AirPassengers)
# Ao realizar um plot da série é possivel ver uma tendencia 
# crescente em todos os anos de estudo, e também uma sazonalidade
# bem presente, pois, anualmente o comportamento é o mesmo, 
# crescente, mas o mesmo, apenas se acentuando e deixando ainda
# mais claro.
ggseasonplot(AirPassengers)
# Ao fazer o plot já ficou claro uma tendencia cresvente ao longo
# dos anos, e agora ao realizar um plot para verficar se há
# realmente uma sazonalidade, é possivel ver uma pequena elevação
# até março, um comportameto um pouco constante entre março e maio
# e aí começa a elevação do pico em todos os anos entre julho e agosto
# talvez por ser o mês de férias, seria valido pesquisar se no país
# ou região da companhia aérea tem férias nessa época para explicar
# esse pico nessa época do ano, após esses meses há uma queda lenta de 
# agosto até novamabro e em seguida começa novamente o mesmo comportamento

summary(AirPassengers)
which(AirPassengers == 104)
which(AirPassengers == 622)

#  O minimo se encontra no mes de novembro de 1949 e o 
# maximo em julho de 1960. Esse valores nessas datas são
# explicados pela tendencia forte crescente o maximo ser no
# ultimo ano e o minimo no primeiro, e o pico e o minimo esta
# numa parte realmetente descrecenre.

# Vale salientar que do primeiro ao terceiro quartil só varia 180mil
# passageiros, mas do terceiro quartil para o maximo dobra o numero
#  de passageiros, ou seja, o pico nos meses no meio do ano são muito
# reelevantes porque eles diferenciam grandemente do restante do nuemro
# de passageiros em outras épocas ao longo do ano.

ndiffs(AirPassengers)
nsdiffs(AirPassengers)
auto.arima(AirPassengers)
