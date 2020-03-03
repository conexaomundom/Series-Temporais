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
# Ao aplicar a função ndiffs no banco, resultará quantas vezes será necessário 
# diferenciar a série para torna la estacionária, ou seja, retirar a tendência 
# presente nela, que normalmente é 0, 1 ou 2, para o banco de numero de 
# passageiros de voos internacionais resultou em apenas uma diferenciação
# já resolve o problema de tendência.
nsdiffs(AirPassengers)
# Ao aplicar a função nsdiffs no banco, resultará quantas vezes será necessário 
# diferenciar a série na componente sazonal para torna la estacionária, ou seja,
# retirar a tendência presente da componente sazonal, que normalmente é 0, 1 ou
# 2 também, para o banco de numero de passageiros de voos internacionais resultou
# em apenas uma diferenciação também já resolve o problema de tendência na
# componente sazonal.

# Poderia explicar um pouco melhor sobre essa questão de componente sazonal
# fiquei um pouco na dúvida.

auto.arima(AirPassengers)

# ********************* Dúvida
# Como resultado do uso da função auto.arima diz a ordem do modelo, 
# que resulta numa estimação dos 
# parametros que explicam a série ARIMA(2,1,1)(0,1,0), em que foi estimado
# p = 2 no caso na parte regressima, d = 1, ou seja, a série precisa ser 
# diferenciada uma vez para corrigir o problema da tendência, mas já era esperado
#  pois ao utilizar a função ndiffs anteriormente tinha resultado que a série
# realmente necessatava de uma diferenciação e no parametro q da parte de
# médias móveis foi estimado 1. Além disso também é estimado a parte da componente
# sazonal em que resultou em P = 0, ou seja a parte autorregressiva da componente
# sazonal foi 0, D = 1 como esperado também que ao utilizar a função nsdiffs
# resultando que o numero de diferenciações para a parte sazonal ser estacionária 
# é 1 e Q = 0 também, ou seja, não há necessidade de parametro na parte de médias 
# móveis para a componente sazonal.
# ********************* Dúvida