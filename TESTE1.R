install.packages("xlsx")
library(xlsx)

banco <- read.xlsx("/home/marinaconexaomundo/Documentos/R_previsoes/banco_estoque.xlsx", "Página1")
nrow(banco)
names(banco)

# Suavização expoenncial simples no R

