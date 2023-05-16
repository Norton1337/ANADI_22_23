library(lubridate)
#1 - Comece por carregar o ficheiro (“ciclismo.csv”) para o ambiente do R, 
# verifique a sua dimensão e obtenha um sumário dos dados.

setwd("C:/Users/manu0/Desktop/RESTO/ANADI/anadi_isep_23/TP2")
data <- read.csv("ciclismo.csv")
dimensions <- dim(data) #11 colunas e 1000 linhas
data_summary <- summary(data) #Possui variaveis categorias, númericas e binárias

#2 - Derive um novo atributo Age usando como valor do atributo dob

ages <- interval(as.Date(data$dob), today()) %/% years(1)
data$age <- ages


#3 - 