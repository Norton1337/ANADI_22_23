#Exercicio 2 iteração 1

# Install package corrplot
install.packages("corrplot")
install.packages("Hmisc")
install.packages("dplyr")


# load corrplot
library(corrplot)
library(Hmisc)
library(tidyr)
library(dplyr)
library(stats)

# Carregar data de dados2.csv

#Os valores estao rodeados de "" então vamos resolver isso


# Ler data do csv
csv_lines <- readLines("/Users/asus/Desktop/ANADI/iteracao_1/DADOS2.csv")

# Remover "" do cabecalho
csv_lines[1] <- gsub('"', '', csv_lines[1])

# Remover "" dos valores
for (i in 2:length(csv_lines)) {
  csv_lines[i] <- gsub('"', '', csv_lines[i])
}

# Escrever a data limpa
writeLines(csv_lines, "/Users/asus/Desktop/ANADI/iteracao_1/new_Dados2.csv")

#Ler do segundo ficheiro
dados <- read.csv("/Users/asus/Desktop/ANADI/iteracao_1/new_Dados2.csv", sep = ",")

#Extrair apenas a data relevante para a resolução do exercicio
algorithms_data <- dados[,3:7]

#2a

#Para verificar se está apto à correlacao, verificamos se segue dsitribuição normal

# Teste de normalidade de Shapiro-Wilk (para n<30)

# H0: a população é normalmente distribuída
# H1: a população não é normalmente distribuída

# Assumindo nível de significância alfa=0.05


shapiro.test(dados$SVM) # p-value=0.2687 > alfa, logo não se rejeita H0
shapiro.test(dados$DT)  # p-value=0.06772 > alfa, logo não se rejeita H0
shapiro.test(dados$KN)  # p-value=0.06926 > alfa, logo não se rejeita H0
shapiro.test(dados$RF)  # p-value=0.3138 > alfa, logo não se rejeita H0
shapiro.test(dados$ML)  # p-value=0.02138 < alfa=0.05, logo rejeita-se H0 e conclui-se que não segue uma distribuição normal 
shapiro.test(dados$GB)  # p-value=0.5125 > alfa, logo não se rejeita H0


#Fazer matriz de pearson de relações
corr_matrix <- rcorr(as.matrix(algorithms_data), type="pearson")
print(corr_matrix)



#representar num gráfico
corrplot(corr_matrix$r, method = 'circle', type = 'upper', order = 'hclust', 
         tl.col = 'black', tl.srt = 45, addCoef.col = 'black', 
         col = colorRampPalette(c("white", "orange"))(100), 
         addgrid.col = 'black')

#2b

# Seleciona as colunas 3 a 8 do conjunto de dados "dados" e armazena em "precisoes"
precisoes <- dados[, 3:8]

# Converte a matriz "precisoes" em uma matriz de dados numéricos e armazena em "matriz_precisoes"
matriz_precisoes <- as.matrix(precisoes)

# Executa o teste de Friedman na matriz de precisões "matriz_precisoes"
friedman.test(matriz_precisoes)


#2c

#Como as diferenças não são significativas, não é necessário fazer o posthoc
 
 
 