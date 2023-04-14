getwd()
data = read.csv("DADOS3.csv")

# [3]
# [3a]

# Dividir as 99 viaturas em três grupos: viaturas com 4 cilindros, com 6 e com 8.
dados4Cyl = subset(data, Cylinders == 4)
dados6Cyl = subset(data, Cylinders == 6)
dados8Cyl = subset(data, Cylinders == 8)


# Existirá diferenças significativas na aceleração entre os três grupos?
# H0: não existem diferenças significativas na aceleração entre os três grupos
# H1: existem diferenças significativas na aceleração entre três grupos


# Pressupostos do teste One-Way ANOVA
# 1 A variável dependente deve ser contínua
# 2 A variável independente deve ter dois ou mais grupos independentes.
#   Geralmente usa-se apenas um teste ANOVA para três ou mais grupos
#   categóricos independentes uma vez que para dois grupos é mais
#   cómodo fazer t-teste.
# 3 As observações devem ser independentes (Não devem existir relações
#                                          entre as observações de grupos diferentes).
# 4 As observações não devem ter outliers significativos
# 5 A variável dependente deve ser normalmente distribuída para cada grupo
#   (usar p.e. teste de Shapiro )
# 6 Deve existir homogeneidade de variâncias (usar teste de Levene)



# 4) As observações não devem ter outliers significativos
colours = c("#1f77b4", "#ff7f0e", "#2ca02c")
accelerations = list(dados4Cyl$Acceleration, dados6Cyl$Acceleration, dados8Cyl$Acceleration)
names(accelerations) = c("4 cylinders", "6 cylinders", "8 cylinders")
boxplot(accelerations, xlab = "Cylinders", ylab = "Acceleration", col = colours)


# Calcular os quartis de cada grupo
dados4CylQ1 = quantile(dados4Cyl$Acceleration, 0.25)
dados4CylQ3 = quantile(dados4Cyl$Acceleration, 0.75)
dados6CylQ1 = quantile(dados6Cyl$Acceleration, 0.25)
dados6CylQ3 = quantile(dados6Cyl$Acceleration, 0.75)
dados8CylQ1 = quantile(dados8Cyl$Acceleration, 0.25)
dados8CylQ3 = quantile(dados8Cyl$Acceleration, 0.75)

# Calcular amplitude de quartis de cada grupo
dados4CylIQR = IQR(dados4Cyl$Acceleration)
dados6CylIQR = IQR(dados6Cyl$Acceleration)
dados8CylIQR = IQR(dados8Cyl$Acceleration)


# Calcular os limites superiores e inferiores dos bigodes de cada grupo
dados4CylLower = dados4CylQ1 - 1.5 * dados4CylIQR
dados4CylUpper = dados4CylQ3 + 1.5 * dados4CylIQR

dados6CylLower = dados6CylQ1 - 1.5 * dados6CylIQR
dados6CylUpper = dados6CylQ3 + 1.5 * dados6CylIQR

dados8CylLower = dados8CylQ1 - 1.5 * dados8CylIQR
dados8CylUpper = dados8CylQ3 + 1.5 * dados8CylIQR

# Verificar não visalmente se existem outliers significativos

has_outliers_dados4 = any(any(dados4Cyl$Acceleration > dados4CylUpper | dados4Cyl$Acceleration < dados4CylLower))
has_outliers_dados6 = any(any(dados6Cyl$Acceleration > dados6CylUpper | dados6Cyl$Acceleration < dados4CylLower))
has_outliers_dados8 = any(any(dados8Cyl$Acceleration > dados8CylUpper | dados8Cyl$Acceleration < dados4CylLower))

if (has_outliers_dados4 || has_outliers_dados6 || has_outliers_dados8) {
  print("There are significant outliers.")
} else {
  print("There are no significant outliers.")
}




# 5) A variável dependente deve ser normalmente distribuída para cada grupo

# A variável dependente é normalmente distruibuída?
# H0: A variável dependente dos três grupos segue uma distruibuição normal
# H1: A variável dependente dos três grupos não segue uma distruibuição normal

# Verificar se p-value é maior que 0.05 em todos os grupos
shapiro.test(dados4Cyl$Acceleration) # p-value = 0.1056 
shapiro.test(dados6Cyl$Acceleration) # p-value = 0.03628 x
shapiro.test(dados8Cyl$Acceleration) # p-value = 0.2729 
# O grupo dados6Cyl não tem a variável dependentente normalmente
#   distribuída logo não podemos utilizar o teste One-Way ANOVA


# p-value é menor que 0.05 num grupo, ou seja, vamos usar testes não parametricos

# - Teste de Kruskal-Wallis -
# Este teste é a alternativa não paramétrica do teste One-Way ANOVA.
# Deve-se utilizar quando a hipótese da normalidade for rejeitada ou se
#   o tamanho das amostras forem pequenas

grupos  = factor(c(rep("Grupo1",length(dados4Cyl$Cylinders)),
                   rep("Grupo2", length(dados6Cyl$Cylinders)),
                   rep("Groupo3",length(dados8Cyl$Cylinders))))
grouposAcceleration = c(dados4Cyl$Acceleration,dados6Cyl$Acceleration)

result = kruskal.test(data$Acceleration,grupos)
result # p-value = 2.795e-11

# O p-value ser menor que 0.05 leva-nos a rejeitar H0.
# Existem diferenças significativas na aceleração entre os três grupos de cilindros.


# [3b]

weight = data$Weight
horsepower = data$Horsepower
cylinders = factor(data$Cylinders)

#Modelo Regressão Linear
model = lm(Acceleration ~ weight + horsepower + cylinders, data = data)
summary(model)
# função par() permite controlar as propriedades do gráfico
par(mfrow = c(2,2)) # 2 linhas e 2 colunas
plot(model)

# Criar um predictor com um peso de 2950 kg, potência de 100 Hp e 4 cilindros. 

predictor = data.frame(weight = 2950, horsepower = 100, cylinders = factor(4))

# Estimar aceleração de um veiculo com o peso, potência e cilindros desejados
predicted_acceleration = predict(model, newdata = predictor)
# 17.30784