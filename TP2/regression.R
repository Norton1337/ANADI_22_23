library(lubridate)
library(corrplot)
library(ggplot2)

#1 - Comece por carregar o ficheiro (“ciclismo.csv”) para o ambiente do R, 
# verifique a sua dimensão e obtenha um sumário dos dados.
setwd("D:/Documentos/3º Ano/2ºSemestre/ANADI/anadi_isep_23/TP2/data")
getwd()
#setwd("C:/Users/asus/Desktop/ANADI/iteracao_2/anadi_isep_23/TP2/data")
data <- read.csv("ciclismo.csv")
dimensions <- dim(data) #11 colunas e 1000 linhas
data_summary <- summary(data) #Possui variaveis categorias, númericas e binárias

#2 - Derive um novo atributo Age usando como valor do atributo dob

ages <- interval(as.Date(data$dob), today()) %/% years(1)
data$age <- ages


#3 - Analise os atributos do conjunto de dados mais significativos,
#usando gráficos, análises estatísticas e/ou outros métodos apropriados. 

count_gender <- table(data$gender)
# Criar o gráfico de barras para ver a distribuição do genero
barplot(count_gender, 
        main = "Gender Distribution",
        xlab = "Gender",
        col = "lightblue")

#Tranformando o atributo categorio binario Gender em binário númerico
# female - 0; male - 1
data$gender<- ifelse(data$gender == "female", 0, 1)

count_training_camp <- table(data$Winter.Training.Camp)
# Criar o gráfico de barras para ver a distribuição do genero
barplot(count_training_camp, 
        main = "Winter Training Camp",
        col = "lightblue")

#Repetindo o mesmo processo para a variável Winter.Training.Camp
# completed - 1; none - 0
data$Winter.Training.Camp<- ifelse(data$Winter.Training.Camp == "none", 0, 1)

count_pro_level <- table(data$Pro.level)
# Criar o gráfico de barras para ver a distribuição do genero
barplot(count_pro_level, 
        main = "Pro Level",
        col = "lightblue")

#Repetindo o mesmo processo para a variável Pro.level
# World Tour - 1; Continental - 0
data$Pro.level <- ifelse(data$Pro.level == "Continental", 0, 1)

#Transformando variáveis categoricas não binárias em numericas

count_countries <- table(data$Continent)
# Criar o gráfico de barras para ver a distribuição do genero
barplot(count_countries, 
        main = "Continets",
        col = "lightblue")

unique_countries <- unique(data$Continent)
data$Continent <- match(data$Continent, unique_countries)


count_teams <- table(data$Team)
# Criar o gráfico de barras para ver a distribuição do genero
barplot(count_teams, 
        main = "Teams",
        col = "lightblue")

unique_teams <- unique(data$Team)
data$Team <- match(data$Team, unique_teams)

count_background <- table(data$Background)
# Criar o gráfico de barras para ver a distribuição do genero
barplot(count_background, 
        main = "Background",
        col = "lightblue")

unique_backgrounds <- unique(data$Background)
data$Background <- match(data$Background, unique_backgrounds)

label_names <- c("Gender", "Team", "Background", "Level", "Winter Camp", "Altitude", "VO2", "HR", "Continent", "Age")
boxplot(data[,c(2:9,11,12)], names=label_names, col = c(1,2,3,4,5,6,7,8,10,11))

hist(data$age, main = "Age Histogram", xlab = "Ages", ylab = "Frequency", col = "lightblue")
hist(data$hr_results, main = "Hr Results Histogram", xlab = "Hr Results", ylab = "Frequency", col = "lightblue")
hist(data$vo2_results,  main = "VO2 Results Histogram", xlab = "VO2 Results", ylab = "Frequency", col = "lightblue")
hist(data$altitude_results,  main = "Altitude Results Histogram", xlab = "Altitude Results", ylab = "Frequency", col = "lightblue")

corrplot(cor(data[,c(2:9,11,12)]), method = "square")

corrplot(cor(data[,c(2:9,11,12)]), method = "color",
         type = "lower", order = "hclust",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.8,
         col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200))

ggplot(data, aes_string(x = data$altitude_results, y = data$vo2_results)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Altitude") +
  ylab("VO2 Results") +
  labs(title = paste("Correlation between Altitude and VO2 results"))

ggplot(data, aes_string(x = data$altitude_results, y = data$hr_results)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Altitude") +
  ylab("HR Results") +
  labs(title = paste("Correlation between Altitude and HR results"))

ggplot(data, aes_string(x = data$vo2_results, y = data$hr_results)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("VO2 Results") +
  ylab("HR Results") +
  labs(title = paste("Correlation between VO2 Results and HR results"))

#4 - Realize o pré-processamento dos dados: 
#a) Faça a identificação de NA e limpe o dataSet, se aplicável
data <- na.omit(data)

#b) Identifique dados inconsistentes e outliers, se aplicável 

outliers_altitude <- boxplot.stats(data$altitude_results)$out
outliers_hr <- boxplot(data$hr_results)$out
outliers_vo2 <- boxplot(data$vo2_results)$out

#Como os outliers representam apenas 0.6% dos dados, não consideramos significativos a sua retirada

#c) Implemente a seleção de atributos, se aplicável 
# Iremos retirar a coluna do ID e da Data de nascimento, 
#o ID é apenas ruído para as analises de regressão, e já a Data de nascimento
#, é uma variável categorica, dessa forma não é muito fácil utiliza-la

data <- data[,-c(1,10)]

#d) Implemente a normalização dos dados, se necessário 
# Como os algoritmos em geral tem a tendência em dar maior valor a valores
# númericos maiores, é aconselhado normalizar os dados, os dados binários que
# fizemos o OneHotEnconder já estão entre 0,1 ou entre [1-x], porém, outros
# atributos como a idade, altitude, possuem valores significamente mais altos, performaremos a normalização nesses atributos

min_max <- function(data_aux) {
  min_value <- min(data_aux)
  max_value <- max(data_aux)
  result <- (data_aux - min_value)/(max_value - min_value)
  return(result)
}


data$age <- min_max(data$age)
data$altitude_results <- min_max(data$altitude)
data$vo2_results <- min_max(data$vo2_results)
data$hr_results <- min_max(data$hr_results)
data$Continent <- min_max(data$Continent)
data$Background <- min_max(data$Background)
data$Team <- min_max(data$Team)
data$gender <- min_max(data$gender)

write.csv(data, "normalised_data.csv", row.names = FALSE)
normalised_data <- read.csv("normalised_data.csv")

#5 - Crie um diagrama de correlação entre todos os atributos. Comente o que observa. 

corrplot(cor(data), method="circle")

corrplot(cor(data), method = "color",
         type = "lower", order = "hclust",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.8,
         col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200))

#exercicio 6
#a)Apresente a função linear resultante.

# data <- read.csv("ciclismo.csv")

# hr_results <- data$hr_results

model <- lm(altitude_results ~ hr_results, data = data)
summary(model)

# Check for any missing

#b) Visualize a reta correspondente ao modelo de regressão linear simples e o
#respetivo diagrama de dispersão

plot(data$hr_results, data$altitude_results, xlab = "hr_results",
 ylab = "Altitude_results", main = "Simple Linear Regression")
abline(model, col = "red")

# c) Calcule o erro médio absoluto (MAE) e raiz quadrada do erro médio (RMSE) do
# modelo sobre os 30% casos de teste.

install.packages("caret")
library(caret)

set.seed(123)

length(data$altitude_results)
length(data$hr_results)

train_index <- sample(c(TRUE,FALSE), nrow(data), replace = TRUE, prob = c(0.7,0.3))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


model <- lm(altitude_results ~ hr_results, data = train_data)

predictions <- predict(model, newdata = test_data)

mae <- mean(abs(predictions - test_data$altitude_results))
mae #0.0935335

rmse <- sqrt(mean((predictions - test_data$altitude_results)^2))
rmse #0.1139122


#d) Teste se é possível obter resultados melhores utilizando um modelo mais
#complexo

train_index <- sample(c(TRUE,FALSE), nrow(data), replace = TRUE, prob = c(0.7,0.3))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

model <- lm(altitude_results ~ hr_results + vo2_results, data = train_data)
summary(model)


predictions <- predict(model, newdata = test_data)

mae <- mean(abs(predictions - test_data$altitude_results))
mae #0.0901299

rmse <- sqrt(mean((predictions - test_data$altitude_results)^2))
rmse #0.1099387



model <- lm(altitude_results ~ hr_results + vo2_results + Pro.level, data = train_data)
summary(model)


predictions <- predict(model, newdata = test_data)

mae <- mean(abs(predictions - test_data$altitude_results))
mae #0.08895729

rmse <- sqrt(mean((predictions - test_data$altitude_results)^2))
rmse #0.1088866

#lower MAE and RMSE, so it is better :D




#exercicio 7
#a) first holdout criteria 
sample <- sample(c(TRUE,FALSE),nrow(normalised_data), replace=TRUE, prob=c(0.7,0.3))
normalised_data.train <- normalised_data[sample,]
normalised_data.test <- normalised_data[!sample,]

summary(normalised_data.train$vo2_results)
summary(normalised_data.test$vo2_results)

#regressao multipla
mlr.model<- lm(vo2_results ~ altitude_results + hr_results, data = normalised_data.train)

par(mfrow=c(2,2))
plot(mlr.model)

View(summary(mlr.model)$coef)



#arvore de regressao
library(rpart)
library(rpart.plot)

tree.model <- rpart(vo2_results ~., method = "anova", data = normalised_data.train)

rpart.plot(tree.model)

#rede neuronal
install.packages("neuralnet")
library(neuralnet)

numnodes <- 1
nn.model <- neuralnet(vo2_results ~ altitude_results + hr_results, data = normalised_data.train, hidden = numnodes,linear.output = TRUE)

plot(nn.model)
nn.model$result.matrix

#mudar parâmetros para 1 nivel interno, 3 nos
numnodes <- 3

nn.model.i <-
  neuralnet(vo2_results ~ altitude_results + hr_results, data = normalised_data.train, hidden = numnodes,linear.output = TRUE)
plot(nn.model.i)

nn.model.i$result.matrix

#mudar parâmetros para 2 niveis iternos com 6 e 2 nos respetivamente

numnodes <- c(6,2)

nn.model.ii <-
  neuralnet(vo2_results ~ altitude_results + hr_results, data = normalised_data.train, hidden = numnodes,linear.output = TRUE)
plot(nn.model.ii)

nn.model.ii$result.matrix

#---------------------------------------
#exercicio 8
mlr.pred <- predict(mlr.model, normalised_data.test)
tree.pred <- predict(tree.model, normalised_data.test)
nn.pred <- predict(nn.model, normalised_data.test)
nn.i.pred <- predict(nn.model.i, normalised_data.test)
nn.ii.pred <- predict(nn.model.ii, normalised_data.test)

# Install and load the 'Metrics' package for MAE and RMSE calculation
if (!require(Metrics)) {
  install.packages("Metrics")
}
library(Metrics)

# Calculate MAE and RMSE for all models
# Create an empty data frame to store the results
results <- data.frame(Model = character(),
                      MAE = numeric(),
                      RMSE = numeric(),
                      stringsAsFactors=FALSE)

# Create a list of models
models <- list("Multiple Linear Regression" = mlr.pred, 
               "Decision Tree" = tree.pred, 
               "Neural Network (1 node)" = nn.pred, 
               "Neural Network (3 nodes)" = nn.i.pred, 
               "Neural Network (6,2 nodes)" = nn.ii.pred)

# Iterate over each model, calculate MAE and RMSE, and add them to the data frame
for (model_name in names(models)) {
  mae <- mae(normalised_data.test$vo2_results, models[[model_name]])
  rmse <- rmse(normalised_data.test$vo2_results, models[[model_name]])
  results <- rbind(results, data.frame(Model = model_name, MAE = round(mae, 2), RMSE = round(rmse, 2)))
}

# View the results
print(results)




#--------------------------------------------------
#Exercicio 9
# Perform a paired t-test
#first between nn models
t_test_result <- t.test(nn.pred, nn.i.pred , paired = TRUE)
print(t_test_result)

t_test_result <- t.test(nn.i.pred, nn.ii.pred , paired = TRUE)
print(t_test_result)

#beetwen mlr and nn.ii.pred
t_test_result <- t.test(mlr.pred, nn.ii.pred , paired = TRUE)
print(t_test_result)

t_test_result <- t.test(nn.ii.pred,tree.model,paired = TRUE)
print(t_test_result)
#best is Multiple Linear Regression - MAE: 0.04 RMSE: 0.05 in this case








