library(lubridate)
library(corrplot)
library(ggplot2)

#1 - Comece por carregar o ficheiro (“ciclismo.csv”) para o ambiente do R, 
# verifique a sua dimensão e obtenha um sumário dos dados.
#setwd("C:/Users/manu0/Desktop/RESTO/ANADI/TP2/data")
setwd("C:/Users/asus/Desktop/ANADI/iteracao_2/anadi_isep_23/TP2/data")
data <- read.csv("ciclismo.csv")
dimensions <- dim(data) #11 colunas e 1000 linhas
data_summary <- summary(data) #Possui variaveis categorias, númericas e binárias

#2 - Derive um novo atributo Age usando como valor do atributo dob

ages <- interval(as.Date(data$dob), today()) %/% years(1)
data$age <- ages


#3 - Analise os atributos do conjunto de dados mais significativos,
#usando gráficos, análises estatísticas e/ou outros métodos apropriados. 

#Tranformando o atributo categorio binario Gender em binário númerico
# female - 0; male - 1
data$gender<- ifelse(data$gender == "female", 0, 1)

#Repetindo o mesmo processo para a variável Winter.Training.Camp
# completed - 1; none - 0
data$Winter.Training.Camp<- ifelse(data$Winter.Training.Camp == "none", 0, 1)

#Repetindo o mesmo processo para a variável Pro.level
# World Tour - 1; Continental - 0
data$Pro.level <- ifelse(data$Pro.level == "Continental", 0, 1)

#Transformando variáveis categoricas não binárias em numericas
unique_countries <- unique(data$Continent)
data$Continent <- match(data$Continent, unique_countries)

unique_teams <- unique(data$Team)
data$Team <- match(data$Team, unique_teams)

unique_backgrounds <- unique(data$Background)
data$Background <- match(data$Background, unique_backgrounds)

label_names <- c("Gender", "Team", "Background", "Level", "Winter Camp", "Altitude", "VO2", "HR", "Continent", "Age")
boxplot(data[,c(2:9,11,12)], names=label_names, col = c(1,2,3,4,5,6,7,8,10,11))

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


#exercicio 7
#a) first holdout criteria 
sample <- sample(c(TRUE,FALSE),nrow(normalised_data), replace=TRUE, prob=c(0.7,0.3))
normalised_data.train <- normalised_data[sample,]
normalised_data.test <- normalised_data[!sample,]

summary(normalised_data.train$vo2_results)
summary(normalised_data.test$vo2_results)

#regressao multipla
mlr.model<- lm(vo2_results ~ altitude_results + hr_results, data = normalised_data.train)

summary(mlr.model)$coef

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






