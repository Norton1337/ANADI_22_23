library(rpart)
library(caTools)
#setwd("C:/Users/manu0/Desktop/RESTO/ANADI/TP2")
#data <- read.csv("ciclismo.csv")
setwd("C:/Users/asus/Desktop/ANADI/iteracao_2/anadi_isep_23/TP2/data")
normalised_data <- read.csv("normalised_data.csv")
#1 - Estude a capacidade preditiva relativamente ao atributo “Pro_level” 
#usando os seguintes métodos:

#árvore de decisão;




#3 - Estude a capacidade preditiva relativamente ao atributo “Gender” usando os seguintes métodos:
#holdout
sample <- sample(c(TRUE,FALSE),nrow(normalised_data), replace=TRUE, prob=c(0.7,0.3))
normalised_data.train <- normalised_data[sample,]
normalised_data.test <- normalised_data[!sample,]

#Rede neuronal
formula <- gender ~ altitude_results + hr_results + vo2_results

neuralnet.model <-   neuralnet(formula, data = normalised_data.train, hidden = numnodes,linear.output = TRUE)
plot(neuralnet.model)
predictions <- predict(neuralnet.model, normalised_data.test)


