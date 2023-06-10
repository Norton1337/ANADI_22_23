library(rpart)
library(caTools)
install.packages("tidyverse")
library(tidyverse)
# cross - validation methods
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("gower")
install.packages("hardhat")
library(caret)

setwd("C:/Users/manu0/Desktop/RESTO/ANADI/TP2/data")
#data <- read.csv("ciclismo.csv")
#setwd("C:/Users/asus/Desktop/ANADI/iteracao_2/anadi_isep_23/TP2/data")
normalised_data <- read.csv("normalised_data.csv")

#3 - Estude a capacidade preditiva relativamente ao atributo “Gender” usando os seguintes métodos:
# Convert 'gender' to a factor
normalised_data$gender <- as.factor(normalised_data$gender)

# Set control for cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# Define the formula
formula <- gender ~ altitude_results + hr_results + vo2_results

# Neural Network model
nn <- train(formula, data = normalised_data, method = 'nnet', linout = FALSE, trControl = ctrl)

# K-Nearest Neighbors model
knn <- train(formula, data = normalised_data, method = "knn", trControl = ctrl)

# Extract accuracies from the resampling results
accuracy_nn <- nn$resample$Accuracy
accuracy_knn <- knn$resample$Accuracy

# Calculate mean and standard deviation of the accuracies
mean_acc_nn <- mean(accuracy_nn, na.rm = TRUE)
sd_acc_nn <- sd(accuracy_nn, na.rm = TRUE)

mean_acc_knn <- mean(accuracy_knn, na.rm = TRUE)
sd_acc_knn <- sd(accuracy_knn, na.rm = TRUE)

# Print the average and standard deviation of the accuracy for both models
print(paste("Neural Network - Mean accuracy: ", mean_acc_nn, " SD: ", sd_acc_nn))

print(paste("K-Nearest Neighbors - Mean accuracy: ", mean_acc_knn, " SD: ", sd_acc_knn))


#alinea b)
# Paired t-test
t_test_result <- t.test(accuracy_nn, accuracy_knn, paired = TRUE)

# Print the result
print(t_test_result)


#alinea c
# Predictions on the test data
pred_nn <- predict(nn, newdata = normalised_data.test)
pred_knn <- predict(knn, newdata = normalised_data.test)

# Confusion Matrix for Neural Network
conf_mat_nn <- confusionMatrix(pred_nn, normalised_data.test$gender)

# Confusion Matrix for K-Nearest Neighbors
conf_mat_knn <- confusionMatrix(pred_knn, normalised_data.test$gender)

# Print the confusion matrix and associated statistics
print("Neural Network Performance:")
print(conf_mat_nn)
print("K-Nearest Neighbors Performance:")
print(conf_mat_knn)



