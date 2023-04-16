#Exercicio 2 iteração 1

# Carregar os dados a partir do arquivo DADOS2.csv

#como os dados do csv está rodeado de aspas, instalamos este package para as


# Read in the CSV file as a character vector
csv_lines <- readLines("/Users/asus/Desktop/ANADI/iteracao_1/DADOS2.csv")

# Remove the quotation marks from the column names
csv_lines[1] <- gsub('"', '', csv_lines[1])

# Remove the quotation marks from the data values
for (i in 2:length(csv_lines)) {
  csv_lines[i] <- gsub('"', '', csv_lines[i])
}

# Write the cleaned lines to a new CSV file
writeLines(csv_lines, "/Users/asus/Desktop/ANADI/iteracao_1/new_Dados2.csv")

#Read data from dados2 CSV
data <- read.csv("/Users/asus/Desktop/ANADI/iteracao_1/new_Dados2.csv", sep = ",")

#extract only data from algorithms
algorithms_data <- data[,3:7]

#produce correlations matrix
corr_matrix <- cor(algorithms_data)
print(corr_matrix)

#To verify significative diferences between algortihms, we can use an ANOVA test

#to setup anova we need libraries to convert data to be readable for anova test
library(tidyr)
library(dplyr)

#Transform data into long format
long_data<- data %>%
  pivot_longer(cols = SVM:GB,names_to = "algorithm",values_to = "precision" )

#ANOVA test 
 model <- aov(precision ~ algorithm ,data = long_data)
 summary(model)

 #To test multiple comparisons we use TukeyHSD 
 TukeyHSD(model)
 
 
 