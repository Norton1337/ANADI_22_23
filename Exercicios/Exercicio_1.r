library(ggplot2)

setwd('C:/Users/manua/ANADI/Trabalho_Prático_1')

#1.a) Acrescente aos dados importados uma coluna com o tempo no sistema POSIXct no formato
#"yy/mm/dd HH:MM:SS GMT". Deve usar as opções origin = "1970-01-01" e tz = "GMT" para
#transformar o tempo (em segundos) para o sistema pedido. 

#Leitura do ficheiro dos dados 1
data_1 <- read.csv("DADOS1.csv")

#Renomeação das colunas
names(data_1)[names(data_1) == "ESP01" | names(data_1) == "ESP02" | names(data_1) == "ESP03"] <- "DISCHARGE_PRESSURE"
names(data_1)[names(data_1) == "ESP01.1" | names(data_1) == "ESP02.1" | names(data_1) == "ESP03.1"] <- "INTAKE_PRESSURE"
names(data_1)[names(data_1) == "ESP01.2" | names(data_1) == "ESP02.2" | names(data_1) == "ESP03.2"] <- "INTAKE_TEMPERATURE"
names(data_1)[names(data_1) == "ESP01.3" | names(data_1) == "ESP02.3" | names(data_1) == "ESP03.3"] <- "MOTOR_TEMP"
names(data_1)[names(data_1) == "ESP01.4" | names(data_1) == "ESP02.4" | names(data_1) == "ESP03.4"] <- "VSDFREQOUT"
names(data_1)[names(data_1) == "ESP01.5" | names(data_1) == "ESP02.5" | names(data_1) == "ESP03.5"] <- "VSDMOTAMPS"

names(data_1)[names(data_1) == "IC01" | names(data_1) == "IC02"] <- "CHOKE_POSITION"
names(data_1)[names(data_1) == "IC01.1" | names(data_1) == "IC02.1"] <- "PRESSURE1"
names(data_1)[names(data_1) == "IC01.2" | names(data_1) == "IC02.2"] <- "PRESSURE2"
names(data_1)[names(data_1) == "IC01.3" | names(data_1) == "IC02.3"] <- "TEMPERATURE1"
names(data_1)[names(data_1) == "IC01.4" | names(data_1) == "IC02.4"] <- "TEMPERATURE2"
names(data_1)[names(data_1) == "IC01.5" | names(data_1) == "IC02.5"] <- "WATER_CUT"
names(data_1)[names(data_1) == "IC01.6" | names(data_1) == "IC02.6"] <- "LIQUID_RATE"
names(data_1)[names(data_1) == "IC01.7" | names(data_1) == "IC02.7"] <- "WATER_RATE"
names(data_1)[names(data_1) == "IC01.8" | names(data_1) == "IC02.8"] <- "OIL_RATE"


#Extração dos dados relativas a cada uma das bombas 
esp01_data<- data_1[3:nrow(data_1),c(2:7,20:28)]
esp02_data<- data_1[3:nrow(data_1),c(8:13,29:37)]
esp03_data<- data_1[3:nrow(data_1),14:19]

#Extração da coluna do tempo e tranformação para a data desejada no alínea a)
column_time<-data_1$X[3:nrow(data_1)]
column_time_aux<-as.POSIXct(as.numeric(column_time), origin = "1970-01-01", tz = "GMT")
#column_time_aux<- format(column_time_aux, "%Y/%m/%d %H:%M:%S %Z")

#Adição da coluna do tempo e do tempo tranformado as subtabelas das bombas
esp01_data<- cbind(esp01_data, Time = column_time, Date = column_time_aux)
esp02_data<- cbind(esp02_data, Time = column_time, Date = column_time_aux)
esp03_data<- cbind(esp03_data, Time = column_time, Date = column_time_aux)

#Filtrando os dados para o dia 4 de agosto de 2013
eps01 <- subset(esp01_data, Date >= as.POSIXct("2013-08-04 00:00:00", tz = "GMT") & Date <= as.POSIXct("2013-08-04 23:59:59", tz = "GMT"))
eps02 <- subset(esp02_data, Date >= as.POSIXct("2013-08-04 00:00:00", tz = "GMT") & Date <= as.POSIXct("2013-08-04 23:59:59", tz = "GMT"))
eps03 <- subset(esp03_data, Date >= as.POSIXct("2013-08-04 00:00:00", tz = "GMT") & Date <= as.POSIXct("2013-08-04 23:59:59", tz = "GMT"))

#1.b)Efetue um gráfico que permita comparar a temperatura do motor nas bombas 1,2 e 3, no dia 4 de agosto de 2013. 


#Comparação das temperaturas ao longo do dia das 3 bombas
dados <- cbind(eps01[,c("Date", "MOTOR_TEMP")],eps02[,c("Date", "MOTOR_TEMP")]$MOTOR_TEMP, eps03[,c("Date", "MOTOR_TEMP")]$MOTOR_TEMP)
colnames(dados) <- c("hora", "bomba1", "bomba2", "bomba3")
plot(as.POSIXct(dados$hora), as.numeric(dados$bomba1), type = "l", col = "red", xlab = "Hora do Dia", ylab = "Temperatura", main = "Comparação da Temperatura do motor entre Bombas", ylim = c(min(as.numeric(dados$bomba2)), max(as.numeric(dados$bomba3))))
lines(as.POSIXct(dados$hora), as.numeric(dados$bomba2), col = "blue")
lines(as.POSIXct(dados$hora), as.numeric(dados$bomba3), col = "green")
legend("topright", legend = c("Bomba 1", "Bomba 2", "Bomba 3"), col = c("red", "blue", "green"), lty = 1)

#Para uma melhor vizualização temos os gráfico separados
plot(eps01$Date, eps01$MOTOR_TEMP, type = "l", xlab = "Horas", ylab = "Temperatura", main = "Gráfico da variação da temperatura do motor ao longo do dia da bomba EPS01", col="red")
plot(eps02$Date, eps02$MOTOR_TEMP, type = "l", xlab = "Horas", ylab = "Temperatura", main = "Gráfico da variação da temperatura do motor ao longo do dia da bomba EPS02", col="#0000ff")
plot(eps03$Date, eps03$MOTOR_TEMP, type = "l", xlab = "Horas", ylab = "Temperatura", main = "Gráfico da variação da temperatura do motor ao longo do dia da bomba EPS03", col="green")


#1.c) - Efetue um boxplot com os dados da alínea anterior. Comente os resultados obtidos. 

#BoxPlot com as três bombas (a escala fica prejudicada)
boxplot(as.numeric(eps01$MOTOR_TEMP), as.numeric(eps02$MOTOR_TEMP), as.numeric(eps03$MOTOR_TEMP), names= c("Bomba 1", "Bomba 2", "Bomba 3"), col=c(2,3,4), main="Boxplot das bombas EPS01, EPS02 e ESP03")

#Para uma melhor visualização temos os gráficos separados
boxplot(as.numeric(eps01$MOTOR_TEMP),col=2, main="Boxplot da temperatura do motor bomba EPS01")
boxplot(as.numeric(eps02$MOTOR_TEMP),col=3, main="Boxplot da temperatura do motor bomba EPS02")
boxplot(as.numeric(eps03$MOTOR_TEMP),col=4, main="Boxplot da temperatura do motor bomba EPS03")



#1.d) - Uma forma de avaliar a quantidade de barris produzida num dia é calcular a média das medições do “oil rate” efetuadas no dia em questão: 

#i. Efetue um gráfico de barras que compare os barris de petróleo produzidos diariamente pelas bombas 1 e 2 no mês de março de 2014. 

barris_1 <- subset(esp01_data, Date >= as.POSIXct("2014-03-01 00:00:00", tz = "GMT") & Date <= as.POSIXct("2014-03-31 23:59:59", tz = "GMT"))
barris_2 <- subset(esp02_data, Date >= as.POSIXct("2014-03-01 00:00:00", tz = "GMT") & Date <= as.POSIXct("2014-03-31 23:59:59", tz = "GMT"))

count_barrils <- function(dataset,days) {
  intervale<-(24*60)/5
  intervale_days<-days
  init<- 1
  vector<- numeric(days)
  for (i in 1:intervale_days) {
    end<- i*intervale
    vector[i] <- as.integer(mean(as.numeric(dataset[init:end,15])))
    init<-(end+1)
  }

  return(vector)
}

dates <- seq(as.Date("2013-03-01"), as.Date("2013-03-31"), by = "day")
result_1<-count_barrils(barris_1,31)
result_2<-count_barrils(barris_2,31)


data_frame_bombas <- data.frame(Data = dates,
                             ESP01 = result_1,
                             ESP02 = result_2)

ggplot(data_frame_bombas, aes(x = Data)) +
  geom_bar(aes(y = ESP01, fill = "Bomba ESP01"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = ESP02, fill = "Bomba ESP02"), stat = "identity", position = "dodge") +
  labs(title = "Produção de óleo por Bomba", y = "Quantidade de barris produzidos", fill = "") +
  scale_fill_manual(values = c("Bomba ESP01" = "#B19CD9", "Bomba ESP02" = "#800080"))

#ii. Em que mês a bomba 1 extraiu mais barris de petróleo?
#N.B. Considere os meses compreendidos entre os dias 1-6-2013 e 31-5-2014. 

barris_1_1 <- subset(esp01_data, Date >= as.POSIXct("2013-06-01 00:00:00", tz = "GMT") & Date <= as.POSIXct("2014-05-31 23:59:59", tz = "GMT"))

dados_mensais_bomba1 <- aggregate(as.numeric(barris_1_1$OIL_RATE) ~ format(as.Date(data), "%Y-%m"), data = barris_1_1$Date, mean)
colnames(dados_mensais_bomba1) <- c("mes", "media_barris")

cores <- rep("#B2DFB2", length(dados_mensais_bomba1[,1]))
mes_max_index <- which(dados_mensais_bomba1$mes == dados_mensais_bomba1$mes[which.max(dados_mensais_bomba1$media_barris)])
cores[mes_max_index]<- "#254117"

barplot(dados_mensais_bomba1$media_barris,  col = cores, names.arg = dados_mensais_bomba1$mes, xlab = "Meses", ylab = "Produção de barris de óleo", main = "Produção mensal da bomba EPS01")

#iii. Extraiu-se uma amostra aleatória de dias entre os dias 1-6-2013 e 31-5-2014 usando as seguintes instruções: 
set.seed(300)
random_number<-sample(1:365,10)

#Calcule a produção diária, nos dias da amostra aleatória, para as bombas 1 e 2 e efetue um
#boxplot dos dados obtidos para a bomba 1 e para a bomba 2. 

number_days<- length(dates <- seq(as.Date("2013-06-01"), as.Date("2014-05-31"), by = "day"))

barris_2_1 <- subset(esp02_data, Date >= as.POSIXct("2013-06-01 00:00:00", tz = "GMT") & Date <= as.POSIXct("2014-05-31 23:59:59", tz = "GMT"))

bomba1<-count_barrils(barris_1_1,number_days)
bomba2<- count_barrils(barris_2_1,number_days)

my_sample <- function(numbers,bomb1,bomb2) {
  vector1<-numeric(10)
  vector2<-numeric(10)
  count=1
  for(i in numbers){
    vector1[count]<-bomb1[i]
    vector2[count]<-bomb2[i]
    count=count+1
  }
  
  return(data.frame(Data = numbers, ESP01 = vector1,ESP02 = vector2))
}


result<-my_sample(random_number,bomba1,bomba2)

boxplot(result$ESP01, result$ESP02,names= c("Bomba1", "Bomba 2"), col=c(2,3), main="Produção Diária de barris da amostra aleatória")



#iv. Utilize as amostras aleatórias da alínea anterior para efetuar um teste de hipóteses que
#permita verificar se a média da produção diária de petróleo da bomba 1 foi superior à da
#bomba 2 no período de 1-6-2013 e 31-5-2014.

#As amostras são independentes e as suas variâncias não são iguais 
variancia_EPS01<- var(as.numeric(barris_1_1$OIL_RATE))
variancia_EPS02<- var(as.numeric(barris_2_1$OIL_RATE))

variancia_EPS01 != variancia_EPS02
#Teste unilateral a direita 
# H0: μ_bomba1 <= μ_bomba2
# H1: μ_bomba1 > μ_bomba2

# Vamos assumir α=0.05

alfa <- 0.05
resTest <-t.test (result$ESP01, result$ESP02, alternative ="greater") 
p_value <-resTest$p.value
#Como o p-value é menor do que o nível de significancia, rejeitamos H0, ou seja aceitamos H1, dessa forma
#podemos concluir que com um nível de siginificância de 5%, a média de produção de barris de óleo da bomba 1 é maior que a média de produção da bomba 2.
p_value < alfa


#v. Confirme se a decisão obtida no teste da alínea anterior corresponde à “realidade”. 


#Calculo da média no período de 1-6-2013 e 31-5-2014 da produção das duas bombas
mean_bomba1<- mean(bomba1) 
mean_bomba2<- mean(bomba2)

#No período de 1-6-2013 e 31-5-2014 verifica-se que a bomba 2 produz menos que a bomba 1 
mean_bomba1 > mean_bomba2


#Calculo da média anual de produção das duas bombas 
number_days<- length(dates <- seq(as.Date("2013-06-01"), as.Date("2014-06-12"), by = "day"))

sample1<-count_barrils(esp01_data,number_days)
sample2<-count_barrils(esp02_data,number_days)

#média anual da produção de barris na bomba 1 e na bomba 2
mean_sample1 <- mean(sample1)
mean_sample2 <- mean(sample2)

#Anualmente também se verifica que a bomba 2 produz menos que a bomba 1 
mean_sample1 > mean_sample2