rm(list=ls()) 

library(stats)
library(tidyverse) 

#importando os dados

data = read.csv('C:\\Users\\mauri\\Desktop\\dataset\\bike_buyers.csv', stringsAsFactors = T)


#overview dos dados
str(data)
summary(data)
class(data)
colSums(is.na(data)) #não tem NA
head(data)

#O campo ID pode ser tirado da análise pois não será utilizado, para tal utilzamos o tidyverse

data = select(data, - ï..ID)
summary(data)

#agora vamos fazer uma analise univariada para verificar como cada feature se comporta.
#variáveis discretas e variáveis contínuas
#discreta: Possui um limitado conjunto de valores, exemplo: Occupation
#continuo: possui infinito valores no conjunto de dados: exemplo: Income


#Para o EDA (analise exploratoria dos dados) vamos vereificar duas coisas: 
#tendência central - qual os valores mais comuns? onde os dados estão centrados?

#spread: como as dados estão espalhados?

#Tipos de gráficso apra usar
#boxplot
#histogram
#density plot
#pie graph

#Income - Discreto ou contínuo? - contínuo
summary(data$Income)
boxplot(data$Income)

#spread
hist(data$Income)
plot(density(data$Income))
