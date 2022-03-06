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

#INCOME - Discreto ou contínuo? - contínuo
summary(data$Income)
boxplot(data$Income)

#spread
hist(data$Income)
plot(density(data$Income), main = "Income density spread")

#EDUCATION
summary(data$Education)
plot(data$Education)
data %>% ggplot() + geom_bar(mapping = aes(x = Education), fill = "blue")

#MARITAL STATUS
summary(data$Marital.Status)
plot(data$Marital.Status)
pie(table(data$Marital.Status))

#Change to Factor

data$Children = factor(data$Children)
summary(data$Children)
plot(data$Children)


bought = data %>% filter(Purchased.Bike == "Yes")

plot(bought$Children)

plot(data$Commute.Distance)
head(data)

#Análise multivariada

#CATEGORICA E CONTINUA

by(data$Income, data$Education, summary)
by(data$Income, data$Education, mean)
by(data$Income, data$Education, median)

#plotando ggplot entre education and income.
data %>% ggplot() + geom_boxplot(mapping = aes(x = Education, y = Income, colour = Education))

#plotando density plot entre education e Income
data %>% ggplot() + geom_density(mapping = aes(x = Income, fill = Education))

#Comprou ou não comprou a moto em relação ao nível de educação.
xtabs(~Education+Purchased.Bike, data)
plot(xtabs(~Education+Purchased.Bike, data))

#Comprou ou não comprou a moto em relação a profissão
xtabs(~Occupation+Purchased.Bike, data)
plot(xtabs(~Occupation+Purchased.Bike, data))

install.packages("gmodels")
library(gmodels)

#TABELA DE CHI² PARA VERIFICAR SE O NÍVEL DE EDUCAÇÃO TME OU NÃO RELAÇÃO NA COMPRA DA MOTO
CrossTable(data$Education, data$Purchased.Bike, chisq = T, prop.t = F)

#Se o valor de p<= 0.05 é estatisticamente significante.Neste caso deu 0.00004.. o que significa que o nível de educação é significante em relação a compra da moto ou não.

names(data)
#TABELA DE CHI? PARA VERIFICAR SE A SITUAÇÃO MARITAL TEM INTERFERÊNCIA NA COMPRA DA MOTO
CrossTable(data$Marital.Status, data$Purchased.Bike, chisq = T, prop.t = F)
#Neste caso temos um p valor de 0.005, o que significa que a situação marital tem inteferência na compra ou não de moto.


#Continuos e continuos

scatter.smooth(data$Age, data$Income)

#verifica-se como o income se comporta ao longo das idades.

