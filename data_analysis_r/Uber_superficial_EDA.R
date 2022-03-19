#Q0. import the dataset
data = read.csv('uberdrive.csv', sep = ',', stringsAsFactors = T)

# Q1. Show the last 10 records of the dataset.
tail(data,10)

# Q2. Show the first 10 records of the dataset
head(data,10)

# Q3. Show the dimension of the dataset.
dim(data)

# Q4. Show the size of the dataset.
nrow(data)
ncol(data)

# Q5. Print the information about all the variables of the data set.
str(data)

# Q6. Check for missing values.
colSums(is.na(data))

# Q11. Get the unique start destinations
unique(data$START.)

# Q12. What is the total number of unique start destinations?
length(unique(data$START.))

#   Q13. Print the total number of unique stop destinations
unique(data$STOP.)

# Q14. Print all the Uber trips that has the starting point of San Francisco.
length(which(data$START. == 'San Francisco'))
#8
data2
# Q15. What is the most popular starting point for the Uber drivers?
data2 = data %>% group_by(START.) %>% summarise(n = n()) %>% arrange(-n) %>% slice(1:10)
data2 %>%  ggplot() + geom_col(mapping = aes(x = reorder(Começo, desc(qtd)), y = qtd), fill = 'red')
#   Q16. What is the most popular dropping point for the Uber drivers?
data %>% group_by(START.) %>% summarise(n = n()) %>% arrange(-n) %>% slice(1:10)

#   Q17. List the most frequent route taken by Uber drivers.
#for START route these are the most famous ones

data %>% group_by(START.) %>% summarise(n = n ()) %>% arrange(-n) %>% slice(1:10)  %>%  ggplot() + geom_col(mapping = aes(x = reorder(START., desc(n)), y = n), fill = 'grey', colour = 'black') + labs(x = 'START OF ROUTE', y = 'NÚMERO DE CORRIDAS', title = 'GRÁFICO DA QUANTIDADE DE ROTAS POR DESTINO')


#for end route, these are the most famous ones 
data %>% group_by(STOP.) %>% summarise(n = n ()) %>% arrange(-n)

# Q18. Print all types of purposes for the trip in an array.
unique(data$PURPOSE.)

# Q19. Plot a bar graph of Purposes vs Distance.
head(data)

data %>% group_by(PURPOSE.) %>% summarise(n = n()) %>% arrange(-n) %>% slice(1:10) %>% 
  ggplot() + geom_col(mapping = aes(x= reorder(PURPOSE.,desc(n)), y = n))

ggplot(data) + geom_bar(mapping = aes(x = PURPOSE.))
data
# Q20. Print a dataframe of Purposes and the distance travelled for that particular Purpose.
data %>% group_by(PURPOSE.) %>% summarise(n = n()) %>% arrange(-n) %>% slice(1:10)

# Q21. Plot number of trips vs Category of trips.
ggplot(data) + geom_bar(mapping = aes(x = CATEGORY.))
unique(data$CATEGORY.)
# Q22. What is proportion of trips that is Business and what is the proportion of trips that is Personal?

table(data$CATEGORY.)/sum(table(data$CATEGORY.))*100


           
