install.packages('doSNOW')

library(caret)
library(doSNOW)


train = read.csv('C:\\Users\\mauri\\Desktop\\dataset\\titanic\\train.csv', header = T)
data

###replace mising embkard values with mode

table(train$Embarked)
train$Embarked[train$Embarked == ''] = 'S'

#add a feature for tracking missing ages
summary(train$Age)
177/nrow(train)*100
####aproximadamente 20% dos dados estão como missing values;
#tracking feature
train$MissingAge = ifelse(is.na(train$Age),'Y', 'N')

table(train$Pclass, train$MissingAge)


### add a feature of family size. Having the sum of family size may be a stronger feature thn having separeted ones.
train$FamilySize = 1 + train$SibSp + train$Parch
head(train)

#transforming data to factor
train$Survived  =as.factor(train$Survived)
train$Parch = as.factor(train$Parch)
train$Sex = as.factor(train$Sex)
train$Embarked = as.factor(train$Embarked)
train$MissingAge = as.factor(train$MissingAge)
train$Pclass = as.factor(train$Pclass)
#Subset data to feature, keep and throw off

features = c('Survived', 'Pclass', "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "MissingAge", 'FamilySize')

train = train[,features]
ncol(train)

summary(train)

#Filling missing data

#first transform all feature to dummy variable
dummy.vars = dummyVars(~., data = train[,-1])
train.dummy = predict(dummy.vars, train[,-1])
View(train.dummy)


#now, impute!
pre.process = preProcess(train.dummy, method = 'bagImpute')
imputed.data = predict(pre.process, train.dummy)
View(imputed.data)

train$Age = imputed.data[,6]

View(train)                 

#####
##machine learning
###
#split data


set.seed(123)
#creating indexes
indexes = createDataPartition(train$Survived, times = 1, p = 0.7, list = F)

#creating train and test split
titanic.train = train[indexes,]
titanic.test = train[-indexes,]


# data still representative 
prop.table(table(train$Survived))
prop.table(table(titanic.train$Survived))
prop.table(table(titanic.test$Survived))
#all the proproportion are kept the same.

#cross validation
train.control = trainControl(method = 'repeatedcv', number = 10, repeats = 3, search = 'grid')

#tune grid
tune.grid = expand.grid(eta = c(0.05,0.075,0.1),
                        nrounds = c(50,75,100),
                        max_depth = 6:8,
                        min_child_weight  =c(2.0,2.25,2.5),
                        colsample_bytree = c(0.3,0.4,0.5),
                        gamma = 0,
                        subsample = 1)

#creating virtual machines for the model
cl = makeCluster(5,type = 'SOCK')
#registering them
registerDoSNOW(cl)

####building the model itself

#rtain the xgboost model using 10fold CV repeated 3 times, and a hyperparameter grid search to train the optimal model.
library(xgboost)
caret.cv = train(Survived ~., 
                 data = titanic.train,
                 method  ='xgbTree',
                 tuneGrid = tune.grid,
                 trControl = train.control)

stopCluster(cl)

preds = predict(caret.cv, titanic.test)


confusion = confusionMatrix(preds,titanic.test$Survived)

acc = sum(diag(confusion$table)/sum(confusion$table)*100)
acc

dddd