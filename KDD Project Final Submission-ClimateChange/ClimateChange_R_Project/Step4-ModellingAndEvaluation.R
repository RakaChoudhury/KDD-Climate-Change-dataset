#=====================================================================
#MODELLING AND EVALUATION
#Devide the climate data in to training and testing datasets
#Develop classification models on the training dataset : CART and C5.0
#Apply the model on test data and find the temparature categories 
#for all records
#Evaluate the categorization done by clustering with results from
#classification using confusion matrix
#=====================================================================
#splitting thre data set into training and test data sets
install.packages("rpart","rpart.plot")
library(rpart)
library(rpart.plot)
#set.seed(12345)
#climateData.norm<- ClimateDataNormalised
data_rand <- climateData.norm[order(runif(371)), ] #creating  a new dataframe 
# where rows are copies of the original dataframe but selected on a random generation of 1000 numbers

summary(climateData.norm$temperature) 
summary(data_rand$temperature) # we check that we get the same data in both dataframes...

head(climateData.norm$temperature)
head(data_rand$temperature) # we check the order of both dataframes are different !

#splitting the dataset
data_train <- data_rand[1:333, ]
data_test  <- data_rand[334:371, ]
# check the data sets  
prop.table(table(data_train$tempCat))
prop.table(table(data_test$tempCat))

# Modeling with CART 
data_model_cart<-rpart(tempCat ~ forestarea+co2emission+poptot+temperature,
                       method="class", data = data_train)

printcp(data_model_cart) # display the results 
plotcp(data_model_cart) # visualize cross-validation results 
summary(data_model_cart) # detailed summary of splits

# plot tree 
plot(data_model_cart, uniform=TRUE, 
     main="Classification Tree for Climate Data")
text(data_model_cart, use.n=TRUE, all=TRUE, cex=.8)

rpart.plot(data_model_cart)
#Testing the model using test data set
data_predict_cart <- predict(data_model_cart,data_test)
# Using C5.0 for modelling
#Training the model
x<-data_train[,c(2,3,4,5)]
y<-data_train$tempCat
y<-as.factor(y)
library(C50)
data_model_c50 <- C5.0(x,y)
data_model_c50
summary(data_model_c50)
# From the summary of the summary of the C50 model we can see that temparture doesnot 
#have any contribution variable to how other factors also influence the temperature
data_model_c50_01<- C5.0(data_train[,c(2,3,4)],as.factor(data_train$tempCat))
summary(data_model_c50_01)

# Testing the model using Test data set
data_predict_c50 <- predict(data_model_c50,data_test)
# Validating the accuracy of the model using chi-square test for propotions
install.packages("gmodels")
library(gmodels)
CrossTable(data_test$tempCat, data_predict_c50,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual ', 'predicted '))

data_predict_c50_01<- predict(data_model_c50_01,data_test)
CrossTable(data_test$tempCat, data_predict_c50_01,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual ', 'predicted '))

