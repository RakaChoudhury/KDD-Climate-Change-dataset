IntegratedData <- read.csv("C:/Users/Piyusha/Documents/Piyusha/KDD/IntegratedDataNew.csv")
#Dimensions of the dataset
dim(IntegratedData)
#Names of the columns
names(IntegratedData)
#Structure of the dataset
str(IntegratedData)
#display summary
summary(IntegratedData)


#Plotting all parameters by year
boxplot(IntegratedData$avg_Temp~IntegratedData$Year)
boxplot(IntegratedData$CO2~IntegratedData$Year)
boxplot(IntegratedData$TotalPopulation~IntegratedData$Year)
Forestsubset<- subset(IntegratedData, Year >=1990)
boxplot(IntegratedData$X..forest.area~IntegratedData$Year)

#Analyze Correlation
CorData<- Forestsubset[,c(2,3,4,5)]
head(CorData,6)
res <- cor(CorData)
round(res,2)


#Covariance
cov(Forestsubset$Year,Forestsubset$X..forest.area)
#with years forest area has been decreasing
cov(Forestsubset$X..forest.area,Forestsubset$CO2)
#the CO2 emmisions have increased with decrease in forest area.
  