library(readr)
climate2 <- read_csv("D:/R workspace/kdd project/GlobalLandTemperatures/GlobalLandTemperaturesByMajorCity.csv",na.strings=" ",col_types = cols(dt = col_date(format = "%Y-%m-%d")))
climate2
GlobalLandTemperaturesByMajorCity <- read_csv("D:/R workspace/kdd project/GlobalLandTemperatures/GlobalLandTemperaturesByMajorCity.csv",col_types = cols(dt = col_date(format = "%Y-%m-%d")))
GlobalLandTemperaturesByMajorCity<-na.omit(GlobalLandTemperaturesByMajorCity)
MajorCityfilter<-subset(GlobalLandTemperaturesByMajorCity,dt > "1899-12-31" & dt <"2013-09-01")

sapply(MajorCityfilter, function(x) sum(is.na(x)))

summary(MajorCityfilter)

library(tidyr)
MajorCityfilter %>% separate(col=dt,into=c("Year","Month","day"),convert=TRUE) -> Meantemp
print(Meantemp)

tapply(Meantemp$Year , Meantemp$AverageTemperature, mean)
r1<-with(dat, tapply(Meantemp$Year , Meantemp$AverageTemperature, mean))
r1
hist(tapply(Meantemp$Year , Meantemp$AverageTemperature, mean))

mean(Meantemp$AverageTemperature[Meantemp$Year == "1900"])

mean(Meantemp$AverageTemperature[Meantemp$Year == "1950"])

mean(Meantemp$AverageTemperature[Meantemp$Year == "2000"])

mean(Meantemp$AverageTemperature[Meantemp$Year == "2010"])

mean(Meantemp$AverageTemperature[Meantemp$Year == "2013"])

hist(MajorCityfilter$AverageTemperature, breaks=11,xlim=c(-30,50), main="histogram of temp",xlab="temp",ylab="Counts")

plot(MajorCityfilter$dt,MajorCityfilter$AverageTemperature, main="Scatterplot Example",xlab="temp ", ylab="count ", dt ="1899-12-31")

GlobalLandTemperaturesByMajorCity <- read_csv("D:/R workspace/kdd project/GlobalLandTemperatures/GlobalLandTemperaturesByMajorCity.csv",col_types = cols(dt = col_date(format = "%Y-%m-%d")))
#check for % of missing data using a simple function
pMiss <- function(x){sum(is.na(x))/length(x)*100}
#shows columns and percent missing
apply(GlobalLandTemperaturesByMajorCity,2,pMiss)
#row wise
apply(GlobalLandTemperaturesByMajorCity,1,pMiss)
#
library(mice)
md.pattern(GlobalLandTemperaturesByMajorCity)

library(moments)
skewness(MajorCity$AverageTemperature)
plot(skewness(MajorCity$AverageTemperature))

library(e1071) 
kurtosis(MajorCity$AverageTemperature)

hist(MajorCity$AverageTemperature, breaks=11,xlim=c(-30,50), main="Histogram of Average Temperature",xlab="Temperature(in deg.C)",ylab="Counts")

dim(GlobalLandTemperaturesByMajorCity)
library(lattice)
stripplot(MajorCity$AverageTemperature,pch=20,cex=1.2)



World Population by Year :
World : http://www.worldometers.info/world-population/world-population-by-year/
Country-wise : 1960 vs 2015 http://data.worldbank.org/indicator/SP.POP.TOTL


Data preparation:
Removed missing values.
Year wise Avg temp
Total population in a year
library(readxl)
IntegratedData <- read_excel("C:/Users/Piyusha/Downloads/IntegratedData.xlsx")
View(IntegratedData)
dim(IntegratedData)

#Checking for outliers in the merged dataset
boxplot(IntegratedData$avg_Temp)
boxplot(IntegratedData$TotalPopulation)
#there are no outliers

#Checking for skweness of merged dataset
skewness(IntegratedData$avg_Temp)
plot(skewness(IntegratedData$avg_Temp))
#Positive skewness indicates that the mean of the 
#data values is larger than the median, and the data distribution is right-skewed.

hist(IntegratedData$avg_Temp, main = "Histogram for Average temperature of merged dataset",xlab="Temperature",ylab="Counts")

hist(IntegratedData$TotalPopulation, main = "Histogram for Average temperature of merged dataset",xlab="Total population",ylab="Counts")

Population<- IntegratedData$TotalPopulation
Year<- IntegratedData$Year

matplot(Population,Year,type="p")
#we can clearly see the gradual increase in population over the year

Temperature <- IntegratedData$avg_Temp
Year<- IntegratedData$Year
matplot(Temperature,Year,type="p")
#The temperature looks to be increasing with a slight variations

##all the above results show that the integrated data shows all the results concluded as per earlier analysis.

#Export the integrated dataset to excel .xlsx file
library(xlsx)
write.xlsx(total, "D:/R workspace/kdd project/GlobalLandTemperatures/IntegratedData.xlsx")

#compute the correlation value between Avg_Temp and TotalPopulation
cor(total$avg_Temp,total$TotalPopulation)

#compute the avg_temperature by month
GlobalLandTemperaturesByMajorCity <- read_csv("GlobalLandTemperaturesByMajorCity.csv",col_types = cols(dt = col_date(format = "%Y-%m-%d")))
GlobalLandTemperaturesByMajorCityByMoth<-subset(GlobalLandTemperaturesByMajorCity,dt> "1990-12-01" & dt < "2010-12-01")

install.packages("magrittr")
install.packages("data.table")
install.packages("tidyr")
install.packages("dplyr")
library(magrittr)
library(data.table)
library(tidyr)
library(dplyr)

MeanTemperatureByMonth = GlobalLandTemperaturesByMajorCityByMoth%>%separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE)%>%group_by(Year, Month)%>%summarise(avg_Temp = mean(AverageTemperature)) #summarise the mean of the temperature as avg_Temp
#omit the lines with missing value

MeanTemperatureByMonth<-na.omit(MeanTemperatureByMonth)
MeanTemperatureByMonth <- MeanTemperatureByMonth[,3]

summary(MeanTemperatureByMonth)
dim(MeanTemperatureByMonth)
View(MeanTemperatureByMonth)

GlobalLandTemperaturesByMajorCity <- read_csv("D:/R workspace/kdd project/GlobalLandTemperatures/GlobalLandTemperaturesByMajorCity.csv",col_types = cols(dt = col_date(format = "%Y-%m-%d")))
sapply(GlobalLandTemperaturesByMajorCity , function(x) sum(is.na(x)))
MajorCity <- subset(GlobalLandTemperaturesByMajorCity, dt> "1900-12-01" & dt < "2013-09-01")
sapply(MajorCity, function(x) sum(is.na(x)))
MajorCity<-na.omit(MajorCity)

IntegratedSet<- read_csv("D:/R workspace/kdd project/GlobalLandTemperatures/IntegratedData.csv")
IntegratedSet <-(IntegratedSet[-1])
#K-means clustering with k=4
k.means.fit <- kmeans(IntegratedSet,3)
attributes(k.means.fit)
k.means.fit$centers
k.means.fit$cluster
k.means.fit$size

library(cluster)
clusplot(IntegratedSet, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,plotchar=TRUE,
         labels=1, lines=0)


#Get suitable value of k for k-means algorithm 
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(MeanTemperatureByMonth, nc=5)

#Hierarchical clustering
d <- dist(MeanTemperatureByMonth, method = "euclidean") # Euclidean distance matrix.
H.fit <- hclust(d, method="ward")
plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=2) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(H.fit, k=2, border="red")
