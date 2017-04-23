
#Consolidated R-code for submission

#Import global land temperature dataset.This dataset gives year wise temperatures for major #cities of the world
GlobalLandTemperaturesByMajorCity <- read_csv("D:/R workspace/kdd project/GlobalLandTemperatures/GlobalLandTemperaturesByMajorCity.csv",col_types = cols(dt = col_date(format = "%Y-%m-%d")))

View(GlobalLandTemperaturesByMajorCity)
#check for % of missing data using a simple function
pMiss <- function(x){sum(is.na(x))/length(x)*100}
#shows columns and percent missing
apply(GlobalLandTemperaturesByMajorCity,2,pMiss)
#row wise
apply(GlobalLandTemperaturesByMajorCity,1,pMiss)
#micepackage can be used for imputation of data in R 
library(mice)
#See patterns in missing data
md.pattern(GlobalLandTemperaturesByMajorCity)

#sapply can be a useful function to explore the part of/entire dataset
#and to apply common function to the selected set.
#Check how many values are NA
sapply(GlobalLandTemperaturesByMajorCity, function(x) sum(is.na(x)))
#There are 11002 rows with missing values for average temperature.

#Pattern in missing values : Data before 1900 has maximum missing values.
#Methods like substituting with constant or mean, imputation won't be appropriate.
#They would adversely affect the patterns of climate change observed worldwide due to biased #data values.Hence, removing the missing values will be most suitable.
#Filter data to include data from 1900 to 2013
MajorCity <- subset(GlobalLandTemperaturesByMajorCity, dt> "1900-12-01" & dt < "2013-09-01")
View(MajorCity)
summary(MajorCity)

library(moments)
#Calculate skewness value -  measure of symmetry
skewness(MajorCity$AverageTemperature)
plot(skewness(MajorCity$AverageTemperature))

library(e1071) 
#Calculate kurtosis value 
#check if  data is heavy-tailed or light-tailed compared to a normal distribution
kurtosis(MajorCity$AverageTemperature)

#Histogram for Average Temperature 
hist(MajorCity$AverageTemperature, breaks=11,xlim=c(-30,50), main="Histogram of Average Temperature",xlab="Temperature(in deg.C)",ylab="Counts")

library(dplyr)
#Split data to consider only year value
MajorCity$year <- format(MajorCity$dt, "%Y")
m <- data.frame(MajorCity$year ,MajorCity$AverageTemperature , MajorCity$AverageTemperatureUncertainty)
View(m)
#Aggregate function aggregates the input data by applying a function specified by the FUN #parameter to each column.
#Here,we used it to calculate the year wise average temperature.
#For each given year,values of AverageTemperature for all cities for that year are taken 
#and their mean is calculated.
aggdata <- aggregate(m, list(m$MajorCity.year), FUN = mean)
print(aggdata)

mean(MajorCity$AverageTemperature)
mean.test <- t.test(x=MajorCity$AverageTemperature, mu=19.0129, conf.level= 0.95) 
mean.test$statistic 
mean.test$p.value 
mean.test$conf.int

box <- boxplot(GlobalLandTemperaturesByMajorCity$AverageTemperature, main = "Checking for outliers in temperature")
box$out    #outlier values
names(box)
box$stats    #Shows trend in outliers

#Graph using GlobalTemperatures.csv
#Shows an increase in the average temperature of land over the years 
View(GlobalTemperatures)
GlobalTemperatures$dt<- format(GlobalTemperatures$dt, "%Y-%m-%d")

#Create a subset of the dataset such that dt is after 1752-12-01
g <- subset(GlobalTemperatures, dt> "1752-12-01")
g$year <- format(g$dt, "%Y")

#Plot Average land temperature versus Year
#The plot shows an increase in average land temperature over the years
df <- data.frame(g$year,g$LandAverageTemperature)
View(df)
aggdata <- aggregate(df, list(df$g.year), FUN = mean)
View(aggdata)

####
#Rise in sea level dataset
Sealevel <-read.csv(file = "SeaLevel.txt")
Sealevel
summary(Sealevel)
#draw the line chart for the sea level rise vs year
ggplot(Sealevel, aes(x = Year, y = CSIRO.Adjusted.Sea.Level)) + geom_smooth(model = lm,size = 2) +
  xlab("Years") + ylab("Sealevel") + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 13)) +
  ggtitle("Sealevel Trend") + 
  theme(plot.title = element_text(size = 13, lineheight=.8, face="bold"))
#Graph shows an increase in the sea level over years

####
#Greenhouse gas emissions dataset
greenhousegas<-read.csv(file = "D:/R workspace/kdd project/supporting datasets/GHG(greenhousegas).csv")
greenhousegas
summary(greenhousegas)

gc()
library(readr)
library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)
GlobalLandTemperaturesByMajorCity <- read_csv("GlobalLandTemperaturesByMajorCity.csv",col_types = cols(dt = col_date(format = "%Y-%m-%d")))
#computer the average temperature by year
MeanTemperature = GlobalLandTemperaturesByMajorCity  %>%
  separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) %>%
  group_by(Year) %>%
  summarise(avg_Temp = mean(AverageTemperature)) #summarise the mean of the temperature as avg_Temp
#omit the lines with missing value
MeanTemperature<-na.omit(MeanTemperature)
MeanTemperature
summary(MeanTemperature)

#draw the line chart for the average temperature vs year. use geom_smooth to add a smoothed #conditional mean 
ggplot(MeanTemperature, aes(x = Year, y = avg_Temp)) + geom_smooth(model = lm,size = 2) +
  xlab("Years") + ylab("Average Temperature") + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 13)) +
  ggtitle("Average Temperature Trend") + 
  theme(plot.title = element_text(size = 13, lineheight=.8, face="bold"))
#Shows an increasing graph indicating the occurrence global warming over the years

#make sure the two datasets have the same dimension
MeanTemperatureAfter1960<-MeanTemperature[MeanTemperature$Year>=1960,]
dim(MeanTemperatureAfter1960)
install.packages("readxl")
GlobalPopulationByCountry <- read_excel("D:/R workspace/kdd project/GlobalLandTemperatures/GlobalPopulationByYear.xlsx")
dim(GlobalPopulationByCountry)
summary(GlobalPopulationByCountry)
GlobalPopulationByCountry<-GlobalPopulationByCountry[GlobalPopulationByCountry$Year<=2012,]
dim(GlobalPopulationByCountry)
#merge the datasets by Year
total <- merge(MeanTemperatureAfter1960,GlobalPopulationByCountry, by="Year")
View(total)

#Export the integrated dataset to excel .xlsx file
library(xlsx)
write.xlsx(total, "D:/R workspace/kdd project/GlobalLandTemperatures/IntegratedData.xlsx")

#compute the correlation value between Avg_Temp and TotalPopulation
cor(total$avg_Temp,total$TotalPopulation)

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

gc()
library(readr)
library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(readxl)
library(TSA)
library(forecast)
library(readxl)
GlobalLandTemperaturesByMajorCity <- read_csv("GlobalLandTemperaturesByMajorCity.csv",col_types = cols(dt = col_date(format = "%Y-%m-%d")))
#computer the average temperature by year
MeanTemperature = GlobalLandTemperaturesByMajorCity  %>%
  separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) %>%
  group_by(Year) %>%
  summarise(avg_Temp = mean(AverageTemperature)) #summarise the mean of the temperature as avg_Temp
#omit the lines with missing value
MeanTemperature<-na.omit(MeanTemperature)
MeanTemperature<-MeanTemperature[MeanTemperature$Year>=1889,]
MeanTemperature
summary(MeanTemperature)
dim(MeanTemperature)

ggplot(MeanTemperature, aes(x = Year, y = avg_Temp)) + geom_smooth(model = lm,size = 2) +
  xlab("Years") + ylab("Average Temperature") + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 13)) +
  ggtitle("Average Temperature Trend") + 
  theme(plot.title = element_text(size = 13, lineheight=.8, face="bold")) 

#Applying k-means clustering algorithm with k=3
k.means<-kmeans(MeanTemperature,3)
k.means

#draw the line chart for the average temperature vs year. use geom_smooth to add a smoothed conditional mean 
ggplot(MeanTemperature, aes(x = Year, y = avg_Temp)) + geom_smooth(model = lm,size = 2) +
  xlab("Years") + ylab("Average Temperature") + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 13)) +
  ggtitle("Average Temperature Trend") + 
  theme(plot.title = element_text(size = 13, lineheight=.8, face="bold")) + 
  geom_point(x = 1911.325, y = 18.61164,size = 3, color = "red")+
  annotate("text",x =1909.383 , y = 20,label = "1989-1931")+
  geom_point(x = 1952.000, y = 18.93435,size = 3, color = "red") + 
  annotate("text",x =1948 , y = 20,label = "1932-1963")+
  geom_point(x = 1992.500, y = 19.37075,size = 3, color = "red") + 
  annotate("text",x =1992.000, y = 20,label = "1964-2012")+
  geom_vline(xintercept = 1932,lty=2,size = 1, color = "green")+
  geom_vline(xintercept = 1963,lty=2,size = 1, color = "green")

#Applying time series ARIMA model
GlobalLandTemperaturesByMajorCity <- read_csv("GlobalLandTemperaturesByMajorCity.csv",col_types = cols(dt = col_date(format = "%Y-%m-%d")))
#time series part
GlobalLandTemperaturesByMajorCityByMoth<-subset(GlobalLandTemperaturesByMajorCity,dt> "1989-12-01")
MeanTemperatureByMonth = GlobalLandTemperaturesByMajorCityByMoth  %>%
  separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) %>%
  group_by(Year, Month) %>%
  summarise(avg_Temp = mean(AverageTemperature)) #summarise the mean of the temperature as avg_Temp
#omit the lines with missing value

MeanTemperatureByMonth<-na.omit(MeanTemperatureByMonth)
MeanTemperatureByMonth <- MeanTemperatureByMonth[,3]

summary(MeanTemperatureByMonth)
dim(MeanTemperatureByMonth)


MeanTemperatureByMonth<-ts(MeanTemperatureByMonth,start=c(1990,1), fre=12)
MeanTemperatureByMonth

boxplot(MeanTemperatureByMonth~cycle(MeanTemperatureByMonth))

training<-ts(MeanTemperatureByMonth,start=c(1990,1),end=c(2010,12), fre=12)
training
test<-ts(MeanTemperatureByMonth,start=c(2011,1),end=c(2013,8), fre=12)
test
plot(training,ylab='MeanTemperatureByMonth')
