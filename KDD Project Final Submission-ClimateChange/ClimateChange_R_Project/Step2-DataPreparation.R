#===============================================================
#DATA PREPARATION
#Combining with temparature data with other attributes
#Creation of New Variable temparature category(LOW,MEDIUM,HIGH)
#Binning using k-means clustering
#Normalization of Numeric Variables
#===============================================================

temperature.ucba <- read.csv("temperature_uscanbra.csv",header = TRUE,sep = ",")
temperature.aens <- read.csv("temperature_egysouth.csv",header = TRUE,sep = ",")
temperature.icmp <- read.csv("temperature_indiachina.csv",header = TRUE,sep = ",")

#retrive mean temperature.ucbas of all countries in to data frames
usaTemp <-c(1960:2012)
brazilTemp <- c(1960:2012)
canadaTemp <- c(1960:2012)
for (i in 1960:2012) {
  usaTemp[i-1959] <- mean(temperature.ucba[temperature.ucba$Country == "USA" & temperature.ucba$X.Year == i,]$Temperature..C.)
  brazilTemp[i-1959] <- mean(temperature.ucba[temperature.ucba$Country == "BRA" & temperature.ucba$X.Year == i,]$Temperature..C.)
  canadaTemp[i-1959] <- mean(temperature.ucba[temperature.ucba$Country == "CAN" & temperature.ucba$X.Year == i,]$Temperature..C.)
}


egyptTemp <- c(1960:2012)
southafricaTemp <- c(1960:2012)
for (i in 1960:2012) {
  egyptTemp[i-1959] <- mean(temperature.aens[temperature.aens$Country == "EGY" & temperature.aens$X.Year == i,]$Temperature..C.)
  southafricaTemp[i-1959] <- mean(temperature.aens[temperature.aens$Country == "ZAF" & temperature.aens$X.Year == i,]$Temperature..C.)
}


chinaTemp <-c(1960:2012)
indiaTemp <- c(1960:2012)

for (i in 1960:2012) {
  chinaTemp[i-1959] <- mean(temperature.icmp[temperature.icmp$Country == "CHN" & temperature.icmp$Year == i,]$Temperature)
  indiaTemp[i-1959] <- mean(temperature.icmp[temperature.icmp$Country == "IND" & temperature.icmp$Year == i,]$Temperature)
  
}


#add temperature column to all the country data
#Also add country name in new column
usaTraining <- cbind(usa[1:53,],usaTemp)
names(usaTraining)[5]<-"temperature"


braTraining <- cbind(brazil[1:53,],brazilTemp)
names(braTraining)[5]<-"temperature"


canTraining<-cbind(canada[1:53,],canadaTemp)
names(canTraining)[5]<-"temperature"

usaTraining$country <- "USA"
braTraining$country <- "BRA"
canTraining$country <- "CAN"

chinaTraining<-cbind(china[1:53,],chinaTemp)
indiaTraining<-cbind(india[1:53,],indiaTemp)


colnames(chinaTraining)[5]<-"temperature"
colnames(indiaTraining)[5]<-"temperature"



# Add country column to the respective datasets
chinaTraining$country<-"CHN"
indiaTraining$country<-"IND"





egypttraining<-cbind(egypt[1:53,],egyptTemp)
egypttraining$country <- "EGY"
names(egypttraining)[5] <- "temperature"





southafricatraining<-cbind(southafrica[1:53,],southafricaTemp)
southafricatraining$country<- "RSA"
names(southafricatraining)[5]<-"temperature"

#================================================
#combine all countries data in to one data frame
#================================================
climateData <- rbind(usaTraining,canTraining,braTraining)
climateData <- rbind(climateData,chinaTraining,indiaTraining)
climateData <- rbind(climateData,egypttraining,southafricatraining)

#==========================================================
#Normalize all the variables by using min-max normalization
#==========================================================

climateData.norm<-climateData

mmnorm.forestarea <- (climateData$forestarea - min(climateData$forestarea))/(max(climateData$forestarea) - min(climateData$forestarea))
climateData.norm$forestarea <- mmnorm.forestarea


mmnorm.co2emission <-(climateData$co2emission - min(climateData$co2emission))/(max(climateData$co2emission) - min(climateData$co2emission))
climateData.norm$co2emission <- mmnorm.co2emission

mmnorm.poptot <-(climateData$poptot - min(climateData$poptot))/(max(climateData$poptot) - min(climateData$poptot))
climateData.norm$poptot <- mmnorm.poptot




#=========================================================================
#Binning using k-means clustering
#create a new variable to hold the category of the record(HIGH,MEDIUM,LOW)
#=========================================================================
library(cluster)
climateclusters <- kmeans(climateData.norm$temperature, centers = 3)
whichbin <- climateclusters$cluster;

bin1<- rownames(climateData.norm[ climateclusters$cluster==1,])
bin2 <- rownames(climateData.norm[ climateclusters$cluster==2,])
bin3 <- rownames(climateData.norm[ climateclusters$cluster==3,])

bin1meantemp <- mean(climateData.norm$temperature[which(rownames(climateData.norm) %in% bin1)])
bin2meantemp <- mean(climateData.norm$temperature[which(rownames(climateData.norm) %in% bin2)])
bin3meantemp <- mean(climateData.norm$temperature[which(rownames(climateData.norm) %in% bin3)])

tempCat <- c("-1", "-1","-1")
maxMean <- max(bin1meantemp,bin2meantemp,bin3meantemp)
minMean <- min(bin1meantemp,bin2meantemp,bin3meantemp)

if(bin1meantemp == maxMean){
  tempCat[1]="HIGH"
}else if(bin1meantemp == minMean){
  tempCat[1]="LOW"}else{
    tempCat[1]="MEDIUM"
  }

if(bin2meantemp == maxMean){
  tempCat[2]="HIGH"
}else if(bin2meantemp == minMean){
  tempCat[2]="LOW"}else{
    tempCat[2]="MEDIUM"
  }

if(bin3meantemp == maxMean){
  tempCat[3]="HIGH"
}else if(bin3meantemp == minMean){
  tempCat[3]="LOW"}else{
    tempCat[3]="MEDIUM"
  }

climateclusters
for(i in 1:nrow(climateData.norm)){
  if(whichbin[i]==1){
    climateData.norm$tempCat[i] <- tempCat[1]
  }
  else if(whichbin[i]==2){
    climateData.norm$tempCat[i] <- tempCat[2]
  }
  else if(whichbin[i]==3){
    climateData.norm$tempCat[i] <- tempCat[3]
  }
}
library(fpc)
plotcluster(climateData.norm$temperature,climateclusters$cluster)
