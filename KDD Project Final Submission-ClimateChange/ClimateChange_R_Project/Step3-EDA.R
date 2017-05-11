
#---------------------------------------------------------------------------------------------
#Initially the country which is non numeric value is set to zero before calculating corelation matrix
egypttraining$country<-NULL
cor(egypttraining)
egypttraining$country<-"EGY"

#plotting all the plots for which correlation > |.80|

plot(egypttraining$forestarea,egypttraining$poptot)
plot(egypttraining$forestarea,egypttraining$co2emission)



#-------------------------------------------------------------------------------------------------
#Initially the country which is non numeric value is set to zero before calculating corelation matrix
southafricatraining$country<-NULL
cor(southafricatraining)
southafricatraining$country<-"RSA"

#plotting all the plots for which correlation > |.80|
plot(southafricatraining$forestarea,southafricatraining$co2emission)

-----------------------------------------------------------------------------------------------------
  #Initially the country which is non numeric value is set to zero before calculating corelation matrix
braTraining$country<-NULL
cor(braTraining)
braTraining$country<-"BRA"

plot(braTraining$forestarea,braTraining$poptot)
plot(braTraining$co2emission,braTraining$poptot)
plot(braTraining$forestarea,braTraining$co2emission)

----------------------------------------------------------------------------------------------------
  #Initially the country which is non numeric value is set to zero before calculating corelation matrix
  canTraining$country<-NULL
cor(canTraining)
canTraining$country<-"CAN"

plot(canTraining$forestarea,canTraining$poptot)

#---------------------------------------------------------------------------------------------------
#Initially the country which is non numeric value is set to zero before calculating corelation matrix
usaTraining$country<-NULL
cor(usaTraining)
usaTraining$country<-"USA"



plot(usaTraining$forestarea,usaTraining$poptot)


#-------------------------------------------------------------------------------------------------------
#Initially the country which is non numeric value is set to zero before calculating corelation matrix
indiaTraining$country<-NULL
cor(indiaTraining)
indiaTraining$country<-"IND"

plot(indiaTraining$forestarea,indiaTraining$poptot)
plot(indiaTraining$co2emission,indiaTraining$poptot)
plot(indiaTraining$forestarea,indiaTraining$co2emission)

#-------------------------------------------------------------------------------------------------------------
#Initially the country which is non numeric value is set to zero before calculating corelation matrix
chinaTraining$country<-NULL
cor(chinaTraining)
chinaTraining$country<-"CHN"

plot(chinaTraining$forestarea,chinaTraining$poptot)
plot(chinaTraining$co2emission,chinaTraining$poptot)
plot(chinaTraining$forestarea,chinaTraining$co2emission)



