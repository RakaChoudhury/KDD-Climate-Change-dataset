#====================================================================================================
#DATA UNDERSTANDING
#This includes understanding the structure of the data and imputing the missing vaules in the data.
#The data related to each country is present in different files
#Data imputation is performed on each country separately as it gives best prdictions
#====================================================================================================

#USA DATA
usa.initial <- read.csv("usa.csv",header = TRUE,sep = ",")
names(usa.initial)<-c("year","forestarea","co2emission","poptot")
usa <- usa.initial

#structure of data is same for all the data sets
str(usa)

#variables present are 
names(usa)

#Summary can be observed as
summary(usa)

#=============================================
#IMPUTE MISSING VALUES FOR ALL DATASET
#=============================================
#USA DATA
#=============================================

#impute forest data from 1:30 
usa.mreg.out2 <- lm(usa$forestarea ~usa$co2emission+usa$poptot)
summary(usa.mreg.out2)


usa.mreg.int2 <- predict(usa.mreg.out2,
                         data.frame(usa$co2emission,usa$poptot),
                         interval = "confidence");
usa$forestarea[1:30]<-usa.mreg.int2[1:30,1]

#impute co2emission in 53:56
usa.mreg.out4 <- lm(usa$co2emission ~ usa$forestarea+usa$poptot)
summary(usa.mreg.out4)


usa.mreg.int4 <- predict(usa.mreg.out4,
                         data.frame(usa$forestarea,usa$poptot),
                         interval = "confidence");
usa$co2emission[c(53:56)]<-usa.mreg.int4[c(53:56),1]


#=============================================
#BRAZIL DATA
#=============================================

brazil.initial <-read.csv("brazil.csv",header = TRUE,sep = ",")
names(brazil.initial)<-c("year","forestarea","co2emission","poptot")
brazil <- brazil.initial

#impute co2emission in 53:56
brazil.reg.out5 <- lm(brazil$co2emission ~brazil$forestarea+brazil$poptot)
summary(brazil.reg.out5)


brazil.reg.int5 <- predict(brazil.reg.out5,
                           data.frame(brazil$forestarea,brazil$poptot),
                           interval = "confidence");
brazil$co2emission[c(53:56)]<-brazil.reg.int5[c(53:56),1]


#impute forest data from 1:30 
brazil.reg.out2 <- lm(brazil$forestarea ~brazil$co2emission+brazil$poptot)
summary(brazil.reg.out2)


brazil.reg.int2 <- predict(brazil.reg.out2,
                           data.frame(brazil$co2emission,brazil$poptot),
                           interval = "confidence");
brazil$forestarea[1:30]<-brazil.reg.int2[1:30,1]

#=============================================
#CANADA DATA
#=============================================
canada.initial<-read.csv("canada.csv",header = TRUE,sep = ",")
names(canada.initial)<-c("year","forestarea","co2emission","poptot")
canada <- canada.initial

#impute forest data from 1:30 
canada.mreg.out2 <- lm(canada$forestarea ~canada$co2emission+canada$poptot)
summary(canada.mreg.out2)


canada.mreg.int2 <- predict(canada.mreg.out2,
                            data.frame(canada$co2emission,canada$poptot),
                            interval = "confidence");
canada$forestarea[1:30]<-canada.mreg.int2[1:30,1]

#impute co2emission in 53:56
canada.mreg.out4 <- lm(canada$co2emission ~canada$forestarea+canada$poptot)
summary(canada.mreg.out4)


canada.mreg.int4 <- predict(canada.mreg.out4,
                            data.frame(canada$forestarea,canada$poptot),
                            interval = "confidence");
canada$co2emission[c(53:56)]<-canada.mreg.int4[c(53:56),1]


#=============================================
#INDIA DATA
#=============================================
# Imputation for missing values in India Dataset
india.initial<- read.csv("india.csv",header= TRUE,sep = ",")
india<-india.initial[-56,]

#missing value of forest area
india.mreg.out2<- lm(india$forestarea~india$co2emission
                     +india$poptot)

india.mreg.int2 <- predict(india.mreg.out2,
                           data.frame(india$co2emission,
                                      india$poptot),
                           interval = "confidence");

india$forestarea[2:30]<-india.mreg.int2[2:30,1]

#missing for co2 emission
india.mreg.out4<- lm(india$co2emission~india$forestarea+india$poptot)


india.mreg.int4 <- predict(india.mreg.out4,
                           data.frame(india$forestarea,india$poptot),
                           interval = "confidence");
india$co2emission[53:55]<-india.mreg.int4[53:55,1]

#missing values for forest area again 
india.mreg.out5<- lm(india$forestarea~india$co2emission
                     +india$poptot)


india.mreg.int5 <- predict(india.mreg.out5,
                           data.frame(india$forestarea,india$co2emission,
                                      india$poptot),
                           interval = "confidence");
india$forestarea[1]<-india.mreg.int5[1,1]


#=============================================
#CHINA DATA
#=============================================
china.initial<- read.csv("china.csv",header= TRUE,sep = ",")
china<-china.initial[-56,]

#missing value of forest area
chn.mreg.out2<- lm(china$forestarea~china$co2emission
                   +china$poptot)

chn.mreg.int2 <- predict(chn.mreg.out2,
                         data.frame(china$co2emission,
                                    china$poptot),
                         interval = "confidence");

china$forestarea[2:30]<-chn.mreg.int2[2:30,1]

#missing for co2 emission
chn.mreg.out4<- lm(china$co2emission~china$forestarea+china$poptot)


chn.mreg.int4 <- predict(chn.mreg.out4,
                         data.frame(china$forestarea,china$poptot),
                         interval = "confidence");
china$co2emission[53:55]<-chn.mreg.int4[53:55,1]

#missing values for forest area again 
chn.mreg.out5<- lm(china$forestarea~china$co2emission
                   +china$poptot)


chn.mreg.int5 <- predict(chn.mreg.out5,
                         data.frame(china$forestarea,china$co2emission,
                                    china$poptot),
                         interval = "confidence");
china$forestarea[1]<-chn.mreg.int5[1,1]


#=============================================
#South Africa DATA
#=============================================
#=============================================
southafrica.initial<- read.csv("southafrica.csv",header= TRUE,sep = ",")
southafrica<-southafrica.initial


#impute forest data from 1:30 
mreg.out.s4 <- lm(southafrica$forestarea ~southafrica$co2emission+southafrica$poptot)
summary(mreg.out.s4)


mreg.int.s4 <- predict(mreg.out.s4,
                       data.frame(southafrica$co2emission,southafrica$poptot),
                       interval = "confidence");
southafrica$forestarea[1:30]<-mreg.int.s4[1:30,1]


#impute co2emission in 53:56
mreg.out.s6 <- lm(southafrica$co2emission ~southafrica$forestarea+southafrica$poptot)
summary(mreg.out.s6)


mreg.int.s6 <- predict(mreg.out.s6,
                       data.frame(southafrica$forestarea,southafrica$poptot),
                       interval = "confidence");
southafrica$co2emission[c(53:56)]<-mreg.int.s6[c(53:56),1]
#=============================================
#Egypt DATA
#=============================================
egypt.initial<- read.csv("egypt.csv",header= TRUE,sep = ",")
egypt<-egypt.initial

#impute forest data from 1:30 
mreg.out.e4 <- lm(egypt$forestarea ~egypt$co2emission+egypt$poptot)
summary(mreg.out.e4)


mreg.int.e4 <- predict(mreg.out.e4,
                       data.frame(egypt$co2emission,egypt$poptot),
                       interval = "confidence");
egypt$forestarea[1:30]<-mreg.int.e4[1:30,1]


#impute co2emission in 53:56
mreg.out.e6 <- lm(egypt$co2emission ~egypt$forestarea+egypt$poptot)
summary(mreg.out.e6)


mreg.int.e6 <- predict(mreg.out.e6,
                       data.frame(egypt$forestarea,egypt$poptot),
                       interval = "confidence");
egypt$co2emission[c(53:56)]<-mreg.int.e6[c(53:56),1]

 

