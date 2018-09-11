#Team Project 1
#Greg Carnovale

#library(tmap)
library(dplyr)
#load data

madrid <- read.csv(file="E:/STAT647/Homework/Data/madrid_2015.csv")
#rename the column with station id to allow merging
colnames(madrid)[14] <- "id"
#transform to date
madrid$date <- as.Date(madrid$date)
#group by id and date
#madrid_date_id <- by(madrid,madrid[,c(1,14)],mean)
#this requires dplyr
madrid_date_id <- madrid %>% group_by(date, id) %>% summarise(BEN=mean(BEN,na.rm=TRUE),
                                                              CO=mean(CO,na.rm=TRUE),
                                                              EBE=mean(EBE,na.rm=TRUE),
                                                              NMHC=mean(NMHC,na.rm=TRUE),
                                                              NO=mean(NO,na.rm=TRUE),
                                                              NO_2=mean(NO_2,na.rm=TRUE),
                                                              O_3=mean(O_3,na.rm=TRUE),
                                                              PM10=mean(PM10,na.rm=TRUE),
                                                              PM25=mean(PM25,na.rm=TRUE),
                                                              SO_2=mean(SO_2,na.rm=TRUE),
                                                              TCH=mean(TCH,na.rm=TRUE),
                                                              TOL=mean(TOL,na.rm=TRUE))
summary(madrid_date_id)

madrid_june_01 <- madrid_date_id[(madrid_date_id$date==as.Date("2015-06-01")),]

summary(madrid_june_01)
hist(madrid_june_01$PM10)
qqnorm(madrid_june_01$PM10)


stations <- read.csv(file="E:/STAT647/Homework/Data/stations.csv")

#restrict the data to June 01

#summarize data

summary(madrid)
summary(stations)

#this is an expensive operation
plot(madrid[,-c(1,14)])

#make histograms of each
par(mfrow=c(3,4))
for (i in 2:13) {hist(madrid[,i],main=paste("Histogram of",colnames(madrid)[i]))}

#merge the data
madrid_air <- merge(madrid, stations, by="id")

par(mfrow=c(1,1))
plot(madrid_air$lat,madrid_air$lon)

