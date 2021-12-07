# Assignment #2 Meter Data Formatting

#install "curl" package
install.packages("curl")
library(curl)

#read two datasets from Github
x<-read.csv(curl("https://raw.githubusercontent.com/bwu99/apr2021_homework/main/data/Assignment%202%20-%20USA_AL_Auburn-Opelika.AP.722284_TMY3_BASE.csv"))
head(x)
y<-read.csv(curl("https://raw.githubusercontent.com/bwu99/apr2021_homework/main/data/Assignment%202%20-%20new.app4.csv"))
head(y)

#install "dplyr" package
install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library(stringr)
install.packages(("tidyr"))
library(tidyr)

#extract date from two datasets and clean its formats

#separate date into date and hour from 1st dataset x and format them match the values of dataset y
x$date <- str_split_fixed(x$Date.Time," ",3)[,2]
x$hour <- str_sub(str_split_fixed(x$Date.Time," ",3)[,3],2,6)
x$month <- ifelse(substr(str_sub(x$date,1,2),1,1) == "0",str_sub(str_sub(x$date,1,2),2),str_sub(x$date,1,2))
x$day <- ifelse(substr(str_sub(x$date,4,5),1,1)=="0",str_sub(str_sub(x$date,4,5),2),str_sub(x$date,4,5))

#separate date into date and hour from 2nd dataset y
y$date <- str_split_fixed(y$time, " ",2)[,1]
y$hour <- str_split_fixed(y$time, " ",2)[,2]
y$month <- str_split_fixed(y$date, "/",3)[,1]
y$day <- str_split_fixed(y$date, "/",3)[,2]

#merge two datasets
finaldataset <- merge(x,y,by=c("month", "day","hour"))

#convert Wmin to kWh and named "QQQ"
finaldataset$QQQ <- finaldataset$W_min*0.000017
finaldataset1 <- select(finaldataset,-c("month","day","hour","Date.Time","date.x","X","time","W_min","date.y"))

#add consumption of electricity
finaldataset1$total_consumption_electricity <- with(finaldataset, rowSums(finaldataset1))
comsumption_electricity <- merge(x=finaldataset,y=finaldataset1,by=c("Electricity.Facility..kW..Hourly."))
fdataset <- select(finaldataset, c("month","day","hour","Electricity.Facility..kW..Hourly."))
finaldataset2 <- merge(fdataset,finaldataset1,by="Electricity.Facility..kW..Hourly.")
finaldataset2$hourly <-as.numeric(substr(finaldataset2$hour,1,2))

#plots
#monthly
plot(finaldataset2$month,finaldataset2$total_consumption_electricity)
#daily
plot(finaldataset2$day,finaldataset2$total_consumption_electricity)
#hourly
plot(finaldataset2$hourly,finaldataset2$total_consumption_electricity)

#summariza from plots
print("From monthly plots, we can conclude that the August electricity consumption is much less than other months'. As a student, the reason that I think that during a summer holiday, school closed may cause less electricity consumption. From daily plots, more electricity consumption happened in the beginning and middle of month. From hourly plots, more electricity consumption happened in the morning. That makes sense since human activity almost happens in the morning and afternoon, not at night.")



