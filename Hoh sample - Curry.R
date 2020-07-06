#This code calculates the distance and time between each tracked point with the purpose of finding the average hourly distance traveled per hour for a single fish, which can be used for all fish.
#The calculated time between each tracked point is only used to check for oddities in the data and is not used in the actual calculation.

library(tidyverse)

#load data containing all fish from Hoh river
#load("Hoh_sheets3")

#subset 1 fish
#F4780F = subset(Hoh_sheets3, Hoh_sheets3$fishID == 4780)

##########################################################
load("F4780F")

#Calculating distances between each tracked point

#Name function arguments
X2<- F4780F$PosX
X<- F4780F$PosX 
Y2<- F4780F$PosY 
Y<- F4780F$PosY 
#function
Distance <- function(X,X2,Y,Y2){
  sqrt(((X2-X)^2) + ((Y2-Y)^2))
}
#except first row
dist.vect<-rep(NA,length(F4780F$PosX)-1)

#loop for entire df
for (i in 1:length(F4780F$PosX)){
  dist.vect[i]<-Distance(X2[i+1],X[i],Y2[i+1],Y[i])
}

dist.vect

#add distance to df
F4780F["distance"] <- dist.vect
#############################################################
#calculating times for single fish

#orders df by Echotime
F4780F<-F4780F %>% arrange(EchoTime)

t<-F4780F[order(F4780F$EchoTime),]

#except first row
time.vect<-rep(NA,length(t$EchoTime))

#loop for entire df
for (i in 2:(length(t$EchoTime))){
  time.vect[i]<-difftime(t$EchoTime[i],t$EchoTime[i-1], units = "secs")
}

time.vect

#add time to df
t["time"] <- time.vect

#gets rid of gaps larger than 1 hour
gooddata = subset(t, t$time < 3600)

#assigns each hour of each day to a df in the environment.
for (k in unique(gooddata$Day)){
  Daily = subset(gooddata, gooddata$Day == k)
  for (l in unique(Daily$Hour)){
    Hourly = subset(Daily, Daily$Hour == l)
    name <- paste ("Day", k, "Hour", l, "4780", sep=".")
    assign(name, Hourly[Hourly$Hour==l,])
  }}

trial4780 <- lapply(ls(pattern="\\.4780"), function(x) get(x))


#if statement to get rid of gaps, and creating a df with dist.total/time.total for each.
trial <-list()
for(i in 1:length(trial4780)){
  dat<- trial4780[[i]]
  dist.total <- 0
  time.total <- 0
  for(j in 1: (nrow(dat)-1)){
    dist.total <- dist.total + dat$distance[j]
    time.total <- time.total + as.numeric(dat$EchoTime[j+1]-dat$EchoTime[j])
    Hour<- dat$Hour[j]
  }
  
  trial[[i]]<- data.frame(Hour,dist.total, time.total)
  print(trial)
  
}
trial

F4780df <- do.call(rbind,trial)
F4780df

#calculates mean of columns (dist.total = average distance traveled / hour)
F4780m<- F4780df %>%
  sapply(., function(col) as.numeric(as.character(col))) %>%
  colMeans(na.rm = TRUE)
F4780m
