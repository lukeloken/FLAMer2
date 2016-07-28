# Load and process picarro data
# Data saved in Flame_ColumbiaRiver dropbox folder from July 2016 campaign

# library(R.matlab)
# library(gdata)
library(stringr)

#How many seconds was the Picarro clock off the gps clock?
Picarro_Time_Offset<-15

#Load Picarro directory and list all files
setwd("E:/Dropbox/FLAME_ColumbiaRiver")
dir<-("picarro_data/csv")

#Bind all files in above path using read.table - Need to specify skip, header, etc. 

PicarroRead<-function(dir, Picarro_Time_Offset){
  PicarroPath<-list.files(dir)
  if (length(PicarroPath)>0){
    PicarroFull<-do.call("rbind", lapply(paste(dir, "/", PicarroPath, sep=""), read.table, sep="," ,skip=0,  header = TRUE, fill=TRUE)) 
  
    PicarroFull$DateTime_UTC<-as.POSIXct(paste(PicarroFull$DATE, floor((PicarroFull$TIME*24)), PicarroFull$min, PicarroFull$sec, sep=" "), format="%Y-%m-%d %H %M %S", tz="UTC")
    PicarroFull<-PicarroFull[order(PicarroFull$DateTime_UTC),]
  
    PicarroFull=PicarroFull[!duplicated(PicarroFull[,"DateTime_UTC"]),]

    PicarroFull$DateTime_UTC<-(PicarroFull$DateTime_UTC+Picarro_Time_Offset[1])
  
    Picarrovars <- c(68, 26:45)
    PicarroFull <- PicarroFull[,Picarrovars]

    return(PicarroFull)
  }
}




