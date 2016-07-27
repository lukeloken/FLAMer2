#Code that does stuff

#More code that does other stuff
library(R.matlab)
library(gdata)

setwd("E:/Dropbox/FLAME_ColumbiaRiver/picarro_data")

setwd("matlab")

files<-list.files()

pic1<-readMat(files[1])

setwd("..")
setwd("csv")

files<-list.files()

pic1<-read.table(files[2], header=T, sep=",", stringsAsFactors = F)
head(pic1[,1:5])

pic1$DateTime<-as.POSIXct(paste(pic1$DATE, floor((pic1$TIME*24)), pic1$min, pic1$sec, sep=" "), format="%Y-%m-%d %H %M %S", tz="UTC")

head(pic1$DateTime)
pic1<-pic1[order(pic1$DateTime),]

plot(pic1$DateTime, pic1$HR_12CH4_dry)


