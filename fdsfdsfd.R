options(digits = 14, digits.secs = 5)

require(data.table)
require(plyr)

if (!require(data.table)){
  install.packages("data.table")  
  require(data.table)
}

if (!require(plyr)){
  install.packages("plyr")  
  require(plyr)
}

path<-"C:\\Users\\Mathias\\Desktop\\txt_data"
setwd(path)
fileNames<- dir(path, pattern = ".txt")

# dat<-fread(fileNames[1], header = T, sep = ",")
# 
# dat2<-dat[,list(wprice = weighted.mean(price,volume)), by = time]
# head(dat2)


dat<-list()
for(i in 1:length(fileNames)){

  dat[[i]]<-  fread(fileNames[i], header = T, sep = ",")
  dat[[i]]<-dat[[i]][,list(wprice = weighted.mean(price,volume)), by = time]
  temp<-substr(fileNames[i], 8, 15)
  temp<-as.POSIXct(temp, format = "%Y%m%d", tz = "CET")
  dat[[i]]$DateTime = temp + dat[[i]]$time
  dat[[i]]$Time = as.numeric(temp) + dat[[i]]$time
  dat[[i]][,time:=NULL]
  dat[[i]]<-dat[[i]][,c(2,3,1)]
  head(dat[[i]])
  print(i)
}

dat3<-rbindlist(dat)

saveRDS(dat3, file = "2014_SPY_Vol_Avg.rdata")
saveRDS(test, file = "2014_SPY_Vol_Avg.rdata")
test<-readRDS("2014_SPY_Vol_Avg.rdata")
dat3==test
identical(dat3,test)
all.equal(dat3,test)
str(dat3)
str(test)
compare(dat3,test, ignoreAttrs=T)
attributes(dat3)
attributes(test)
test<-data.table(test)

test$Date<-format(test$DateTime, format = "%Y-%m-%d")
test<-readRDS("2014_SPY_Vol_Avg.rdata")
test2<-selectDays(test, as.Date("2014-01-01"), nDays = 1)
test4<-selectDays(test, as.Date("2014-01-01"), nDays = 2)
test3<-test2[1:100,]

plot(timePoints(test2, timeOffset = 1)$I)
sum(diff(timePoints(test4, timeOffset = 1)$I)==0)
timePoints(test3, lagOffset = 8, initialDelay = 68)
timePoints(test3, timeOffset = 0.1)
# 
# 
# temp<-substr(fileNames[1], 8, 15)
# temp<-as.POSIXct(temp, format = "%Y%m%d", tz = "CET")
# temp<-strptime(temp, format = "%Y%m%d", tz = "CET")
# temp
# as.numeric(temp)+a
# at<-strptime(a, format = "%OS")
# at
# b<-temp+a
# b
# b<-format(b, timeFormat = "%Y-%m-%d %H:%M:%OS3")
# b
# as.numeric(b)





