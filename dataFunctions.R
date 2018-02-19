selectDays<-function(x, startDate, endDate = NA, nDays = NA, dateFormat = "%Y-%m-%d"){
  
  #startDate = as.POSIXct(startDate, format = dateFormat, tz = "CET")
  #endDate = as.POSIXct(endDate, format = dateFormat, tz = "CET")
  
  unDates<-unique(x$Date)
  startIndex<-match(TRUE,unDates>=startDate)
  startDate<-unDates[startIndex]
  # startDate2<-unDates[unDates>=startDate][1]
  # startDate<-x[which(x$DateTime>=startDate)[1],1]
  
  print(paste0("Chosen start date: ", startDate))
  
  if(nDays <= 0 ){
    stop("nDays can't be non-positive" )
  }
  
  if(nDays > 0 ){
    if(!is.na(endDate)){
      stop("Specify either 'endDate' xor 'nDays'")
    } else {
      endDate <- unDates[startIndex + nDays - 1]
    }
  } else { #nDays = 1
    if(is.na(endDate)){
      stop("Specify either 'endDate' xor 'nDays'")
    } else {
      endDate <- unDates[unDates >= endDate][1]
      
    }
  }
  
  print(paste0("Chosen end date: ", endDate))
  #print(paste0("Total number of days: "))
    
  return(x[between(Date, startDate, endDate, incbounds = T)])
  
}


timePoints<-function(x, timeOffset = 0, lagOffset = 0, initialDelay = max(timeOffset, lagOffset), minLag = 0){
  # can only specify either timeOffset or lagOffset, and assumes initialDelay 
  #to be on same "scale" i.e. either seconds or lag
  setkey(x, Date)
  
  unDates<-unique(x$Date)
  dateIndicesOffset <- match(unDates, x$Date)
  if(!(timeOffset>0 | lagOffset>0)){
    stop("Specify either 'timeOffset' xor 'timeOffset'")
  }
  
  if((lagOffset>0 & minLag>0)){
    stop("minLag should only be used with timeOffset, not with lagOffset")
  }
  
  indexList<-list()
  
  for(i in 1:length(unDates)){
    currentData <- x[x$Date == unDates[i], "Time"]

    if(timeOffset > 0){
      startTime <- currentData$Time[1]
      lastTime <- currentData$Time[length(currentData$Time)]
      startTime <- startTime + initialDelay
      
      if(sum(currentData$Time<=startTime)<minLag){
        startTime <- currentData$Time[minLag]
        warning("Minimum lag not exceeded by initial delay. Initial timepoint moved according to minimum lag requirement.")
      }
      
      lengthOut<-floor((lastTime-startTime)/timeOffset)+1
      
      allTimes<-seq(startTime, by = timeOffset, length.out = lengthOut)
      #allTimes<-data.table(allTimes, val = allTimes)
      currentData[,val:=Time]
      #setnames(allTimes, c("Timepoints"))
      setattr(currentData, "sorted", "Time")
     
      
      indexList[[i]]<-currentData[J(allTimes), .I, roll = "nearest", by = .EACHI][,Time:=NULL]
      
      #Add final
      if(indexList[[i]][length(indexList[[i]])]!=length(currentData$Time)){
        indexList[[i]]<- rbindlist(list(indexList[[i]], list(length(currentData$Time))))+dateIndicesOffset[i]-1
      }
      
    
    } else {#using lagOffset
      startTime <- initialDelay
      lastTime <- length(currentData$Time)
      allTimes<-seq(startTime, by = lagOffset, length.out = floor((lastTime-startTime)/lagOffset)+1)
      
      #add last
      if(allTimes[length(allTimes)]!=lastTime){
        allTimes<-c(allTimes, lastTime)
      }
      
      indexList[[i]]<-data.table(I=allTimes+dateIndicesOffset[i]-1)
      
    }
  }
  return(rbindlist(indexList))
  
}
