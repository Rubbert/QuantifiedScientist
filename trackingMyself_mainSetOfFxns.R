generalFitbitReader <- function(folderPathFitbit,regexString){
  #A general reader of FitBit files
  # folderPathFitbit - folder with your downloaded fitbit files
  # regexString - regular expression to get only the information you are interested in (e.g. )
  
  library(plyr)
  library(jsonlite)
  #get the files of interest
  allFilesSelFitbit = list.files(path = folderPathFitbit, pattern = regexString)
  #Now load them iteratively into one large df
  totalDfGenReader = NULL
  for (indexSel in 1:length(allFilesSelFitbit)){
    print(paste(indexSel, 'out of', length(allFilesSelFitbit)))
    fileNameSel = allFilesSelFitbit[[indexSel]]
    
    raw_data <- paste(readLines(file.path(folderPathFitbit,fileNameSel)), collapse="")
    json_data2 <- jsonlite::fromJSON(raw_data)
    if (class(json_data2$value)=="data.frame"){
      json_data2 = json_data2$value
      colnames(json_data2) = gsub('value.','',colnames(json_data2), fixed=T)
    }
    
    colsTokeep = which(!(unlist(lapply(json_data2, class)) %in% c("data.frame", "list")))
    json_data2 = json_data2[,colsTokeep]
    
    print(head(json_data2))
    if (is.null(totalDfGenReader)){
      totalDfGenReader = json_data2
    }else{
      #rownames(json_data2) = as.character(as.numeric(rownames(json_data2)) + as.numeric(rownames(totalDfGenReader)[nrow(totalDfGenReader)]))
      
      totalDfGenReader = rbind.fill(totalDfGenReader,json_data2)
    }
  }
  if (('date' %in% colnames(totalDfGenReader)) & !(('dateTime' %in% colnames(totalDfGenReader)))){
    totalDfGenReader$date = as.Date(totalDfGenReader$date, format = "%m/%d/%y")
    totalDfGenReader$dateStr = as.character(totalDfGenReader$date)
  }else if ('dateTime' %in% colnames(totalDfGenReader)){
    totalDfGenReader$dateTimeStr = totalDfGenReader$dateTime
    totalDfGenReader$dateTime = as.POSIXct(totalDfGenReader$dateTimeStr,format="%m/%d/%y %H:%M:%S",tz='GMT')
    totalDfGenReader = totalDfGenReader[which(!duplicated(totalDfGenReader)),]
  }else if ("startTime" %in% colnames(totalDfGenReader)){
    totalDfGenReader$startTimeStr = totalDfGenReader$startTime
    totalDfGenReader$startTime = as.POSIXct(totalDfGenReader$startTime,format="%m/%d/%y %H:%M:%S",tz='GMT')
    
    totalDfGenReader$startTimeStr = totalDfGenReader$originalStartTime
    totalDfGenReader$originalStartTime = as.POSIXct(totalDfGenReader$originalStartTime,format="%m/%d/%y %H:%M:%S",tz='GMT')
    
    totalDfGenReader = totalDfGenReader[which(!duplicated(totalDfGenReader)),]  
  }
  return(totalDfGenReader)
}

convertToFitbitDailyTotals <- function(totalDf,sleepStagesFitbitEpochs,sleepBasedOrMidnight='midnight'){
  #Convert to daily totals
  #totalDf - ...
  #sleepStagesFitbitEpochs - needed for the next variable, if you want it sleep based
  #sleepBasedOrMidnight - do you want the number of steps from midnight to midnight, or based on bed time" 'midnight' or 'sleepbased'
  
  #Now convert this to daily totals
  #Extract all unique dates
  allDatesWithData = unique(as.Date(totalDf$dateTime))
  sleepEndPrevious = NULL
  dataVector = rep(NA,length(allDatesWithData))
  names(dataVector) = as.character(allDatesWithData)
  for (dateSelIndex in 1:length(allDatesWithData)){
    dateSel = allDatesWithData[[dateSelIndex]]
    print(dateSel)
    dateSelChar = as.character(dateSel)
    
    if (sleepBasedOrMidnight=="midnight"){
      if (dateSelChar %in% names(sleepStagesFitbitEpochs)){
        sleepEnd = sleepStagesFitbitEpochs[[dateSelChar]][nrow(sleepStagesFitbitEpochs[[dateSelChar]]),"dateTime"]
        sleepEndTime = sleepEnd
      }else{
        sleepEnd = as.POSIXct(as.character(paste0(dateSel+1,' 05:30:00')),format="%Y-%m-%d %H:%M:%S",tz='Europe/Amsterdam')
        attributes(sleepEnd)$tzone <- "GMT"
      }
      if (is.null(sleepEndPrevious)){
        sleepEndPrevious = as.POSIXct(as.character(paste0(dateSel,' 05:30:00')),format="%Y-%m-%d %H:%M:%S",tz='Europe/Amsterdam')
        attributes(sleepEndPrevious)$tzone <- "GMT"
      }
    }else if (sleepBasedOrMidnight=='sleepbased'){
      sleepEndPrevious = as.POSIXct(as.character(paste0(dateSel,' 00:00:01')),format="%Y-%m-%d %H:%M:%S",tz='Europe/Amsterdam')
      attributes(sleepEndPrevious)$tzone <- "GMT"
      sleepEnd = as.POSIXct(as.character(paste0(dateSel+1,' 00:00:01')),format="%Y-%m-%d %H:%M:%S",tz='Europe/Amsterdam')
      attributes(sleepEnd)$tzone <- "GMT"
    }else{
      break
    }
    #Now sum all steps in between these times
    extractedDay =totalDf[which(totalDf$dateTime<sleepEnd & totalDf$dateTime>=sleepEndPrevious), ]
    sumForTheDay = sum(as.numeric(extractedDay$value),na.rm=T)
    
    dataVector[[dateSelChar]] = sumForTheDay
    
    sleepEndPrevious = sleepEnd
  }
  return(dataVector)
}


convertToSleepEpochsFitBit <- function(fitbitGeneralFolder, folderNameFitbit){
  #Convert FitBit sleep data to 30 second epochs
  #fitbitGeneralFolder - main folder that contains the folder with fitbit downloads
  #folderNameFitbit - the subfolder that contains all the actual files
  
  #First read in all the exported fitbit sleep data
  #The data are devided into several files, each about 30 days of data
  folderPathFitbit = file.path(fitbitGeneralFolder, folderNameFitbit)
  filepathFitbitSleep = file.path(folderPathFitbit,'user-site-export')
  sleepFiles = list.files(path = filepathFitbitSleep, pattern = '^sleep.*\\.json$')
  sleepStagesPerDayFull = list()
  for (fileIndex in 1:length(sleepFiles)){
    print(fileIndex)
    sleepFileName = sleepFiles[[fileIndex]]
    raw_data <- paste(readLines(file.path(filepathFitbitSleep,sleepFileName)), collapse="")
    #json_data <- rjson::fromJSON(raw_data, method="C")
    json_data2 <- jsonlite::fromJSON(raw_data)
    sleepStagesPerDay = json_data2$levels$data
    sleepStagesPerDayFull = c(sleepStagesPerDayFull, sleepStagesPerDay)
  }
  
  #now convert to 30 second epochs and add date to name
  sleepStagesFitbitEpochs = list()
  for (indexSel in 1:length(sleepStagesPerDayFull)){
    print(paste(indexSel, 'out of', length(sleepStagesPerDayFull)))
    dataOneDay = sleepStagesPerDayFull[[indexSel]]
    
    dataOneDay$dateTimeStr =dataOneDay$dateTime
    dataOneDay$dateTime = as.POSIXct(dataOneDay$dateTimeStr,format="%Y-%m-%dT%H:%M:%S",tz=Sys.timezone())
    
    dataOneDayEpochs = as.data.frame(matrix(NA, nrow=0,ncol=ncol(dataOneDay)))
    colnames(dataOneDayEpochs) = dataOneDay
    for (rowIndex in 1:nrow(dataOneDay)){
      toAddEpoch <- as.data.frame(lapply(dataOneDay[rowIndex,,drop=F], rep, (dataOneDay[rowIndex,'seconds']/30)))
      toAddEpoch$dateTime = do.call("c",lapply(c(0:(nrow(toAddEpoch)-1)), function(x) dataOneDay[rowIndex,'dateTime']+(x*30)))
      dataOneDayEpochs = rbind(dataOneDayEpochs, toAddEpoch)
    }
    
    
    datesInFitbit =  unique(as.Date(dataOneDayEpochs$dateTime))
    if (length(datesInFitbit)==1){
      dateOfTheDayBefore = datesInFitbit[[1]]-1
    }else{
      dateOfTheDayBefore = datesInFitbit[[1]]
    }
    #dateOfTheDayBefore = dateOfTheDayBefore[[1]]
    print(dateOfTheDayBefore)
    
    dataOneDayEpochs$sleepStageStr = dataOneDayEpochs$level
    if (as.character(dateOfTheDayBefore) %in% names(sleepStagesFitbitEpochs) && 
        nrow(dataOneDayEpochs)==nrow(sleepStagesFitbitEpochs[[as.character(dateOfTheDayBefore)]]) &&
        all.equal(dataOneDayEpochs[,'dateTime'],sleepStagesFitbitEpochs[[as.character(dateOfTheDayBefore)]][,'dateTime'])){
      print(paste0(dateOfTheDayBefore, ' is double'))
      #The first date from the previous sleep file is the same as the last date from the next file --> so there will be doubles
      #see e.g. "2017-08-06T04:07:00.000" in files "sleep-2017-08-06.json" and "sleep-2017-07-07.json"
    }else if (as.character(dateOfTheDayBefore) %in% names(sleepStagesFitbitEpochs)){
      numberOfOverlappingEpochs = length(intersect(sleepStagesFitbitEpochs[[as.character(dateOfTheDayBefore)]][,'dateTime'], 
                                                   dataOneDayEpochs[,'dateTime']))
      #Sometimes there will be naps, which means there are multiple sleep recordings per date
      #However, there can be no overlapping times
      stopifnot(numberOfOverlappingEpochs==0)
      sleepStagesFitbitEpochs[[as.character(dateOfTheDayBefore)]] = rbind(sleepStagesFitbitEpochs[[as.character(dateOfTheDayBefore)]],dataOneDayEpochs)
    }else{
      sleepStagesFitbitEpochs[[as.character(dateOfTheDayBefore)]] = dataOneDayEpochs
    }
  }
  saveRDS(object = sleepStagesFitbitEpochs, file.path(filepathFitbitSleep, 'sleepStagesFitbitEpochs.RDS'))
}


loadFitBitDataFromFolder <- function(fitbitGeneralFolder, folderNameFitbit){
  #Loads most of the FitBit data in daily totals
  #fitbitGeneralFolder - main folder that contains the folder with fitbit downloads
  #folderNameFitbit - the subfolder that contains all the actual files
  
  folderPathFitbit = file.path(fitbitGeneralFolder,folderNameFitbit,'user-site-export')
  #Load the sleep epochs to define days and calculate sleep stages
  #NOTE: IMPROVE
  #The Fitbit epoch data already needs to be there. I use this to get daily totals (e.g. steps), not from midnight to midnight, but from 
  #taking into account bedtime
  sleepStagesFitbitEpochs = readRDS(file.path(folderPathFitbit, 'sleepStagesFitbitEpochs.RDS'))
  
  #Calculate the average getting up time per day of the week
  #We can use this for the missing data in defining start and end of days
  #First load steps
  regexStringSteps = '^steps\\-.*'
  totalDfSteps = generalFitbitReader(folderPathFitbit,regexStringSteps)
  dataVectorSteps = convertToFitbitDailyTotals(totalDfSteps,sleepStagesFitbitEpochs)
  dataVectorSteps = dataVectorSteps[!is.na(names(dataVectorSteps))]
  dfSteps = data.frame(dataVectorSteps)
  names(dfSteps) = 'steps'
  
  
  #Now calories
  regexStringCalories = '^calories\\-.*'
  totalDfCalories = generalFitbitReader(folderPathFitbit,regexStringCalories)
  dataVectorCalories = convertToFitbitDailyTotals(totalDfCalories,sleepStagesFitbitEpochs)
  dataVectorCalories = dataVectorCalories[!is.na(names(dataVectorCalories))]
  dfCalories = data.frame(dataVectorCalories)
  names(dfCalories) = 'calories'
  
  mergedDfFitBit = merge(x = dfSteps, y = dfCalories, by='row.names', all = T)
  rownames(mergedDfFitBit) = mergedDfFitBit$Row.names
  mergedDfFitBit$Row.names <- NULL
  
  #resting_heart_rate
  regexStringSel = paste0('^resting_heart_rate.*')
  totalDfRHR = generalFitbitReader(folderPathFitbit,regexStringSel)
  totalDfRHR = totalDfRHR[!is.na(totalDfRHR$date),]
  rownames(totalDfRHR) = totalDfRHR$dateStr
  totalDfRHR = totalDfRHR[,'value',drop=F]
  colnames(totalDfRHR) = 'resting.hear.rate'
  
  mergedDfFitBit = merge(x = mergedDfFitBit, y = totalDfRHR, by='row.names', all = T)
  rownames(mergedDfFitBit) = mergedDfFitBit$Row.names
  mergedDfFitBit$Row.names <- NULL
  
  #sedentary_minutes
  #lightly_active_minutes
  #very_active_minutes
  #moderately_active_minutes
  minutesInActivityLevels = NULL
  for (activityLevelSel in c('sedentary','lightly_active','very_active','moderately_active')){
    regexStringSel = paste0(activityLevelSel,'_minutes.*')
    totalDfSelActivity = generalFitbitReader(folderPathFitbit,regexStringSel)
    rownames(totalDfSelActivity) = as.character(totalDfSelActivity$dateTime)
    totalDfSelActivity = totalDfSelActivity[,'value',drop=F]
    totalDfSelActivity$value = as.numeric(totalDfSelActivity$value)
    colnames(totalDfSelActivity) = paste0(activityLevelSel,'_minutes')
    if (is.null(minutesInActivityLevels)){
      minutesInActivityLevels = totalDfSelActivity
    }else{
      minutesInActivityLevels = cbind.data.frame(minutesInActivityLevels,totalDfSelActivity)
    }
  }
  
  mergedDfFitBit = merge(x = mergedDfFitBit, y = minutesInActivityLevels, by='row.names', all = T)
  rownames(mergedDfFitBit) = mergedDfFitBit$Row.names
  mergedDfFitBit$Row.names <- NULL
  
  
  #summarize the sleepstages fitbit
  head(sleepStagesFitbitEpochs)
  totalSleepDfFitbit = as.data.frame(matrix(NA,nrow=length(sleepStagesFitbitEpochs), ncol=9))
  rownames(totalSleepDfFitbit) = names(sleepStagesFitbitEpochs)
  colnames(totalSleepDfFitbit) = c('sleepFitbit_mins_wake','sleepFitbit_mins_light','sleepFitbit_mins_rem','sleepFitbit_mins_deep',
                                   'sleepFitbit_perc_wake','sleepFitbit_perc_light','sleepFitbit_perc_rem','sleepFitbit_perc_deep',
                                   'sleepFitbit_totalMinsInBed')
  #mins: light, deep, wake, rem
  #percentages: light, deep, wake, rem
  #minsInBed
  for (dateSel in names(sleepStagesFitbitEpochs)){
    selDatFitbit = sleepStagesFitbitEpochs[[dateSel]]
    totalMinsInBed = nrow(selDatFitbit)/2
    if (!('awake' %in% selDatFitbit$level)){
      minsDeep = sum(selDatFitbit$level=='deep')/2
      minsRem = sum(selDatFitbit$level=='rem')/2
      minsLight = sum(selDatFitbit$level=='light')/2
      minsWake = sum(selDatFitbit$level=='wake')/2
    }else{
      minsDeep = NA
      minsRem = NA
      minsLight = NA
      minsWake = NA
    }
    percDeep = 100 * minsDeep/totalMinsInBed
    percRem = 100 * minsRem/totalMinsInBed
    percLight = 100 * minsLight/totalMinsInBed
    percWake = 100 * minsWake/totalMinsInBed
    
    #Ned to add 1 from the date since in the epoch loading I use the "monday evening" as a reference, while the fitbit counts this as the tuesday
    totalSleepDfFitbit[as.character(as.Date(dateSel)+1),c('sleepFitbit_mins_wake','sleepFitbit_mins_light','sleepFitbit_mins_rem','sleepFitbit_mins_deep',
                                 'sleepFitbit_perc_wake','sleepFitbit_perc_light','sleepFitbit_perc_rem','sleepFitbit_perc_deep',
                                 'sleepFitbit_totalMinsInBed')] = c(minsWake, minsLight, minsRem, minsDeep,
                                                                    percWake, percLight, percRem, percDeep,
                                                                    totalMinsInBed)
    
  }
  
  
  mergedDfFitBit = merge(x = mergedDfFitBit, y = totalSleepDfFitbit, by='row.names', all = T)
  rownames(mergedDfFitBit) = mergedDfFitBit$Row.names
  mergedDfFitBit$Row.names <- NULL
  
  mergedDfFitBit = mergedDfFitBit[!is.na(mergedDfFitBit$steps),]
  return(mergedDfFitBit)
}

#Now test plot some of the stages
plot_hypnogram <- function(stageDf,colSel,titleSel='' ,timeCol = 'dateTime_set1', labels = c("deep","light","rem","wake","missing")){
  #Adapted this script from: https://github.com/boupetch/sleepr/blob/master/R/hypnogram.R
  stages <- stageDf
  stages$begin <- stages[,timeCol]
  stages$end <- stages[,timeCol]+30
  
  stages$sleepStageStr <- factor(stages[,colSel], levels = labels)
  hypnogram <- ggplot2::ggplot(stages,ggplot2::aes_string(x="begin",y=colSel,group=1)) +
    ggplot2::geom_line() + 
    ggplot2::theme_bw() +
    ggplot2::xlab("") + 
    ggplot2::ylab("") +
    ggtitle(titleSel) + theme(plot.title = element_text(hjust = 0.5))
  
  rem = stages[stages$sleepStageStr == "rem",]
  
  if(nrow(rem) > 0){
    for(i in c(1:nrow(rem))){
      df <- stats::reshape(rem[i,], idvar = colSel, varying = c("begin","end"),
                           v.names = "value", direction = "long")
      hypnogram <- hypnogram+ggplot2::geom_line(data=df,mapping = ggplot2::aes_string(x="value",y=colSel,group=1),colour='red',size=2)
    }
  }
  hypnogram = hypnogram + scale_x_datetime(breaks = date_breaks("1 hour"), labels = date_format("%H:%M", tz = "Europe/Amsterdam")) 
  
  hypnogram = hypnogram + theme_xkcd  + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=19),panel.grid.major.y = element_line(color = "grey80"))
  
  
  return(hypnogram)
}
