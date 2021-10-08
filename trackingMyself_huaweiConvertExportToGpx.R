setwd('/Users/rterhorst/Research/src/')
library(caret)
library('rlang')
library("foreign")
library("R.utils")
library('zoo')
library(ggplot2)
#library(chron)
#library(lme4)
library("openxlsx")
#library(hash)
library(stringr)
library(data.table)
library('jsonlite')
library(lubridate)
source('trackingMyself_mainSetOfFxns.R')

Lines <- readLines('/Users/rterhorst/Research/src/ConfigFiles/Directories_macosx_16inch.cfg')
Lines2 <- chartr("[]", "==", Lines) 
DF <- read.table(textConnection(Lines2), as.is = TRUE, sep = "=", fill = TRUE) 
ini_data = subset(transform(DF, V3 = na.locf(ifelse(V1 == "", V2, NA))), V1 != "") 
#Define datadir
data_dir_track <- ini_data$V2[ini_data$V1=="trackingmyself " & ini_data$V3=="DataDirectories"]
data_dir_track <- substring(data_dir_track, 2)
image_dir_track <- ini_data$V2[ini_data$V1=="trackingmyself " & ini_data$V3=="ImageDirectories"]
image_dir_track <- substring(image_dir_track, 2)

huaweiGeneralFolder = file.path(data_dir_track,'huawei')

huaweiFolderSel = file.path(huaweiGeneralFolder,'huaweiWatch3','HUAWEI_HEALTH_20211004184532')

folderToSaveGpx = file.path(huaweiFolderSel, 'robGpx')
mkdirs(folderToSaveGpx)
##
filePathData = file.path(huaweiFolderSel,'Motion path detail data & description','motion path detail data.json')
raw_data <- paste(readLines(filePathData), collapse="")
json_data2 <- jsonlite::fromJSON(raw_data)

locationInfo = json_data2$attribute
distanceInfo = json_data2$totalDistance
stepsRealInfo = json_data2$realSteps
stepsTotalInfo = json_data2$totalSteps
startTimeInfo = json_data2$startTime
sportType = json_data2$sportType

#Check just those for which a significant distance was recorded
indicesWithDistance = which(distanceInfo>2000)
for (sportSelIndex in indicesWithDistance){
  print(sportSelIndex)
  print(paste0('start time = ', startTimeInfo[[sportSelIndex]]))
  print(paste0('sport type = ', sportType[[sportSelIndex]]))
  
  #Extract the string with the lon and lat
  locationInfoRaw = locationInfo[[sportSelIndex]]
  locationInfoRawSplit = strsplit(locationInfoRaw, split = '\n')[[1]]
  ##Keep only the ones with longitude and lattitude
  locationInfoRawSplit = locationInfoRawSplit[grep(pattern = '.*\\;lat\\=[0-9].*', x = locationInfoRawSplit) ]
  locationInfoMatrix = fread(text = locationInfoRawSplit, data.table = F)
  #Convert data and column namesto right format
  locationInfoMatrix$V1 = NULL#gsub('HW_EXT_TRACK_DETAIL@is','',locationInfoMatrix$V1,fixed=T)
  for (colIndex in c(1:ncol(locationInfoMatrix))){
    prefix = str_split(locationInfoMatrix[1,colIndex],'=')[[1]][[1]]
    locationInfoMatrix[,colIndex] = as.numeric(gsub(paste0(prefix,'='),'',locationInfoMatrix[,colIndex]))
    colnames(locationInfoMatrix)[[colIndex]]=prefix
  }
  
  #Convert data-time (still need to check if timezone is correct)
  locationInfoMatrix$dateTime <- as.POSIXct(locationInfoMatrix$t, origin="1970-01-01")
  
  #Remove some with a wrong last row
  locationInfoMatrix = locationInfoMatrix[which(locationInfoMatrix[,'lat']!=90),]
  
  #Some extra header info
  pre <- '<?xml version="1.0" encoding="utf-8" standalone="yes"?> <gpx version="1.1" creator="GPS Visualizer http://www.gpsvisualizer.com/" xmlns="http://www.topografix.com/GPX/1/1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd"> <trk>   <name>test1</name> <trkseg>'
  post <- '</trkseg>  </trk>  </gpx>'
  
  #Create variable to save
  textGPX = paste(
    c(pre,
      mapply(function(lat, lon, datetime) {
        sprintf('<trkpt lat="%f" lon="%f">
        <time>%s</time>
      </trkpt>', lat, lon, datetime)
      }, locationInfoMatrix$lat, locationInfoMatrix$lon, locationInfoMatrix$dateTime),
      post, "\n"),
    collapse="\n")

  #save to file
  writeLines(text = textGPX, con = file.path(folderToSaveGpx,paste0('sport',sportType[[sportSelIndex]],'_',as.POSIXct(json_data2$startTime[[sportSelIndex]]/1000, origin="1970-01-01"),'.GPX')))
}



#https://gis.stackexchange.com/questions/187475/converting-position-data-created-in-r-to-gpx-format

#   dat <- read.table(header=TRUE, stringsAsFactors=FALSE, text='
# DEVICE_ID   LAT         LONGITUDE   DATE        TIME
# 150211559   12.920818   77.600197   02-01-17    0:00:00
# 150211559   12.914159   77.600037   02-01-17    0:01:39
# 150211559   12.919819   77.600189   02-01-17    0:00:10
# 150211559   12.919434   77.600174   02-01-17    0:00:20
# 150211559   12.918937   77.60009    02-01-17    0:00:29
# 150211559   12.914159   77.600037   02-01-17    0:01:49
# 150211559   12.918482   77.600136   02-01-17    0:00:39
# 150211559   12.917423   77.60009    02-01-17    0:00:49')
#   
#   dat$dt <- format(as.POSIXct(paste(dat$DATE, dat$TIME), format="%m-%d-%y %H:%M:%S"),
#                    format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
