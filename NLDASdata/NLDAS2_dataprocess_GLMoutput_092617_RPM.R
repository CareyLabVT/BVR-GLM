### Upload and transform the NLDAS date for GLM FCR ###
### Dates include DEC 2014 through Oct 2017 ###

### Author(s) HD, NKW, MEL & RPM ###
### Last updated from RPM and the NLDAS2 Downloading script ###
### DATE: 092617 ###

### Updated by AGH - adjust from GMT to EST, 09 Mar 2021

library(dplyr)

#setwd
setwd("./NLDASData") #relative path

press<-read.csv("FCR_NLDAS2_PRESsfc_110_SFC.csv") #PRES = surface Pressure [Pa]

temp<-read.csv("FCR_NLDAS2_TMP2m_110_HTGL.csv") #TMP = 2 m aboveground temperature [K]

precip<-read.csv("FCR_NLDAS2_APCPsfc_110_SFC_acc1h.csv") #APCP = precipitation hourly total [kg/m2]

hum<-read.csv("FCR_NLDAS2_SPFH2m_110_HTGL.csv") #SPFH = 2 m aboveground Specific humidity [kg/kg]

windx<-read.csv("FCR_NLDAS2_UGRD10m_110_HTGL.csv") #UGRD = 10 m aboveground Zonal wind speed [m/s]

windy<-read.csv("FCR_NLDAS2_VGRD10m_110_HTGL.csv") #VGRD = 10 m aboveground Meridional wind speed [m/s]

long<-read.csv("FCR_NLDAS2_DLWRFsfc_110_SFC.csv") #DLWRF = longwave radiation flux downwards (surface) [W/m2]

short<-read.csv("FCR_NLDAS2_DSWRFsfc_110_SFC.csv") #DSWRF = shortwave radiation flux downwards (surface) [W/m2]

### Convert Temperature to Degrees C
temp<-temp%>%mutate(AirTemp=TMP2m_110_HTGL-273.15)%>% select(dateTime, AirTemp)

### Convert precip to m/day
precip<-precip%>%mutate(Rain=APCPsfc_110_SFC_acc1h/1000*24)%>% select(dateTime, Rain) #m/day

### calculate wind speeds from Zonal and Meridional
### https://www.ncl.ucar.edu/Document/Functions/Contributed/wind_speed.shtml

windx$dateTime <- as.POSIXct(strptime(windx$dateTime, "%Y-%m-%d %H", tz="EST"))
wind <- cbind(windx, windy$VGRD10m_110_HTGL, deparse.level = 1)
names(wind)[6] <- "VGRD10m_110_HTGL"

### ran these each seperately to make sure I am getting the correct outputs!
wind <- select(wind,dateTime,UGRD10m_110_HTGL,VGRD10m_110_HTGL)
wind <- mutate(wind,Wind = sqrt(UGRD10m_110_HTGL^2+VGRD10m_110_HTGL^2))
wind <- select(wind, dateTime, Wind)


### Convert specific humidity to relative humidity
### from Bolton 1980 The computation of Equivalent Potential Temperature 
### \url{http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html}
### @author David LeBauer

relhum <- cbind(temp, hum$SPFH2m_110_HTGL, deparse.level = 1)
names(relhum)[3] <- "spechum"

qair2rh <- function(qair, temps, press = 1013.25){
  es <-  6.112 * exp((17.67 * temps)/(temps + 243.5))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
  return(rh)
}

qair <- relhum$spechum
temps <- relhum$AirTemp

relhum <- qair2rh(qair,temps)*100

hum <- cbind(hum, relhum, deparse.level = 1)
hum <- select(hum, dateTime, relhum)
names(hum)[2] <- "RelHum"

### Select the longwave and Shortwave Values for FCR ###

short <- select(short, dateTime, DSWRFsfc_110_SFC)
names(short)[2] <- "ShortWave"

long <- select(long, dateTime, DLWRFsfc_110_SFC)
names(long)[2] <- "LongWave"

### Bind them all together ### 
FCR_NLDAS_met <- cbind(short, long$LongWave, temp$AirTemp, hum$RelHum, wind$Wind, precip$Rain, deparse.level = 1)

names(FCR_NLDAS_met)[3] <- "LongWave"
names(FCR_NLDAS_met)[4] <- "AirTemp"
names(FCR_NLDAS_met)[5] <- "RelHum"
names(FCR_NLDAS_met)[6] <- "Wind"
names(FCR_NLDAS_met)[7] <- "Rain"

### Write the FCR NLDAS data into a format for GLM
write.table(FCR_NLDAS_met,"FCR_GLM_met_NLDAS2_JAN20_DEC20.csv", sep = ",", row.names = F) #be sure to update this with the date of the data you are pulling!

## Adjusted to EST for GLM modeling
FCR_NLDAS_met$dateTime <- as.POSIXct(FCR_NLDAS_met$dateTime, format='%Y-%m-%d %H:%M:%S', tz="GMT")
FCR_NLDAS_met$dateTime_EST <- format(FCR_NLDAS_met$dateTime,tz="America/New_York")

FCR_NLDAS_met_est <- FCR_NLDAS_met %>% 
  select(-dateTime) %>% 
  rename(dateTime = dateTime_EST)

col_order <- c("dateTime","ShortWave","LongWave","AirTemp","RelHum","Wind","Rain")
FCR_NLDAS_met_est <- FCR_NLDAS_met_est[, col_order]

write.table(FCR_NLDAS_met_est,"FCR_GLM_met_NLDAS2_JAN20_DEC20_estAdj.csv", sep = ",", row.names = F)

#Let's assume that you're now merging multiple files together: MERGING TIME!!!!

library(zoo)

data1<-FCR_NLDAS_met #whatever new file was just created

data1$time<-data1$dateTime #depending on whether your time column is time or dateTime
data1$WindSpeed<-data1$Wind #only do this if you have Wind Speed instead of Wind

data1$dateTime<-NULL #get rid of these bad cols
data1$Wind<-NULL #this one too

data2<-read.csv("FCR_GLM_met_NLDAS2_Dec14_Dec19.csv", header=TRUE) #past accumulated file
  
newdata<-merge(data1,data2, all=TRUE) #merge them- go!

newdata$time<-as.POSIXct(strptime(newdata$time, "%Y-%m-%d %H:%M:%S", tz="EST")) #get dates into POSIX
newdata<-newdata[order(newdata$time),] #sort sort sort

write.table(newdata, "FCR_GLM_met_NLDAS2_Dec14_Dec19.csv", sep = ",", row.names=F) #brand new accumulated file
