### Script to compare precipitation: USGS Roanoke Gage, FCR Precip gage, NLDAS data
### A Hounshell, 27 Aug 2020

# Load libraries
pacman::p_load(tidyverse,ggplot2,zoo)

setwd("C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Raw_Data")

# Load data: USGS Roanoke Gage (from Heather); downloaded NLDAS data; FCR precip (from EDI)
usgs <- read_csv("USGS_precip_2013-2019.csv")
usgs$mdate <- as.POSIXct(strptime(usgs$mdate, "%m/%d/%Y", tz = "EST"))

nldas <- read_csv('C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/inputs/BVR_GLM_NLDAS_010113_123119_GMTadjusted.csv')
nldas$time <- as.POSIXct(strptime(nldas$time, "%Y-%m-%d", tz = "EST"))

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/389/4/c1db8816742823eba86696b29f106d0f" 
infile1 <- paste0(getwd(),"/Met_final_2015_2019.csv")
download.file(inUrl1,infile1,method="curl")

fcr <- read.csv("Met_final_2015_2019.csv", header=T)
fcr$DateTime <- as.POSIXct(strptime(fcr$DateTime, "%Y-%m-%d", tz = "EST"))

# Convert USGS data from in/d to mm/d
# AGH NOTE: Pretty sure in/d was in mm/d?
usgs <- usgs %>% mutate(p_mm = P_inperd*25.4)

# Average nldas data across days (mm/d)
# NOTE: Needed to convert NLDAS data to hourly rain by multiplying by 60 then summing over the day
nldas_rain <- nldas %>% select(time,Rain) %>% mutate(Rain_hr = Rain*60) %>% 
  group_by(time) %>% summarise_all(funs(sum))


# Averagey FCR data across days (mm/d)
fcr_rain <- fcr %>% select(DateTime,Rain_Total_mm) %>% group_by(DateTime) %>% summarise_all(funs(sum))

# Plot them all
ggplot()+
  geom_line(usgs,mapping=aes(x=mdate,y=P_inperd,color="USGS"))+
  geom_line(nldas_rain,mapping=aes(x=time,y=Rain_hr,color="NLDAS"))+
  geom_line(fcr_rain,mapping=aes(x=DateTime,y=Rain_Total_mm,color="FCR Met"))+
  theme_classic(base_size = 15)

# Merge all by date
usgs_date <- usgs %>% select(mdate,P_inperd) %>% rename(date=mdate,usgs_mm=P_inperd)
nldas_date <- nldas_rain %>% select(time,Rain_hr) %>% rename(date=time,nldas_mm=Rain_hr)
fcr_date <- fcr_rain %>% rename(date=DateTime,fcr_mm=Rain_Total_mm)

all <- merge(usgs_date,nldas_date,by="date")
all <- merge(all,fcr_date,by="date")

ggplot()+
  geom_line(all,mapping=aes(x=date,y=usgs_mm,color="USGS"))+
  geom_line(all,mapping=aes(x=date,y=nldas_mm,color="NLDAS"))+
  geom_line(all,mapping=aes(x=date,y=fcr_mm,color="FCR"))+
  theme_classic(base_size=15)

ggplot(all,mapping=aes(x=fcr_mm,y=usgs_mm))+
  geom_point()+
  theme_classic(base_size=15)

ggplot(all,mapping=aes(x=fcr_mm,y=nldas_mm))+
  geom_point()+
  theme_classic(base_size=15)
