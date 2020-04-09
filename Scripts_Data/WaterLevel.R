### BVR Water Level comparisons (WVWA vs. Carey Lab)
### Then match water level with water volume (from ArcGIS) and extrapolate to daily values
### A Hounshell, 07 Apr 2020
### Updated: A Hounshell, 09 Apr 2020

## Load packages
pacman::p_load(tidyverse,ggplot2,zoo)

## Load WVWA data (2009-2018) (will need to be updated!)
wvwa <- read_csv("C:/Users/ahoun/OneDrive/Desktop/BVR_GLM/BVR-GLM/Raw_Data/Beaver_Weir_Readings_29Sep18_Converted.csv")
wvwa$Date <- as.POSIXct(strptime(wvwa$Date, "%m/%d/%Y", tz = "EST"))

## Load in Carey lab data (2018 to 2019)
carey <- read_csv("C:/Users/ahoun/OneDrive/Desktop/BVR_GLM/BVR-GLM/Raw_Data/2018to2019_WaterLevel.csv")
carey$Date <- as.POSIXct(strptime(carey$Date, "%m/%d/%Y", tz = "EST"))

## Merge the two data sets by date
carey_wl <- carey %>% select(Date, BVR_WaterLevel_m) %>% drop_na(BVR_WaterLevel_m)
wvwa_wl <- wvwa %>% select(Date,BVR_WaterLevel_m) %>% drop_na(BVR_WaterLevel_m)

wlevel_comp <- merge(carey_wl,wvwa_wl,by="Date")

wlevel <- merge(carey_wl,wvwa_wl,by="Date",all=TRUE)

wlevel$mean <- rowMeans(wlevel[c('BVR_WaterLevel_m.x', 'BVR_WaterLevel_m.y')], na.rm=TRUE)

wlevel_2 <- wlevel %>% select(Date,mean)
names(wlevel_2)[2] <- "BVR_WaterLevel_m"

write_csv(wlevel_2, path = "C:/Users/ahoun/OneDrive/Desktop/BVR_GLM/BVR-GLM/Data_Output/07Apr20_BVR_WaterLevel.csv")

# Extrapolate to daily vaules

# Remove hour from date
wlevel_2$Date <- format(as.POSIXct(wlevel_2$Date,'%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
daily <- as.data.frame(seq(as.POSIXct("2009-09-01"), as.POSIXct("2019-12-07"), by="days"))
names(daily)[1] <- "Date"
daily$Date <- format(as.POSIXct(daily$Date,'%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')

daily_vol <- merge(daily,wlevel_2,by="Date",all.x = TRUE)

# Lineraly interpolate water level data to daily
daily_vol$BVR_WaterLevel_m <- na.approx(daily_vol$BVR_WaterLevel_m)

# Rounnd to one decimal point
daily_vol$BVR_WaterLevel_m <- format(round(daily_vol$BVR_WaterLevel_m,digits = 1),nsmall=1)

# Export out as csv then use Matlab to create for loop to assign volume for each water level daily from 2009-2019
write_csv(daily_vol, path = "C:/Users/ahoun/OneDrive/Desktop/BVR_GLM/BVR-GLM/Data_Output/09Apr20_BVR_WaterLevelDaily.csv")
