### Script to look at Temperature in BVR inflows (from RC days in 2019)
### A Hounshell, 18 Jun 2020

# Load in libraries
pacman::p_load(tidyverse,ggplot2,zoo)

# Load in data: from 2025 Desktop
rc_day <- read_csv("C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Raw_Data/2019_2020_continuum_data.csv")
rc_day$DateTime <- as.POSIXct(strptime(rc_day$DateTime, "%m/%d/%Y %H:%M", tz = "EST"))

bvr <- rc_day %>% filter(Reservoir == "BVR") %>% filter(Site == "B100" | Site == "B200")

fcr <- rc_day %>% filter(Reservoir == "FCR") %>% filter(Site == "F100")

# Plot
ggplot()+
  geom_point(bvr,mapping=aes(x=DateTime,y=Temp_Celcius,color=Site))+
  geom_line(bvr,mapping=aes(x=DateTime,y=Temp_Celcius,color=Site))+
  geom_point(fcr,mapping=aes(x=DateTime,y=Temp_Celcius,color=Site))+
  geom_line(fcr,mapping=aes(x=DateTime,y=Temp_Celcius,color=Site))+
  theme_classic(base_size=15)
