### Script to calculate BVR outflow from calculated inflows (Schuler equation + bootstrapped baseflow) and 
### BVR water level
### A Hounshell, 20 Apr 20

# Load in packages
pacman::p_load(tidyverse,ggplot2,zoo)

# Load in calculated inflow (from Schuler_Eq)
q <- read_csv("C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Data_Output/q_calc.csv")
q$time <- as.POSIXct(strptime(q$time, "%Y-%m-%d", tz = "EST"))

# Load in BVR water level data
vol <- read_csv("C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Data_Output/09Apr20_BVR_WaterLevelDailyVol.csv")
vol$Date <- as.POSIXct(strptime(vol$Date, "%m/%d/%Y", tz = "EST"))
vol1 <- vol %>% filter(Date>=as.Date('2013-01-01')&Date<=as.Date('2018-12-31')) %>% select(Date,BVR_Vol_m3)

# Calculate change dvol/dt for each time point
vol2 <- vol %>% filter(Date>=as.Date('2013-01-02')&Date<=as.Date('2019-01-01')) %>% select(Date,BVR_Vol_m3)

dvol <- vol %>% filter(Date>=as.Date('2013-01-02')&Date<=as.Date('2019-01-01')) %>% select(Date)

# Calculate dVol/dt by vol2-vol1/s
vol3 <- as.data.frame((vol2$BVR_Vol_m3 - vol1$BVR_Vol_m3)/(24*60*60))
names(vol3)[1] <- "dv_m3s"

dvol <- cbind.data.frame(dvol,vol3)

ggplot(dvol,mapping=aes(x=Date,y=dv_m3s))+
  geom_line()+
  theme_classic(base_size=15)

# Calculate outflow
q2 <- q %>% filter(time>=as.Date('2013-01-02')&time<=as.Date('2018-12-31'))
dvol <- dvol %>% filter(Date>=as.Date('2013-01-02')&Date<=as.Date('2018-12-31'))

outflow <- as.data.frame(q2$q_total_cms-dvol$dv_m3s)
names(outflow)[1] <- "outflow_cms"

outflow <- cbind.data.frame(dvol,outflow)

outflow <- outflow %>% select(Date,outflow_cms)

ggplot(outflow,mapping=aes(x=Date,y=outflow_cms))+
  geom_line()+
  theme_classic(base_size=15)

# Calculate residence time using daily BVR volume and inflow
wrt <- (vol1$BVR_Vol_m3/q$q_total_cms)/(60*60*24)
wrt <- cbind.data.frame(q,wrt)
wrt <- wrt %>% select(time,wrt)
wrt <- wrt %>% mutate(wrt = replace(wrt, which(wrt == Inf), NA))
mean(wrt$wrt,na.rm=TRUE)
median(wrt$wrt,na.rm=TRUE)

ggplot(wrt,mapping=aes(x=time,y=wrt))+
  geom_line()+
  theme_classic(base_size=15)
