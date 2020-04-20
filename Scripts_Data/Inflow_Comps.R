### Script to compare BVR inflow as calculated using the Schuler + Baseflow method and the soil mositure method (from HWP)
### A Hounshell, 20 Apr 20

# Load packages
pacman::p_load(tidyverse,ggplot2,zoo)

# Load Q from Schuler + Baseflow method (cms)
flow <- read_csv("C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Data_Output/q_calc.csv")
flow$time <- as.POSIXct(strptime(flow$time, "%Y-%m-%d", tz = "EST"))

# Load Q from soil moisture method (m3/d)
soil <- read_csv("C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Data_Output/SoilMoisture_BVR_flow_calcs.csv")
soil$time <- as.POSIXct(strptime(soil$time, "%m/%d/%Y", tz = "EST"))

# Convert 100 to cms
soil <- soil %>% mutate(q100_cms = Q_B100_cms/(60*24*60))
soil <- soil %>% mutate(q200_cms = (Q_B200_cms + Q_B175_cms + Q_B215_cms)/(60*60*24))
soil <- soil %>% mutate(qtotal_cms = (Q_B200_cms + Q_B175_cms + Q_B215_cms + Q_B100_cms)/(60*60*24))

# Load in measured discharge from 2019
# Load in inflow data (2019 ONLY - eventually add 2020 data)
baseflow <- read_csv("C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Raw_Data/2019_Continuum_Discharge.csv")
baseflow$Date <- as.POSIXct(strptime(baseflow$Date, "%m/%d/%Y", tz = "EST"))

# Select data from BVR
bvr_100 <- baseflow %>% filter(Reservoir == "BVR" & Site == "100")
bvr_200 <- baseflow %>% filter(Reservoir == "BVR" & Site == "200")

# Plot 100
ggplot()+
  geom_line(flow,mapping=aes(x=time,y=q100_total_cms,color="Schuler"))+
  geom_line(soil,mapping=aes(x=time,y=q100_cms,color="Soil"))+
  geom_point(bvr_100,mapping=aes(x=Date,y=Flow_cms,color="Q"))+
  theme_classic(base_size=15)

ggplot()+
  geom_line(flow,mapping=aes(x=time,y=q100_total_cms,color="Schuler"))+
  geom_line(soil,mapping=aes(x=time,y=q100_cms,color="Soil"))+
  geom_point(bvr_100,mapping=aes(x=Date,y=Flow_cms,color="Q"))+
  xlim(as.POSIXct("2019-04-01"),as.POSIXct("2019-10-31"))+
  ylim(0,0.075)+
  theme_classic(base_size=15)

# Plot 200
ggplot()+
  geom_line(flow,mapping=aes(x=time,y=q200_total_cms,color="Schuler"))+
  geom_line(soil,mapping=aes(x=time,y=q200_cms,color="Soil"))+
  geom_point(bvr_200,mapping=aes(x=Date,y=Flow_cms,color="Q"))+
  theme_classic(base_size=15)

ggplot()+
  geom_line(flow,mapping=aes(x=time,y=q200_total_cms,color="Schuler"))+
  geom_line(soil,mapping=aes(x=time,y=q200_cms,color="Soil"))+
  geom_point(bvr_200,mapping=aes(x=Date,y=Flow_cms,color="Q"))+
  xlim(as.POSIXct("2019-04-01"),as.POSIXct("2019-10-31"))+
  ylim(0,0.15)+
  theme_classic(base_size=15)

# Calculate outflow and wrt for soil moisture method
vol <- read_csv("C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Data_Output/09Apr20_BVR_WaterLevelDailyVol.csv")
vol$Date <- as.POSIXct(strptime(vol$Date, "%m/%d/%Y", tz = "EST"))

# Select dates for Initial volume
vol1 <- vol %>% filter(Date>=as.Date('2014-01-01')&Date<=as.Date('2019-12-05')) %>% select(Date,BVR_Vol_m3)

# Select dates for Final volume
vol2 <- vol %>% filter(Date>=as.Date('2014-01-02')&Date<=as.Date('2019-12-06')) %>% select(Date,BVR_Vol_m3)

dvol <- vol %>% filter(Date>=as.Date('2014-01-02')&Date<=as.Date('2019-12-06')) %>% select(Date)

# Calculate dVol/dt by vol2-vol1/s
vol3 <- as.data.frame((vol2$BVR_Vol_m3 - vol1$BVR_Vol_m3)/(24*60*60))
names(vol3)[1] <- "dv_m3s"

dvol <- cbind.data.frame(dvol,vol3)

soil2 <- soil %>% filter(time>=as.Date('2014-01-02')&time<=as.Date('2019-12-06')) %>% select(time,qtotal_cms)

outflow <- as.data.frame(soil2$qtotal_cms-dvol$dv_m3s)
names(outflow)[1] <- "outflow_cms"

outflow <- cbind.data.frame(dvol,outflow)

outflow <- outflow %>% select(Date,outflow_cms)

ggplot(outflow,mapping=aes(x=Date,y=outflow_cms))+
  geom_line()+
  theme_classic(base_size=15)

# Calculate residence time using daily BVR volume and inflow
wrt <- (vol2$BVR_Vol_m3/soil2$qtotal_cms)/(60*60*24)
wrt <- cbind.data.frame(soil2,wrt)
wrt <- wrt %>% select(time,wrt)
wrt <- wrt %>% mutate(wrt = replace(wrt, which(wrt == Inf), NA))
mean(wrt$wrt,na.rm=TRUE)
median(wrt$wrt,na.rm=TRUE)

ggplot(wrt,mapping=aes(x=time,y=wrt))+
  geom_line()+
  theme_classic(base_size=15)