### Script to calculate BVR inflow using a the soil moisture method (from HWP; 'stormflow') and baseflow as estimated
### from discharge measurements during summer 2019
### Baseflow from B175 and B215 are scaled to watershed area as compared to B200
### For precent of watershed see: BVR_Watershed in Arc
### A Hounshell, 21 Apr 20

# Load packages
pacman::p_load(tidyverse,ggplot2,zoo)

# Load Q from soil moisture method (m3/d) (aka: 'stormflow')
storm <- read_csv("C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Data_Output/SoilMoisture_BVR_flow_calcs.csv")
storm$time <- as.POSIXct(strptime(storm$time, "%m/%d/%Y", tz = "EST"))

# Load in inflow data (2019 ONLY - eventually add 2020 data)
base <- read_csv("C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Raw_Data/2019_Continuum_Discharge.csv")
base$Date <- as.POSIXct(strptime(base$Date, "%m/%d/%Y", tz = "EST"))

# Select data from BVR
bvr_base <- base %>% filter(Reservoir == "BVR")

# Create a normal distribution for resampling based on all inflow data
baseflow <- bvr_base %>% select(Flow_cms)
mean(baseflow$Flow_cms)
sd(baseflow$Flow_cms)
hist(baseflow$Flow_cms)

### Use average and sd to generate a resampling from a random normal distribution
# B100
base_100 <- as.data.frame(rnorm(2191, mean=mean(baseflow$Flow_cms), sd=sd(baseflow$Flow_cms)))
names(base_100)[1] <- "base_100_m3s"

# Remove values < 0
base_100 <- base_100 %>% mutate(base_100_m3s = replace(base_100_m3s, which(base_100_m3s < 0), 0))
hist(base_100$base_100_m3s)

# B200
base_200 <- as.data.frame(rnorm(2191, mean=mean(baseflow$Flow_cms), sd=sd(baseflow$Flow_cms)))
names(base_200)[1] <- "base_200_m3s"

base_200 <- base_200 %>% mutate(base_200_m3s = replace(base_200_m3s, which(base_200_m3s < 0), 0))
hist(base_200$base_200_m3s)

# B215: same proportion as B200
base_215 <- as.data.frame(rnorm(2191, mean=mean(baseflow$Flow_cms), sd=sd(baseflow$Flow_cms)))
names(base_215)[1] <- "base_215_m3s"

base_215 <- base_215 %>% mutate(base_215_m3s = replace(base_215_m3s, which(base_215_m3s < 0), 0))
hist(base_215$base_215_m3s)

# B175: 71% watershed area as B200; scale inflow
base_175 <- as.data.frame(rnorm(2191, mean=mean(baseflow$Flow_cms), sd=sd(baseflow$Flow_cms)))
names(base_175)[1] <- "base_175_m3s"

base_175$base_175_m3s <- (0.71*base_175$base_175_m3s)
base_175 <- base_175 %>% mutate(base_175_m3s = replace(base_175_m3s, which(base_175_m3s < 0), 0))
hist(base_175$base_175_m3s)

# Convert all stormflow from m3/d to cms
storm_cms <- storm %>% select(Q_BVR_cms,Q_B100_cms,Q_B175_cms,Q_B200_cms,Q_B215_cms)
storm_cms <- storm_cms/(60*60*24)
date <- storm %>% select(time)
storm_cms <- cbind.data.frame(date,storm_cms)

# Calculate reservoir stormflow assuming outflow = summed inflow
storm_cms <- storm_cms %>% mutate(res_cms = Q_BVR_cms - (Q_B100_cms + Q_B175_cms + Q_B200_cms + Q_B215_cms))

# Plot stormflow
ggplot()+
  geom_line(storm_cms,mapping=aes(x=time,y=res_cms,color="Res"))+
  geom_line(storm_cms,mapping=aes(x=time,y=Q_B100_cms,color="100"))+
  geom_line(storm_cms,mapping=aes(x=time,y=Q_B175_cms,color="175"))+
  geom_line(storm_cms,mapping=aes(x=time,y=Q_B200_cms,color="200"))+
  geom_line(storm_cms,mapping=aes(x=time,y=Q_B215_cms,color="215"))+
  theme_classic(base_size=15)

# Calculate storm + baseflow for all inflows
storm_cms <- cbind.data.frame(storm_cms,base_100)
storm_cms <- cbind.data.frame(storm_cms,base_175)
storm_cms <- cbind.data.frame(storm_cms,base_200)
storm_cms <- cbind.data.frame(storm_cms,base_215)

storm_cms <- storm_cms %>% mutate(inflow_100 = Q_B100_cms + base_100_m3s)
storm_cms <- storm_cms %>% mutate(inflow_175 = Q_B175_cms + base_175_m3s)
storm_cms <- storm_cms %>% mutate(inflow_200 = Q_B200_cms + base_200_m3s)
storm_cms <- storm_cms %>% mutate(inflow_215 = Q_B215_cms + base_215_m3s)

flow_cms <- storm_cms %>% select(time,res_cms,inflow_100,inflow_175,inflow_200,inflow_215)
flow_cms <- flow_cms %>% mutate(total = inflow_100 + inflow_175 + inflow_200 + inflow_215)

# Plot flows
ggplot()+
  geom_line(flow_cms,mapping=aes(x=time,y=inflow_100,color="100"))+
  geom_line(flow_cms,mapping=aes(x=time,y=inflow_175,color="175"))+
  geom_line(flow_cms,mapping=aes(x=time,y=inflow_200,color="200"))+
  geom_line(flow_cms,mapping=aes(x=time,y=inflow_215,color="215"))+
  theme_classic(base_size=15)

ggplot()+
  geom_line(flow_cms,mapping=aes(x=time,y=total,color="total"))+
  theme_classic(base_size=15)

bvr_100 <- bvr_base %>% filter(Site=="100")

ggplot()+
  geom_line(flow_cms,mapping=aes(x=time,y=inflow_100,color="100"))+
  geom_point(bvr_100,mapping=aes(x=Date,y=Flow_cms,color="Q"))+
  xlim(as.POSIXct("2019-04-01"),as.POSIXct("2019-10-31"))+
  ylim(0,0.075)+
  theme_classic(base_size=15)

bvr_200 <- bvr_base %>% filter(Site=="200")

ggplot()+
  geom_line(flow_cms,mapping=aes(x=time,y=inflow_200,color="200"))+
  geom_point(bvr_200,mapping=aes(x=Date,y=Flow_cms,color="Q"))+
  theme_classic(base_size=15)

ggplot()+
  geom_line(flow_cms,mapping=aes(x=time,y=inflow_200,color="200"))+
  geom_point(bvr_200,mapping=aes(x=Date,y=Flow_cms,color="Q"))+
  xlim(as.POSIXct("2019-04-01"),as.POSIXct("2019-10-31"))+
  ylim(0,0.06)+
  theme_classic(base_size=15)

### Calculate WRT using total inflow and BVR water level data
vol <- read_csv("C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Data_Output/09Apr20_BVR_WaterLevelDailyVol.csv")
vol$Date <- as.POSIXct(strptime(vol$Date, "%m/%d/%Y", tz = "EST"))

# Select correct dates for volume
vol <- vol %>% filter(Date>=as.Date('2014-01-01')&Date<=as.Date('2019-12-06')) %>% select(Date,BVR_Vol_m3)

# Select correct dates for flow
flow_wrt <- flow_cms %>% filter(time>=as.Date('2014-01-01')&time<=as.Date('2019-12-06')) %>% select(time,total)

wrt <- (vol$BVR_Vol_m3/flow_wrt$total)/(60*60*24)
wrt <- cbind.data.frame(flow_wrt,wrt)
wrt <- wrt %>% select(time,wrt)

mean(wrt$wrt,na.rm=TRUE)
median(wrt$wrt,na.rm=TRUE)

ggplot(wrt,mapping=aes(x=time,y=wrt))+
  geom_line()+
  geom_hline(yintercept=393,color="blue")+
  geom_hline(yintercept=433,color="green")+
  theme_classic(base_size=15)
