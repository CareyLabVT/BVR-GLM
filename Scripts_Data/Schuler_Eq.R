### Script to use NLDAS (2013 - 2018) preciptation data + Schuler equation (Ward et al. 2020; Sunapee Metadata)
### to calculate daily run-off volume for BVR
## A Hounshell, 27 Mar 2020
## NOTE: Assuming precipitation is reported in mm
## Saved Rfile as: Est_Inflow

# Load packages
pacman::p_load(tidyverse,ggplot2,zoo)

# Load in NLDAS data: currently from 01 Jan 2013 to 31 Dec 2018 (will need to update at some point : )
nldas <- read_csv("C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Raw_Data/FCR_GLM_NLDAS_010113_123118_GMTadjusted.csv")

# Remove rows with NA values (original file skipped every-other row)
nldas2 <- nldas[complete.cases(nldas),]

# Convert datetime
nldas2$time <- as.POSIXct(strptime(nldas2$time, "%m/%d/%Y %H:%M", tz = "GMT"))

# Select precipitation to calculate daily values
precip <- nldas2 %>% select(time,Rain)
precip$time <- format(as.POSIXct(precip$time, '%Y-%m-%d'), format='%Y-%m-%d')
precip$time <- as.Date(precip$time)

# Sum hourly precip values to daily (mm/h to mm/d)
precip <- precip %>% group_by(time) %>% summarize_all(funs(sum)) %>% arrange(time)

# Use Schuler Equation to calculate daily run-off volume to BVR
# R = P * Pj * Rv where P = precipitation (mm/d), Pj = 0.9 (fraction of annual rainfall events that produce run-off), Rv = Runoff
# Coefficient (assuming no imprevious surface in BVR; Rv = 0.05)
# Converted to m/d (from mm/d) -> then multiply by total watershed surface area for BVR
precip <- precip %>% mutate(r = (Rain * 0.9 * 0.05)/1000)

# Multiply precipitation (m/d) by watershed area (m2) and convert from m3/d to m3/s: 
# separated by Inflow_200, Inflow_100, and Reservoir watershed area
# Watershed area calculated using USGS StreamStats then loading shapefile into ArcGis
# Inflow_200 = 1874400.788 m3; Inflow_100 = 808500 m3; Reservoir = 993500 m3
precip <- precip %>% mutate(Inflow200_m3 = (r * 1874400.788)/(24*60*60))
precip <- precip %>% mutate(Inflow100_m3 = (r * 808500)/(24*60*60))
precip <- precip %>% mutate(Reservoir_m3 = (r * 993500)/(24*60*60))

# Plot overland flow for each (aka: 'stormflow')
ggplot()+
  geom_line(precip,mapping=aes(x=time,y=Inflow200_m3,color="200"))+
  geom_line(precip,mapping=aes(x=time,y=Inflow100_m3,color="100"))+
  geom_line(precip,mapping=aes(x=time,y=Reservoir_m3,color="Reservoir"))+
  theme_classic(base_size=15)

# Load in inflow data (2019 ONLY - eventually add 2020 data)
baseflow <- read_csv("C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Raw_Data/2019_Continuum_Discharge.csv")
baseflow$Date <- as.POSIXct(strptime(baseflow$Date, "%m/%d/%Y", tz = "EST"))

# Select data from BVR
bvr_baseflow <- baseflow %>% filter(Reservoir == "BVR")

# Compare distrbutions of inflow at 100 vs. 200
ggplot(bvr_baseflow,aes(x=factor(Site),y=Flow_cms,color=factor(Site)))+
  geom_boxplot()+
  theme_classic(base_size=15)

# Plot 100 vs. 200
ggplot(bvr_baseflow,mapping=aes(x=Date,y=Flow_cms,color=factor(Site)))+
  geom_line()+
  geom_point()+
  theme_classic(base_size=15)

# Create a normal distribution for resampling based on all inflow data
flow <- bvr_baseflow %>% select(Flow_cms)
mean(flow$Flow_cms)
sd(flow$Flow_cms)
hist(flow$Flow_cms)

### Use average and sd to generate a resampling from a random normal distribution
q_100 <- as.data.frame(rnorm(2191, mean=mean(flow$Flow_cms), sd=sd(flow$Flow_cms)))
names(q_100)[1] <- "q_100_m3s"

q_100 <- q_100 %>% mutate(q_100_m3s = replace(q_100_m3s, which(q_100_m3s < 0), 0))
hist(q_100$q_100_m3s)

q_200 <- as.data.frame(rnorm(2191, mean=mean(flow$Flow_cms), sd=sd(flow$Flow_cms)))
names(q_200)[1] <- "q_200_m3s"

q_200 <- q_200 %>% mutate(q_200_m3s = replace(q_200_m3s, which(q_200_m3s < 0), 0))
hist(q_200$q_200_m3s)

discharge <- cbind.data.frame(precip,q_100)
discharge <- cbind.data.frame(discharge,q_200)

discharge <- discharge %>% mutate(q100_total = Inflow100_m3 + q_100_m3s)
discharge <- discharge %>% mutate(q200_total = Inflow200_m3 + q_200_m3s)
discharge <- discharge %>% mutate(q_total = Reservoir_m3 + q100_total + q200_total)

# Plot
ggplot()+
  geom_line(discharge,mapping=aes(x=time,y=q100_total,color="100"))+
  geom_line(discharge,mapping=aes(x=time,y=q200_total,color="200"))+
  geom_line(discharge,mapping=aes(x=time,y=Reservoir_m3,color="Reservoir"))+
  geom_line(discharge,mapping=aes(x=time,y=q_total,color="Total"))+
  theme_classic(base_size=15)

# Export out calculated discharge
q_out <- discharge %>% select(time,Reservoir_m3,q100_total,q200_total,q_total)
names(q_out)[2] <- "res_cms"
names(q_out)[3] <- "q100_total_cms"
names(q_out)[4] <- "q200_total_cms"
names(q_out)[5] <- "q_total_cms"

write_csv(q_out, path = "C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Data_Output/q_calc.csv")

### Old code using same distribution as the collected data

# Random dates
dates <- as.data.frame(sample(seq(as.Date('2013-01-01'), as.Date('2018-12-31'), by="day"), 313))
baseflow_100 <- sample(flow$Flow_cms,size=313,replace=TRUE)
baseflow_200 <- sample(flow$Flow_cms,size=313,replace=TRUE)

dates <- cbind.data.frame(dates,baseflow_100)
dates <- cbind.data.frame(dates,baseflow_200)
names(dates)[1] <- "time"

precip <- merge(precip,dates,by="time",all.x=TRUE)
precip$baseflow_100[1] <- sample(flow$Flow_cms,size=1)
precip$baseflow_200[1] <- sample(flow$Flow_cms,size=1)
precip$baseflow_100[2191] <- sample(flow$Flow_cms,size=1)
precip$baseflow_200[2191] <- sample(flow$Flow_cms,size=1)

# Lineraly extrapolate sampled baseflow
precip$baseflow_100 <- na.approx(precip$baseflow_100)
precip$baseflow_200 <- na.approx(precip$baseflow_200)

# Plot approximated baseflow
ggplot()+
  geom_line(precip,mapping=aes(x=time,y=baseflow_100,color="100"))+
  geom_line(precip,mapping=aes(x=time,y=baseflow_200,color="200"))+
  theme_classic(base_size=15)

precip <- precip %>% mutate(total_200 = baseflow_200 + Inflow200_m3)
precip <- precip %>% mutate(total_100 = baseflow_100 + Inflow100_m3)
precip <- precip %>% mutate(total_flow = total_200 + total_100 + Reservoir_m3)

# Plot simulated flows (200 and 100)
ggplot()+
  geom_line(precip,mapping=aes(x=time,y=total_200,color="200"))+
  geom_line(precip,mapping=aes(x=time,y=total_100,color="100"))+
  geom_line(precip,mapping=aes(x=time,y=Reservoir_m3,color="Reservoir"))+
  geom_line(precip,mapping=aes(x=time,y=total_flow,color="Total"))+
  theme_classic(base_size=15)

ggplot(precip,mapping=aes(x=time,y=total_flow))+
  geom_line()+
  theme_classic(base_size=15)