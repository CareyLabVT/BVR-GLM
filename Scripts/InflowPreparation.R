#originally written by CCC on 16 July 2018 to create weir and wetland inflow files + outflow for FCR GLM model
#updated 1 June 2020 to be made "tidy" and update nutrient fractions for inflows
# Updated for BVR GLM inflow files: A Hounshell, 18 Jun 2020
# Updated 08 July 2020 per FCR inflow file: Added DONR and DOPR fractions for inflow prep
# Updated 17 Sep 2020 to include updated inflow model using NLDAS data
# Updated 23 Feb 2021 to include new inflow file (BVR_flow_calcs_new)

wd <- getwd()
setwd(wd)
sim_folder <- getwd()

#load packages
pacman::p_load(dplyr,zoo,EcoHydRology,rMR,tidyverse,lubridate)
#library(dplyr)
#library(zoo)
#library(EcoHydRology)
#library(rMR)
#library(tidyverse)
#library(lubridate)

# First, read in inflow file generated from Thronthwaite Overland flow model + groundwater recharge
# From HW: for entire watershed (?); units in m3/s
# Updated inflow model using NLDAS precip and temp data: units in m3/d - need to convert to m3/s
inflow <- read_csv("./inputs/BVR_flow_calcs_new.csv")
inflow$time = as.POSIXct(strptime(inflow$time,"%Y-%m-%d", tz="EST"))
inflow <- inflow[,-c(1)]
names(inflow)[2] <- "FLOW"
inflow$FLOW = inflow$FLOW/86400
inflow <- inflow %>% 
  filter(time >= "2014-01-01")
 
#diagnostic plot
plot(inflow$time, inflow$FLOW)

# Need to append water temperature to inflow file (as TEMP)
# Based on comparisons btw BVR inflow (100,200) temp from RC days and FCR inflow (100) (see BVR_Inflow_Temp.R),
# going to assume temperature measured at FCR 100 is close to BVR inflow temp

# Download FCR inflow data from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/202/7/f5fa5de4b49bae8373f6e7c1773b026e" 
infile1 <- paste0(getwd(),"/inflow_for_EDI_2013_06Mar2020.csv")
download.file(inUrl1,infile1,method="curl")

temp <- read_csv("./inflow_for_EDI_2013_06Mar2020.csv")
temp$DateTime = as.POSIXct(strptime(temp$DateTime,"%Y-%m-%d", tz="EST"))
temp <- temp %>% select(DateTime, WVWA_Temp_C) %>% 
  rename(time=DateTime, TEMP=WVWA_Temp_C) %>%
  dplyr::filter(time > as.POSIXct("2013-12-31") & time < as.POSIXct("2020-01-01")) %>% 
  group_by(time) %>% 
  summarise(TEMP=mean(TEMP)) #gives averaged daily temp in C

# Merge inflow and inflow temp datasets
inflow <- merge(inflow,temp,by="time",all=TRUE)
inflow <- inflow %>% mutate(TEMP=na.fill(na.approx(TEMP),"extend"))

# Add SALT column (salinty = 0 for all time points)
inflow <- inflow %>% mutate(SALT = rep(0,length(inflow$time)))

#some diagnostic plots of inflow
plot(inflow$time, inflow$FLOW, type = "o")
plot(inflow$time, inflow$TEMP, type = "l", col = "red")

#now let's merge with chemistry
#first pull in BVR chem data from 2013-2019 from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/199/6/2b3dc84ae6b12d10bd5485f1c300af13" 
infile1 <- paste0(getwd(),"/chem.csv")
download.file(inUrl1,infile1,method="curl")

BVRchem <- read.csv("./inputs/chem.csv", header=T) %>%
  select(Reservoir:DIC_mgL) %>%
  dplyr::filter(Reservoir=="BVR") %>%
  dplyr::filter(Site==100 | Site==200) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  rename(time = DateTime)

# Diagnostic plots of BVR 100 vs. 200 for chemistry data
ggplot(BVRchem,mapping=aes(x=factor(Site),y=TN_ugL,color=factor(Site)))+
  geom_boxplot()+
  theme_classic(base_size=15)

ggplot(BVRchem,mapping=aes(x=factor(Site),y=TP_ugL,color=factor(Site)))+
  geom_boxplot()+
  theme_classic(base_size=15)

ggplot(BVRchem,mapping=aes(x=factor(Site),y=NH4_ugL,color=factor(Site)))+
  geom_boxplot()+
  theme_classic(base_size=15)

ggplot(BVRchem,mapping=aes(x=factor(Site),y=NO3NO2_ugL,color=factor(Site)))+
  geom_boxplot()+
  theme_classic(base_size=15)

ggplot(BVRchem,mapping=aes(x=factor(Site),y=SRP_ugL,color=factor(Site)))+
  geom_boxplot()+
  theme_classic(base_size=15)

ggplot(BVRchem,mapping=aes(x=factor(Site),y=DOC_mgL,color=factor(Site)))+
  geom_boxplot()+
  theme_classic(base_size=15)

ggplot(BVRchem,mapping=aes(x=factor(Site),y=DIC_mgL,color=factor(Site)))+
  geom_boxplot()+
  theme_classic(base_size=15)

# Look at combined distributions
hist(BVRchem$TN_ugL)
hist(BVRchem$TP_ugL)
hist(BVRchem$NH4_ugL)
hist(BVRchem$NO3NO2_ugL)
hist(BVRchem$SRP_ugL)
hist(BVRchem$DOC_mgL)
hist(BVRchem$DIC_mgL)

# Create nuts (randomly sampled from a normal distribution) for total inflow
bvr_nuts <- as.data.frame(seq.Date(as.Date("2014/01/01"),as.Date("2019/12/31"), "days"))
names(bvr_nuts)[1] <- "time"
bvr_nuts$time<-as.POSIXct(strptime(bvr_nuts$time, "%Y-%m-%d", tz="EST"))
bvr_nuts <- bvr_nuts %>% 
  mutate(TN_ugL = rnorm(2191,mean=mean(BVRchem$TN_ugL,sd=sd(BVRchem$TN_ugL)))) %>% 
  mutate(TP_ugL = rnorm(2191,mean=mean(BVRchem$TP_ugL),sd=sd(BVRchem$TP_ugL))) %>% 
  mutate(NH4_ugL = rnorm(2191,mean=mean(BVRchem$NH4_ugL,sd=sd(BVRchem$NH4_ugL)))) %>% 
  mutate(NO3NO2_ugL = rnorm(2191,mean=mean(BVRchem$NO3NO2_ugL,sd=sd(BVRchem$NO3NO2_ugL)))) %>% 
  mutate(SRP_ugL = rnorm(2191,mean=mean(BVRchem$SRP_ugL,sd=sd(BVRchem$SRP_ugL)))) %>% 
  mutate(DOC_mgL = rnorm(2191,mean=mean(BVRchem$DOC_mgL,sd=sd(BVRchem$DOC_mgL)))) %>% 
  mutate(DIC_mgL = rnorm(2191,mean=mean(BVRchem$DIC_mgL,sd=sd(BVRchem$DIC_mgL))))

#read in lab dataset of dissolved silica, measured by Jon in summer 2014 only
silica <- read.csv("./inputs/FCR2014_Chemistry.csv", header=T) %>%
  select(Date, Depth, DRSI_mgL) %>%
  mutate(Date = as.POSIXct(strptime(Date, "%Y-%m-%d", tz="EST"))) %>%
  dplyr::filter(Depth == 999) %>% #999 = weir inflow site
  select(Date, DRSI_mgL) %>%
  rename(time = Date)
  
#diagnostic plot of silica
plot(silica$time, silica$DRSI_mgL)
hist(silica$DRSI_mgL)
median(silica$DRSI_mgL) #this median concentration is going to be used to set as the constant Si inflow conc in both wetland & weir inflows

alldata<-merge(inflow, bvr_nuts, by="time", all.x=TRUE)

#read in lab dataset of CH4 from 2015-2019
# for BVR: Only have a handful of days w/ CH4 in inflows (BVR 100 and 200); aggregate all time points
# and average CH4 - use average as CH4 input for the entier year
ghg <- read.csv("./inputs/BVR_GHG_Inflow_20200619.csv", header=T) %>%
  dplyr::filter(Reservoir == "BVR") %>%
  dplyr::filter(Depth_m == 100|Depth_m == 200) %>% #weir inflow
  select(DateTime, ch4_umolL) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%d-%b-%y", tz="EST"))) %>%
  rename(time = DateTime, CAR_ch4 = ch4_umolL)

# Calculate average for the BVR data points and mutate column to alldata
alldata <- alldata %>% mutate(CAR_ch4 = mean(ghg$CAR_ch4))

#some other cool long-term plots
plot(alldata$time, alldata$SRP_ugL)
plot(alldata$time, alldata$DOC_mgL)
plot(alldata$time, alldata$NO3NO2_ugL)
plot(alldata$time, alldata$NH4_ugL)
plot(alldata$time, alldata$TN_ugL)
plot(alldata$time, alldata$TP_ugL)
plot(alldata$time, alldata$DIC_mgL)

#need to convert mass observed data into mmol/m3 units for two pools of organic carbon
total_inflow <- alldata %>% 
  mutate(NIT_amm = NH4_ugL*1000*0.001*(1/18.04)) %>% 
  mutate(NIT_nit = NO3NO2_ugL*1000*0.001*(1/62.00)) %>% #as all NO2 is converted to NO3
  mutate(PHS_frp = SRP_ugL*1000*0.001*(1/94.9714)) %>% 
  mutate(OGM_doc = DOC_mgL*1000*(1/12.01)* 0.10) %>% #assuming 10% of total DOC is in labile DOC pool (Wetzel page 753)
  mutate(OGM_docr = DOC_mgL*1000*(1/12.01)* 0.90) %>% #assuming 90% of total DOC is in labile DOC pool
  mutate(TN_ugL = TN_ugL*1000*0.001*(1/14)) %>% 
  mutate(TP_ugL = TP_ugL*1000*0.001*(1/30.97)) %>% 
  mutate(OGM_poc = 0.1*(OGM_doc+OGM_docr)) %>% #assuming that 10% of DOC is POC (Wetzel page 755)
  mutate(OGM_don = (5/6)*(TN_ugL-(NIT_amm+NIT_nit))*0.10) %>% #DON is ~5x greater than PON (Wetzel page 220)
  mutate(OGM_donr = (5/6)*(TN_ugL-(NIT_amm+NIT_nit))*0.90) %>% #to keep mass balance with DOC, DONr is 90% of total DON
  mutate(OGM_pon = (1/6)*(TN_ugL-(NIT_amm+NIT_nit))) %>%
  mutate(OGM_dop = 0.3*(TP_ugL-PHS_frp)*0.10) %>% #Wetzel page 241, 70% of total organic P = particulate organic; 30% = dissolved organic P
  mutate(OGM_dopr = 0.3*(TP_ugL-PHS_frp)*0.90) %>% #Wetzel page 241, 70% of total organic P = particulate organic; 30% = dissolved organic P
  mutate(OGM_pop = 0.7*(TP_ugL-PHS_frp)) %>% 
  #mutate(PHS_frp_ads = PHS_frp) %>% #Following Farrell et al. 2020 EcolMod
  mutate(CAR_dic = DIC_mgL*1000*(1/52.515)) #Long-term avg pH of FCR is 6.5, at which point CO2/HCO3 is about 50-50
#given this disparity, using a 50-50 weighted molecular weight (44.01 g/mol and 61.02 g/mol, respectively)
#note: we are not using a DONr recalcitrant pool for inflows because "bacterial utilization of these 
#compounds [i.e. DON] is extremely rapid" Wetzel p. 220
#because we have added the pool of PHS_frp_ads, which functionally is DOPr, not adding a DOPr pool
  
#reality check of mass balance: these histograms should be at zero minus rounding errors
hist(total_inflow$TP_ugL - (total_inflow$PHS_frp + total_inflow$OGM_dop + total_inflow$OGM_pop))
hist(total_inflow$TN_ugL - (total_inflow$NIT_amm + total_inflow$NIT_nit + total_inflow$OGM_don + total_inflow$OGM_pon))

#creating OXY_oxy column using RMR package, assuming that oxygen is at 100% saturation in this very well-mixed stream
# Obtained elevation from BVR DEM at BVR 100 inflow to the reservoir
for(i in 1:length(total_inflow$TEMP)){
  total_inflow$OXY_oxy[i]<-(temp.C= Eq.Ox.conc(total_inflow$TEMP[i], elevation.m = 586,
                                  bar.press = NULL, bar.units = NULL,
                                  out.DO.meas = "mg/L",
                                  salinity = 0, salinity.units = "pp.thou"))*1000*(1/32)
}

#clean it up and get vars in order
total_inflow <- total_inflow %>%
  select(time, FLOW, TEMP, SALT, OXY_oxy, NIT_amm:CAR_dic, CAR_ch4) %>% 
  mutate(SIL_rsi = rep(median(silica$DRSI_mgL),length(total_inflow$time))) %>%
  mutate(SIL_rsi = SIL_rsi*1000*(1/60.08)) %>% #setting the Silica concentration to the median 2014 inflow concentration for consistency
  mutate_if(is.numeric, round, 4) #round to 4 digits 

#write file for inflow for the weir, with 2 pools of OC (DOC + DOCR)  
write.csv(total_inflow, "BVR_inflow_2014_2019_20210223_allfractions_2poolsDOC_withch4_nldasInflow.csv", row.names = F)

#copying dataframe in workspace to be used later
alltdata = alldata

########SKIP THIS STEP IF YOU WANT TO USE 2 POOLS OF OC! 
#This is making the weir inflow with only *1* pool of OC
#need to convert mass observed data into mmol/m3 units for ONE pool of organic carbon
weir_inflow <- alldata %>% 
  mutate(NIT_amm = NH4_ugL*1000*0.001*(1/18.04)) %>% 
  mutate(NIT_nit = NO3NO2_ugL*1000*0.001*(1/62.00)) %>% #as all NO2 is converted to NO3
  mutate(PHS_frp = SRP_ugL*1000*0.001*(1/94.9714)) %>% 
  mutate(OGM_doc = DOC_mgL*1000*(1/12.01)) %>% 
  mutate(TN_ugL = TN_ugL*1000*0.001*(1/14)) %>% 
  mutate(TP_ugL = TP_ugL*1000*0.001*(1/30.97)) %>% 
  mutate(OGM_poc = 0.1*(OGM_doc)) %>% #assuming that 10% of DOC is POC (Wetzel page 755)
  mutate(OGM_don = (5/6)*(TN_ugL-(NIT_amm+NIT_nit))) %>% #DON is ~5x greater than PON (Wetzel page 220)
  mutate(OGM_pon = (1/6)*(TN_ugL-(NIT_amm+NIT_nit))) %>%
  mutate(OGM_dop = 0.3*(TP_ugL-PHS_frp)) %>% #Wetzel page 241, 70% of total organic P = particulate organic; 30% = dissolved organic P
  mutate(OGM_pop = 0.7*(TP_ugL-PHS_frp)) %>% 
  mutate(PHS_frp_ads = PHS_frp) %>% #Following Farrell et al. 2020 EcolMod
  mutate(CAR_dic = DIC_mgL*1000*(1/52.515)) #Long-term avg pH of FCR is 6.5, at which point CO2/HCO3 is about 50-50
#given this disparity, using a 50-50 weighted molecular weight (44.01 g/mol and 61.02 g/mol, respectively)

#reality check of mass balance: these histograms should be at zero minus rounding errors
hist(weir_inflow$TP_ugL - (weir_inflow$PHS_frp + weir_inflow$OGM_dop + weir_inflow$OGM_pop))
hist(weir_inflow$TN_ugL - (weir_inflow$NIT_amm + weir_inflow$NIT_nit + weir_inflow$OGM_don + weir_inflow$OGM_pon))

#creating OXY_oxy column using RMR package, assuming that oxygen is at 100% saturation in this very well-mixed stream
for(i in 1:length(weir_inflow$TEMP)){
  weir_inflow$OXY_oxy[i]<-(temp.C= Eq.Ox.conc(weir_inflow$TEMP[i], elevation.m = 506,
                                              bar.press = NULL, bar.units = NULL,
                                              out.DO.meas = "mg/L",
                                              salinity = 0, salinity.units = "pp.thou"))*1000*(1/32)
}

weir_inflow <- weir_inflow %>%
  select(time, FLOW, TEMP, SALT, OXY_oxy, NIT_amm:CAR_dic, CAR_ch4) %>% 
  mutate(SIL_rsi = rep(median(silica$DRSI_mgL),length(weir_inflow$time))) %>%
  mutate(SIL_rsi = SIL_rsi*1000*(1/60.08)) %>% #setting the Silica concentration to the median 2014 inflow concentration for consistency
  mutate_if(is.numeric, round, 4) #round to 4 digits 

write.csv(weir_inflow, "FCR_weir_inflow_2013_2019_20200607_allfractions_1poolDOC.csv", row.names = F)

##############################################################
##############################################################

#REGARDLESS OF YOUR OC POOLS, WILL NEED TO MAKE OUTFLOW!
# For BVR, need to take into account changing water level as well!
# Used script originally developed in BVR_outflow.R
# Note: Last recorded day of water level was 12-6-19 - assumed water level was the same through 12-31-19

# Load in water level + volume data for BVR
# Calculated using WVWA + Carey Lab BVR water level observations and joined DEM + 2018 Bathymetry survey
# See: BVR_Volume script for Matlab
vol <- read_csv("./Data_Output/09Apr20_BVR_WaterLevelDailyVol.csv")
vol$Date <- as.POSIXct(strptime(vol$Date, "%m/%d/%Y", tz = "EST"))

vol1 <- vol %>% filter(Date>=as.Date('2013-12-31')&Date<=as.Date('2019-12-30')) %>% select(Date,BVR_Vol_m3)
vol2 <- vol %>% filter(Date>=as.Date('2014-01-01')&Date<=as.Date('2019-12-31')) %>% select(Date,BVR_Vol_m3)

dvol <- vol %>% filter(Date>=as.Date('2014-01-01')&Date<=as.Date('2019-12-31')) %>% select(Date)

# Calculate dVol/dt by vol2-vol1/s
vol3 <- as.data.frame((vol2$BVR_Vol_m3 - vol1$BVR_Vol_m3)/(24*60*60))
names(vol3)[1] <- "dv_m3s"

dvol <- cbind.data.frame(dvol,vol3)

# Check change in water level
ggplot(dvol,mapping=aes(x=Date,y=dv_m3s))+
  geom_line()

# Calculate outflow as the total inflow - change in water level
outflow <- as.data.frame(alldata$FLOW-dvol$dv_m3s)
names(outflow)[1] <- "FLOW"
outflow <- cbind.data.frame(dvol,outflow)
outflow <- outflow %>% select(Date,FLOW) %>% 
  mutate_if(is.numeric,round,4) #round to 4 digits
names(outflow)[1] <- "time"

#outflow <- outflow %>% filter(time<as.POSIXct("2014-08-19"))
#stream_flow <- inflow %>% filter(time>as.POSIXct("2014-08-19")) %>% select(time,FLOW)
#outflow <- rbind.data.frame(outflow,stream_flow)

#diagnostic plot
ggplot()+
  geom_line(outflow,mapping=aes(x=time,y=FLOW,color="Outflow"))+
  geom_line(inflow,mapping=aes(x=time,y=FLOW,color="Inflow"))+
  theme_classic(base_size=15)

#write file
write.csv(outflow, "BVR_spillway_outflow_2014_2019_20210223_nldasInflow.csv", row.names=F)
  