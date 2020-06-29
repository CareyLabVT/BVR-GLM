#Use this script to create field data files for the pertinent BVR water quality variables at
#Station 50
#written by CCC originally on 16 July 2018
#updated and cleaned up on 2 June 2020
# Adapted to BVR: A Hounshell, 25 June 2020

setwd("C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/field_data")

library(tidyverse)
library(lubridate)
library(zoo)

#focal depths we are trying to compare modeled data vs observations; for CTD/YSI casts
#assumed the deepest point of BVR = 11 m
depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) 

###########################################################
######TEMPERATURE, DO, AND CHLA FROM CTD

#need to import CTD observations from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/200/10/2461524a7da8f1906bfc3806d594f94c" 
infile1 <- paste0(getwd(),"/CTD_final_2013_2019.csv")
download.file(inUrl1,infile1,method="curl")

#read in CTD temp file from EDI to create field file, but first need to subset CTD data per each day to depths
ctd<-read_csv('CTD_final_2013_2019.csv') %>% #read in observed CTD data, which has multiple casts on the same day (problematic for comparison)
  filter(Reservoir=="BVR") %>%
  filter(Site==50) %>%
  rename(time=Date, depth=Depth_m, temp=Temp_C, DO=DO_mgL, chla = Chla_ugL) %>%
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
  select(time, depth, temp, DO, chla) %>%
  na.omit() 

# Import YSI observations from EDI
inUrl1 <- "https://pasta.lternet.edu/package/data/eml/edi/198/7/25b5e8b7f4291614d5c6d959a08148d8"
infile1 <- paste0(getwd(),"/YSI_PAR_profiles_2013-2019.csv")
download.file(inUrl1,infile1,method="curl")

ysi <- read_csv('YSI_PAR_profiles_2013-2019.csv') %>% 
  filter(Reservoir=="BVR") %>% 
  filter(Site==50) %>% 
  rename(time=DateTime,depth=Depth_m,temp=Temp_C,DO=DO_mgL) %>% 
  mutate(time = as.POSIXct(strptime(time, "%m/%d/%y",tz='EST'))) %>% 
  select(time,depth,temp,DO) %>% 
  na.omit()

# Select unique dates from both CTD and YSI casts
ysi_date_list <- as.data.frame(unique(as.Date(ysi$time)))
names(ysi_date_list)[1] <- "time"
ysi_date_list$ysi_bvr <- rep(-99,length(ysi_date_list$time))

ctd_date_list <- as.data.frame(unique(as.Date(ctd$time)))
names(ctd_date_list)[1] <- "time"
ctd_date_list$ctd_bvr <- rep(-99,length(ctd_date_list$time))

# Combine Unique dates list by date
bvr_dates <- merge(ysi_date_list, ctd_date_list, by="time", all.x=TRUE, all.y=TRUE)

### Merge data CTD and YSI datasets for BVR
bvr_merge <- merge(ctd, ysi, by="time", all.x=TRUE, all.y=TRUE)

# Find where there are Na values in the CTD data: need to do it for each column
ctd_bvr_na <- is.na(bvr_merge$depth.x)
bvr_merge$depth.x[ctd_bvr_na] <- bvr_merge$depth.y[ctd_bvr_na]

ctd_bvr_na <- is.na(bvr_merge$temp.x)
bvr_merge$temp.x[ctd_bvr_na] <- bvr_merge$temp.y[ctd_bvr_na]

ctd_bvr_na <- is.na(bvr_merge$DO.x)
bvr_merge$DO.x[ctd_bvr_na] <- bvr_merge$DO.y[ctd_bvr_na]

bvr_all <- bvr_merge %>% select(time,depth.x,temp.x,DO.x,chla) %>% 
  rename(depth=depth.x,temp=temp.x,DO=DO.x)

bvr_date_list <- as.data.frame(unique(as.Date(bvr_all$time)))

## Average across date and depth
bvr_all <- bvr_all %>% group_by(time,depth) %>% summarize_all(funs(mean))

#Initialize an empty matrix with the correct number of rows and columns 
temp<-matrix(data=NA, ncol=ncol(bvr_all), nrow=length(depths)) #of cols in CTD data, and then nrows = # of layers produced
super_final<-matrix(data=NA, ncol=1, nrow=0)
dates<-unique(bvr_all$time)

#create a function to chose the matching depth closest to our focal depths
closest<-function(xv, sv){
  xv[which.min(abs(xv-sv))]}

library(plyr) #only use plyr for this for loop, then detach!

#For loop to retrieve CTD depth with the closest function and fill in matrix
for (i in 1:length(dates)){
  j=dates[i]
  q <- subset(bvr_all, bvr_all$time == j)

  layer1 <- q[q[, "depth"] == closest(q$depth,0.1),][1,]
  layer2<- q[q[, "depth"] == closest(q$depth,1.0),][1,]
  layer3<- q[q[, "depth"] == closest(q$depth,2),][1,]
  layer4<- q[q[, "depth"] == closest(q$depth,3),][1,]
  layer5<- q[q[, "depth"] == closest(q$depth,4),][1,]
  layer6<- q[q[, "depth"] == closest(q$depth,5),][1,]
  layer7<- q[q[, "depth"] == closest(q$depth,6),][1,]
  layer8<- q[q[, "depth"] == closest(q$depth,7),][1,]
  layer9<- q[q[, "depth"] == closest(q$depth,8),][1,]
  layer10<- q[q[, "depth"] == closest(q$depth,9),][1,]
  layer11<- q[q[, "depth"] == closest(q$depth,10),][1,]
  layer12<- q[q[, "depth"] == closest(q$depth,11),][1,]
  
  temp<-rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,layer11,layer12)
  temp[,((ncol(bvr_all))+1)] <- depths
  colnames(temp)[((ncol(bvr_all))+1)]<-"new_depth"
  final <- temp
  final <- data.frame(final)
  super_final <- rbind.fill.matrix(super_final,final)
}

detach(package:plyr)#to prevent issues with dplyr vs plyr not playing well together!

#now need to clean up the data frame and make all factors numeric
super_final_2 <- as.data.frame(super_final) %>%
  select(time, new_depth, temp, DO, chla) %>%
  rename(depth = new_depth) %>%
  mutate(time = as.POSIXct(strptime(time, "%Y-%m-%d", tz="EST"))) %>%
  filter(time > as.Date("2014-01-01")) #need to make sure that the CTD data only start after first day of sim

# Check data!
plot(super_final_2$time,super_final_2$temp)
plot(super_final_2$time,super_final_2$DO)
plot(super_final_2$time,super_final_2$chla)

#export CTD data!
temp <- super_final_2 %>%
  select(time, depth, temp) %>%
  rename(DateTime = time, Depth = depth) %>%
  write.csv("CleanedObsTemp.csv", row.names = F)

oxygen <- super_final_2 %>%
  select(time, depth, DO)
oxygen$DO <- as.numeric(oxygen$DO)
oxygen <- oxygen %>%  
  mutate(DO = DO*1000/32) %>% #to convert mg/L to molar units
  rename(DateTime = time, Depth = depth, OXY_oxy=DO) %>%
  write.csv("CleanedObsOxy.csv", row.names = F)

chla <- super_final_2 %>% select(time, depth, chla)
chla <- na.omit(chla)
chla <- chla %>% 
  rename(DateTime = time, Depth = depth, PHY_TCHLA=chla) %>%
  write.csv("CleanedObsChla.csv", row.names = F)

## Find 'winter' depth profiles to use for initial model conditions
super_final_2$depth <- as.numeric(super_final_2$depth)
super_final_2$temp <- as.numeric(super_final_2$temp)
super_final_2$DO <- as.numeric(super_final_2$DO)
super_final_2$chla <- as.numeric(super_final_2$chla)

ggplot(super_final_2,mapping=aes(x=time,y=temp,color=as.factor(depth)))+
  geom_line()

ggplot(super_final_2,mapping=aes(x=time,y=DO,color=as.factor(depth)))+
  geom_line()

ggplot(super_final_2,mapping=aes(x=time,y=chla,color=as.factor(depth)))+
  geom_line()

ggplot(super_final_2,mapping=aes(x=temp,y=-depth,color=as.factor(time)))+
  geom_line()

sup <- super_final_2 %>% mutate(doy = yday(time)) %>% filter(doy>335)

ggplot(sup,mapping=aes(x=temp,y=-depth,color=as.factor(time)))+
  geom_line()

sup <- sup %>% group_by(depth) %>% summarize_all(funs(mean)) # Average of 12-06-2018 and 12-06-2019

# Convert oxygen to correct units
sup <- sup %>% mutate(DO = DO*1000/32) #to convert mg/L to molar units

# Extrapolate to initial depths needed
depth <- c(0.1, 0.33, 0.66, 1, 1.33, 1.66, 2, 2.33, 2.66, 3, 3.33, 3.66, 4, 4.33, 4.66, 5, 5.33, 5.66, 6, 6.33, 6.66, 7, 7.33, 7.66, 8, 8.33, 8.66, 9, 9.33, 9.66, 10, 10.33, 10.66, 11, 11.33, 11.66, 12, 12.33, 12.66, 13, 13.33)
initdepths <- rep(-99,length(depth))

initdepths <- cbind.data.frame(depth,initdepths)
initdepths <- merge(initdepths, sup, by="depth", all.x=TRUE, all.y=TRUE)

initdepths <- initdepths %>% mutate(temp = na.fill(na.approx(temp,na.rm=FALSE),"extend")) %>% 
  mutate(DO = na.fill(na.approx(DO,na.rm=FALSE),"extend")) %>% 
  mutate(chla = na.fill(na.approx(chla,na.rm=FALSE),"extend")) %>% 
  select(depth,temp,DO,chla) %>% 
  write.csv("Init_CTD.csv", row.names = F)

###########################################################
###### WATER CHEM DATA FROM EDI

#now let's build a chemistry field_data file
#first pull in FCR chem data from 2013-2019 from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/199/6/2b3dc84ae6b12d10bd5485f1c300af13" 
infile1 <- paste0(getwd(),"/chem.csv")
download.file(inUrl1,infile1,method="curl")

BVRchem <- read.csv("chem.csv", header=T) %>%
  select(Reservoir:DIC_mgL) %>%
  dplyr::filter(Reservoir=="BVR") %>%
  dplyr::filter(Site==50) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth_m, NH4_ugL:DIC_mgL) %>%
  rename(Depth=Depth_m) %>%
  mutate(NIT_amm = NH4_ugL*1000*0.001*(1/18.04)) %>% 
  mutate(NIT_nit = NO3NO2_ugL*1000*0.001*(1/62.00)) %>% #as all NO2 is converted to NO3
  mutate(PHS_frp = SRP_ugL*1000*0.001*(1/94.9714)) %>% 
  mutate(OGM_doc = DOC_mgL*1000*(1/12.01)* 0.10) %>% #assuming 10% of total DOC is in labile DOC pool (Wetzel page 753)
  mutate(OGM_docr = DOC_mgL*1000*(1/12.01)* 0.90) %>% #assuming 90% of total DOC is in recalcitrant DOC pool
  mutate(CAR_dic = DIC_mgL*1000*(1/52.515)) %>% #Long-term avg pH of FCR is 6.5, at which point CO2/HCO3 is about 50-50
  #given this disparity, using a 50-50 weighted molecular weight (44.01 g/mol and 61.02 g/mol, respectively)
  select(DateTime, Depth, NIT_amm:CAR_dic) %>%
  drop_na(NIT_amm) %>%
  filter(OGM_docr<500) %>% #remove high DOC outliers
  distinct(DateTime, Depth, .keep_all=TRUE)
 
ggplot(BVRchem, aes(DateTime, OGM_docr, colour=Depth)) + 
  geom_point()

write.csv(BVRchem, "field_chem_2DOCpools.csv", row.names = F)

# Select 12/6/18 for Initial chemistry conditions
chem <- BVRchem %>% filter(DateTime==as.POSIXct("2018-12-06")) %>% rename(depth=Depth)
init_chem <- rep(-99,length(depth))

init_chem <- cbind.data.frame(depth,init_chem)
init_chem <- merge(init_chem, chem, by="depth", all.x=TRUE, all.y=TRUE)

init_chem <- init_chem %>% mutate(NIT_amm = na.fill(na.approx(NIT_amm,na.rm=FALSE),"extend")) %>% 
  mutate(NIT_nit = na.fill(na.approx(NIT_nit,na.rm=FALSE),"extend")) %>% 
  mutate(PHS_frp = na.fill(na.approx(PHS_frp,na.rm=FALSE),"extend")) %>%
  mutate(OGM_doc = na.fill(na.approx(OGM_doc,na.rm=FALSE),"extend")) %>% 
  mutate(OGM_docr = na.fill(na.approx(OGM_docr,na.rm=FALSE),"extend")) %>% 
  mutate(CAR_dic = na.fill(na.approx(CAR_dic,na.rm=FALSE),"extend")) %>% 
  select(depth,NIT_amm,NIT_nit,PHS_frp,OGM_doc,OGM_docr,CAR_dic) %>% 
  write.csv("Init_chem.csv", row.names = F)

#######now make FCR chem dataset with one DOC pool
FCRchem <- read.csv("chem.csv", header=T) %>%
  select(Reservoir:DIC_mgL) %>%
  dplyr::filter(Reservoir=="FCR") %>%
  dplyr::filter(Site==50) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth_m, NH4_ugL:DIC_mgL) %>%
  rename(Depth=Depth_m) %>%
  mutate(NIT_amm = NH4_ugL*1000*0.001*(1/18.04)) %>% 
  mutate(NIT_nit = NO3NO2_ugL*1000*0.001*(1/62.00)) %>% #as all NO2 is converted to NO3
  mutate(PHS_frp = SRP_ugL*1000*0.001*(1/94.9714)) %>% 
  mutate(OGM_doc = DOC_mgL*1000*(1/12.01)) %>% 
  mutate(CAR_dic = DIC_mgL*1000*(1/52.515)) %>% #Long-term avg pH of FCR is 6.5, at which point CO2/HCO3 is about 50-50
  #given this disparity, using a 50-50 weighted molecular weight (44.01 g/mol and 61.02 g/mol, respectively)
  select(DateTime, Depth, NIT_amm:CAR_dic) %>%
  drop_na(NIT_amm) %>%
  distinct(DateTime, Depth, .keep_all=TRUE)

ggplot(FCRchem, aes(DateTime, OGM_doc, colour=Depth)) + 
  geom_point()

write.csv(FCRchem, "field_chem_1DOCpool.csv", row.names = F)

###########################################################
###### ANCILLARY LAB CHEMISTRY DATASETS NEEDED FOR CALIBRATION 

#read in lab dataset of dissolved silica, measured by Jon in summer 2014 only
silica <- read.csv("C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/inputs/BVR2014_Chemistry.csv", header=T) %>%
  select(Date, Depth, DRSI_mgL) %>%
  mutate(Date = as.POSIXct(strptime(Date, "%m/%d/%Y", tz="EST"))) %>%
  rename(DateTime = Date, SIL_rsi=DRSI_mgL) %>%
  mutate(SIL_rsi = SIL_rsi*1000*(1/60.08)) #convert to molar units

silica <- na.omit(silica)

write.csv(silica, "field_silica.csv", row.names = F)

#read in lab dataset of dissolved methane concentrations, measured in FCR
ch4 <- read.csv("C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/inputs/Dissolved_GHG_data_FCR_BVR_site50_inf_wet_15_19_not_final.csv", header=T) %>%
  dplyr::filter(Reservoir=="BVR") %>%
  dplyr::filter(Depth_m < 50) %>% #to remove weir inflow site
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  rename(Depth = Depth_m, CAR_ch4 = ch4_umolL, CAR_pCO2 = co2_umolL) %>%
  select(DateTime, Depth, CAR_ch4, CAR_pCO2) %>%
  mutate(CAR_pCO2 = CAR_pCO2*(0.0018/1000000)/0.0005667516) %>% #to convert umol/L to pCO2
  group_by(DateTime, Depth) %>%
  summarise(CAR_pCO2=mean(CAR_pCO2,na.rm=TRUE), CAR_ch4=mean(CAR_ch4,na.rm=TRUE))
write.csv(ch4, "field_gases.csv", row.names=F)

###########################################################
###### SECCHI DATA FROM EDI

#first pull in Secchi data from 2013-2019 from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/198/7/01c3762d9d2c4069eeb3dc10aa236c47" 
infile1 <- paste0(getwd(),"/Secchi_depth_2013-2019.csv")
download.file(inUrl1,infile1,method="curl")
#note that something's funky with this file- I had to open it up and re-format dates before it could be used

secchi <- read.csv("Secchi_depth_2013-2019.csv", header=T) %>%
  dplyr::filter(Reservoir=="BVR") %>%
  dplyr::filter(Site==50) %>%
  dplyr::filter(Flag_Secchi==0) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%m/%d/%Y", tz="EST"))) %>%
  mutate(Depth = rep(1, length(DateTime))) %>%
  mutate(extc_coef = Secchi_m/1.7) %>%
  select(DateTime, Depth, Secchi_m, extc_coef)
write.csv(secchi, "field_secchi.csv", row.names=F)
  
  