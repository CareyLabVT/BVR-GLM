#### Script to test Alkalinity models for GLM-AED ####
# See: https://github.com/AquaticEcoDynamics/libaed-water/blob/master/src/aed_carbon.F90
# Start on lines 428; Mode 1-5
# FCR-GLM uses Mode 1

wd <- getwd()
setwd(wd)
sim_folder <- getwd()

#load packages
pacman::p_load(dplyr,zoo,EcoHydRology,rMR,tidyverse,lubridate,akima)

# Load data: Temp, DIC
# ASSUME SALINITY IS ZERO FOR ALL TIME POINTS! WILL BE SOOOO SMALL AND CAN IGNORE!
temp <- read_csv("./field_data/CleanedObsTemp.csv") %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,"%Y-%m-%d")))

dic <- read_csv("./field_data/field_chem_2DOCpools.csv") %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d")))
dic <- dic[!is.na(dic$CAR_dic),]

data <- left_join(dic,temp,by=c("DateTime","Depth"))

# Basically assume salnity = 0
data <- data %>% 
  mutate(sal = sample(0:0.1))

# Calculate TAlk using each model
# Model 1
data <- data %>% 
  mutate(alk_1 = 1627.4 + 22.176*sal)

# Model 2
p00  =       1063
p10  =      1.751
p01  =   -0.05369
p20  =     0.2266
p11  =  -0.001252
p02  =  0.0002546

data <- data %>% 
  mutate(alk_2 = p00 + p10*sal + p01*CAR_dic + p20*sal**2 + p11*CAR_dic*sal + p02*CAR_dic**2)

# Model 3
p00 =      -258.8
p10 =       34.59
p01 =      0.9923
p20 =      0.8186
p11 =    -0.03101
p02 =   0.0001045

data <- data %>% 
  mutate(alk_3 = p00 + p10*sal + p01*CAR_dic + p20*sal**2 + p11*CAR_dic*sal + p02*CAR_dic**2)

# Model 4
p00 =      -47.51
p10 =      -17.21
p01 =        1.32
p20 =      0.1439
p11 =     0.01224
p02 =  -0.0002055

data <- data %>% 
  mutate(alk_4 = p00 + p10*sal + p01*CAR_dic + p20*sal**2 + p11*CAR_dic*sal + p02*CAR_dic**2)

# Model 5
p00 =       157.7
p10 =       4.298
p01 =      0.6448
p20 =      0.2107
p11 =   -0.002072
p02 =   0.0001239

data <- data %>% 
  mutate(alk_5 = p00 + p10*sal + p01*CAR_dic + p20*sal**2 + p11*CAR_dic*sal + p02*CAR_dic**2)

ggplot()+
  geom_line(data,mapping=aes(x=DateTime,y=alk_1,color="alk_1"))+
  geom_line(data,mapping=aes(x=DateTime,y=alk_2,color="alk_2"))+
  geom_line(data,mapping=aes(x=DateTime,y=alk_3,color="alk_3"))+
  geom_line(data,mapping=aes(x=DateTime,y=alk_4,color="alk_4"))+
  geom_line(data,mapping=aes(x=DateTime,y=alk_5,color="alk_5"))+
  theme_classic(base_size=10)

# Calculate TCO2 following Carbon module (line 516)
a    =  8.24493*10^-1 - 4.0899*10^-3*data$temp + 7.6438*10^-5*data$temp**2 - 8.2467*10^-7*data$temp**3 + 5.3875*10^-9*data$temp**4
b    = -5.72466*10^-3 + 1.0227*10^-4*data$temp - 1.6546*10^-6*data$temp**2
c    =  4.8314*10^-4

data <- data %>% 
  mutate(dcf  = (999.842594 + 6.793952*10^-2*temp - 9.095290*10^-3*temp**2 + 1.001685*10^-4*temp**3
          - 1.120083*10^-6*temp**4 + 6.536332*10^-9*temp**5+a*sal+b*sal**1.5+c*sal**2)/(1.0*10^3))


### Create our own Alk model! ###
# [Alk] = DIC*pKa1 (assume that at neutral pH = 5-8)
alpha = ((10^-5/10^-6.3)+1+(10^-10.3/10^-5))^-1
alpha = ((10^-6/10^-6.3)+1+(10^-10.3/10^-6))^-1
alpha = ((10^-7/10^-6.3)+1+(10^-10.3/10^-7))^-1

data <- data %>% 
  mutate(alk_6 = 0.333854989*CAR_dic)

# Plot pH through time for BVR (b/c I'm curious)
# Load in data
ctd<-read.csv('C:/Users/ahoun/Desktop/BVR-GLM/field_data/CTD_final_2013_2019.csv')

ctd <- ctd %>% #read in observed CTD data, which has multiple casts on the same day (problematic for comparison)
  filter(Reservoir=="BVR") %>%
  filter(Site==50)

ctd <- ctd[!is.na(ctd$pH),]
ctd$Date <-  as.POSIXct(strptime(ctd$Date,"%Y-%m-%d", tz="EST"))

#focal depths we are trying to compare modeled data vs observations; for CTD/YSI casts
#assumed the deepest point of BVR = 11 m
depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

bvr_all <- ctd %>% 
  select(Date,Depth_m,Temp_C,DO_mgL,Cond_uScm,pH)

bvr_all <- bvr_all %>% group_by(Date,Depth_m) %>% summarize_all(funs(mean))

#Initialize an empty matrix with the correct number of rows and columns 
temp<-matrix(data=NA, ncol=ncol(bvr_all), nrow=length(depths)) #of cols in CTD data, and then nrows = # of layers produced
super_final<-matrix(data=NA, ncol=1, nrow=0)
dates<-unique(bvr_all$Date)

#create a function to chose the matching depth closest to our focal depths
closest<-function(xv, sv){
  xv[which.min(abs(xv-sv))]}

library(plyr) #only use plyr for this for loop, then detach!

#For loop to retrieve CTD depth with the closest function and fill in matrix
for (i in 1:length(dates)){
  j=dates[i]
  q <- subset(bvr_all, bvr_all$Date == j)
  
  layer1 <- q[q[, "Depth_m"] == closest(q$Depth_m,0.1),][1,]
  layer2<- q[q[, "Depth_m"] == closest(q$Depth_m,1.0),][1,]
  layer3<- q[q[, "Depth_m"] == closest(q$Depth_m,2),][1,]
  layer4<- q[q[, "Depth_m"] == closest(q$Depth_m,3),][1,]
  layer5<- q[q[, "Depth_m"] == closest(q$Depth_m,4),][1,]
  layer6<- q[q[, "Depth_m"] == closest(q$Depth_m,5),][1,]
  layer7<- q[q[, "Depth_m"] == closest(q$Depth_m,6),][1,]
  layer8<- q[q[, "Depth_m"] == closest(q$Depth_m,7),][1,]
  layer9<- q[q[, "Depth_m"] == closest(q$Depth_m,8),][1,]
  layer10<- q[q[, "Depth_m"] == closest(q$Depth_m,9),][1,]
  layer11<- q[q[, "Depth_m"] == closest(q$Depth_m,10),][1,]
  layer12<- q[q[, "Depth_m"] == closest(q$Depth_m,11),][1,]
  
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
  select(-c(1,Depth_m)) %>% 
  rename(DateTime = Date, Depth = new_depth) %>% 
  mutate(DateTime = as.POSIXct(DateTime))

super_final_2$pH <- as.numeric(super_final_2$pH)
super_final_2$Depth <- as.numeric(super_final_2$Depth)

# Combine DIC and pH data
data_2 <- full_join(data,super_final_2,by=c("DateTime","Depth"))

# Calculate alk ('alk 6') based on DIC and pH
### Create our own Alk model! ###
data_2 <- data_2 %>% 
  mutate(alk_6 = (((10^-pH/10^-6.3)+1+(10^-10.3/10^-pH))^-1)*CAR_dic)

# Plot Alk_4 ('best' fitting existing alk model) and Alk_6 through time
ggplot()+
  geom_point(data_2,mapping=aes(x=DateTime,y=alk_4,color="Alk 4"))+
  geom_line(data_2,mapping=aes(x=DateTime,y=alk_4,color="Alk 4"))+
  geom_point(data_2,mapping=aes(x=DateTime,y=alk_6,color="Alk 6"))+
  geom_line(data_2,mapping=aes(x=DateTime,y=alk_6,color="Alk 6"))+
  xlim(as.POSIXct("2018-01-01"),as.POSIXct("2019-12-31"))+
  theme_classic(base_size = 15)

median(data_2$alk_6,na.rm=TRUE)
max(data_2$alk_6,na.rm=TRUE)
min(data_2$alk_6,na.rm=TRUE)

# Plot long term pH
ggplot(super_final_2,mapping=aes(x=Date,y=pH,group=as.factor(new_depth),color=new_depth))+
  geom_line()+
  theme_classic(base_size=15)

median(super_final_2$pH)
max(super_final_2$pH)
min(super_final_2$pH)

# Plot DIC concentrations
ggplot(dic,mapping=aes(x=DateTime,y=CAR_dic,color=as.factor(Depth)))+
  geom_line()+
  geom_point()+
  theme_classic(base_size=15)

median(dic$CAR_dic)
min(dic$CAR_dic)
max(dic$CAR_dic)

### Load in pCO2 data and try to calculate DIC from pCO2 and pH -- use this (plus the DIC data) to calibrate
### alkalinity mode = 4
gases <- read_csv("./field_data/field_gases.csv") %>% 
  mutate(DateTime = as.POSIXct(strptime(DateTime,"%Y-%m-%d")))

gases_2 <- full_join(gases,super_final_2,by=c("DateTime","Depth"))

gases_2 <- gases_2 %>% 
  arrange(DateTime,Depth)

         