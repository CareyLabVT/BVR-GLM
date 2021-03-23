#### Script to test Alkalinity models for GLM-AED ####
# See: https://github.com/AquaticEcoDynamics/libaed-water/blob/master/src/aed_carbon.F90
# Start on lines 428; Mode 1-5
# FCR-GLM uses Mode 1

wd <- getwd()
setwd(wd)
sim_folder <- getwd()

#load packages
pacman::p_load(dplyr,zoo,EcoHydRology,rMR,tidyverse,lubridate)

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
