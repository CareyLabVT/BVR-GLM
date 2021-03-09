### Script to plot variables v. oxygen ###
# Thinking about what Fsed, Ksed should look like for calibrations
# 09 Mar 2021, A Hounshell

pacman::p_load(tidyverse,lubridate,broom)

# Load in variable and oxygen data
obs_oxy<-read.csv('field_data/CleanedObsOxy.csv') %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))

var="CAR_dic"
obs_dic<-read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  group_by(DateTime, Depth) %>%
  summarise(CAR_dic = mean(CAR_dic)) %>%
  na.omit() 
obs_dic<-as.data.frame(obs_dic)

# Merge Oxy and DIC data
oxy_dic <- left_join(obs_dic,obs_oxy,by=c("DateTime","Depth"))
oxy_dic_depth <- oxy_dic %>% 
  filter(Depth == "11")

fit <- nls(CAR_dic ~ SSasymp(OXY_oxy,yf,y0,log_alpha),data=oxy_dic)

qplot(OXY_oxy, CAR_dic, data = augment(fit)) + geom_line(aes(y = .fitted))

fit <- nls(CAR_dic ~ SSasymp(OXY_oxy,yf,y0,log_alpha),data=oxy_dic_depth)

qplot(OXY_oxy, CAR_dic, data = augment(fit)) + geom_line(aes(y = .fitted))

# Plot
ggplot(oxy_dic,mapping=aes(x=OXY_oxy,y=CAR_dic,color=as.factor(Depth)))+
  geom_point()+
  theme_classic()

ggplot(oxy_dic_depth,mapping=aes(x=OXY_oxy,y=CAR_dic,color=as.factor(Depth)))+
  geom_point()+
  theme_classic()

# CH4
var="CAR_ch4"
obs_ch4<-read.csv('field_data/field_gases.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  group_by(DateTime, Depth) %>%
  summarise(CAR_ch4 = mean(CAR_ch4)) %>%
  na.omit() 
obs_ch4<-as.data.frame(obs_ch4)

# Merge Oxy and ch4 data
oxy_ch4 <- left_join(obs_ch4,obs_oxy,by=c("DateTime","Depth"))
oxy_ch4_depth <- oxy_ch4 %>% 
  filter(Depth == "11")

fit <- nls(CAR_ch4 ~ SSasymp(OXY_oxy,yf,y0,log_alpha),data=oxy_ch4)

qplot(OXY_oxy, CAR_ch4, data = augment(fit)) + geom_line(aes(y = .fitted))

# Plot
ggplot(oxy_ch4,mapping=aes(x=OXY_oxy,y=CAR_ch4,color=as.factor(Depth)))+
  geom_point()+
  theme_classic()

ggplot(oxy_ch4_depth,mapping=aes(x=OXY_oxy,y=CAR_ch4,color=as.factor(Depth)))+
  geom_point()+
  theme_classic()

var="CAR_pCO2"
obs_co2<-read.csv('field_data/field_gases.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  group_by(DateTime, Depth) %>%
  summarise(CAR_pCO2 = mean(CAR_pCO2)) %>%
  na.omit() 

oxy_co2 <- left_join(obs_co2,obs_oxy,by=c("DateTime","Depth"))
oxy_co2_depth <- oxy_co2 %>% 
  filter(Depth == "11")

fit <- nls(CAR_pCO2 ~ SSasymp(OXY_oxy,yf,y0,log_alpha),data=oxy_co2)

qplot(OXY_oxy, CAR_pCO2, data = augment(fit)) + geom_line(aes(y = .fitted))
