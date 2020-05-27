### Script to compare FCR weir and wetlands 1. Discharge and 2. Chemistry for 2019
### A Hounshell, 26 May 2020

# Load packages
pacman::p_load(tidyverse,ggplot2,dplyr,ggpubr)

########################################## 1. Discharge ########################################################

# Load in discharge data: comparing WVWA and VT discharge calcacluated at weir with wetlands flowmate measurements
flowmate <- read.csv('C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Raw_Data/2019_Continuum_Discharge.csv')
flowmate$Date <- as.POSIXct(strptime(flowmate$Date, "%m/%d/%Y"))

weir <- read.csv('C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Raw_Data/inflow_for_EDI_2013_06Mar2020.csv')
weir$DateTime <- as.POSIXct(strptime(weir$DateTime, "%m/%d/%Y %H:%M"))

# Select wetlands flowmate data only
wetlands <- flowmate %>% filter(Reservoir == "FCR" & Site == "200")

# Select data for 2019
weir_2019 <- weir %>% filter(DateTime>=as.Date('2019-01-01')&DateTime<=as.Date('2019-12-31'))

# Select data for noon each day (assume this will be close to flowmate measurements taken at wetlands)
weir_2019 <- weir_2019 %>% mutate(time = format(weir_2019$DateTime, "%H:%M"))
weir_noon <- weir_2019 %>% filter(time == "12:00")
weir_noon <- weir_noon %>% mutate(Date = as.POSIXct(format(weir_noon$DateTime, "%Y-%m-%d")))
weir_noon <- weir_noon %>% select(Date,WVWA_Flow_cms,VT_Flow_cms)


# Merge wetlands and weir_noon data
merged <- left_join(weir_noon,wetlands,by="Date",all=TRUE)

# Select rows that have both weir and wetlands data
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

merged_select <- completeFun(merged,"Flow_cms")

merged_select <- merged_select %>% select(Date,WVWA_Flow_cms,VT_Flow_cms,Flow_cms)
names(merged_select)[4] <- "Wetlands_Flow_cms"

# Plot
ggplot()+
  geom_point(merged_select,mapping=aes(x=Date,y=WVWA_Flow_cms,color="Weir WVWA"))+
  geom_line(merged_select,mapping=aes(x=Date,y=WVWA_Flow_cms,color="Weir WVWA"))+
  geom_point(merged_select,mapping=aes(x=Date,y=VT_Flow_cms,color="Weir VT"))+
  geom_line(merged_select,mapping=aes(x=Date,y=VT_Flow_cms,color="Weir VT"))+
  geom_point(merged_select,mapping=aes(x=Date,y=Wetlands_Flow_cms,color="Wetlands"))+
  geom_line(merged_select,mapping=aes(x=Date,y=Wetlands_Flow_cms,color="Wetlands"))+
  theme_classic(base_size=15)

# Save as a .csv
write_csv(merged_select, path = "C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Data_Output/FCR_Q_Ratios.csv")

########################################### 2. Chemistry #######################################################

# Load in chemistry data (from EDI; 2013-2019)
chem <- read.csv('C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Raw_Data/chemistry_edi_2019.csv')
chem$DateTime <- as.POSIXct(strptime(chem$DateTime, "%m/%d/%Y"))

chem_100 <- chem %>% filter(DateTime>=as.Date('2019-01-01')&DateTime<=as.Date('2019-12-31')) %>% 
  filter(Reservoir == "FCR", Site == "100") %>% select(Site,DateTime,TN_ugL,TP_ugL,NH4_ugL,NO3NO2_ugL,SRP_ugL,
                                                       DOC_mgL,DIC_mgL,DC_mgL,DN_mgL)
names(chem_100)[3] <- "Weir_TN_ugL"
names(chem_100)[4] <- "Weir_TP_ugL"
names(chem_100)[5] <- "Weir_NH4_ugL"
names(chem_100)[6] <- "Weir_NO3NO2_ugL"
names(chem_100)[7] <- "Weir_SRP_ugL"
names(chem_100)[8] <- "Weir_DOC_mgL"
names(chem_100)[9] <- "Weir_DIC_mgL"
names(chem_100)[10] <- "Weir_DC_mgL"
names(chem_100)[11] <- "Weir_DN_mgL"


chem_200 <- chem %>% filter(Reservoir == "FCR", Site == "200") %>% select(Site,DateTime,TN_ugL,TP_ugL,NH4_ugL,
                                                                          NO3NO2_ugL,SRP_ugL,DOC_mgL,DIC_mgL,
                                                                          DC_mgL,DN_mgL)
names(chem_200)[3] <- "Wtlnds_TN_ugL"
names(chem_200)[4] <- "Wtlnds_TP_ugL"
names(chem_200)[5] <- "Wtlnds_NH4_ugL"
names(chem_200)[6] <- "Wtlnds_NO3NO2_ugL"
names(chem_200)[7] <- "Wtlnds_SRP_ugL"
names(chem_200)[8] <- "Wtlnds_DOC_mgL"
names(chem_200)[9] <- "Wtlnds_DIC_mgL"
names(chem_200)[10] <- "Wtlnds_DC_mgL"
names(chem_200)[11] <- "Wtlnds_DN_mgL"

chem_merge <- left_join(chem_100,chem_200,by="DateTime")
chem_merge <- completeFun(chem_merge,"Site.y")

chem_merge <- chem_merge %>% select(DateTime,Weir_TN_ugL,Weir_TP_ugL,Weir_NH4_ugL,Weir_NO3NO2_ugL,Weir_SRP_ugL,
                                    Weir_DOC_mgL,Weir_DIC_mgL,Weir_DC_mgL,Weir_DN_mgL,Wtlnds_TN_ugL,
                                    Wtlnds_TP_ugL,Wtlnds_NH4_ugL,Wtlnds_NO3NO2_ugL,Wtlnds_SRP_ugL,
                                    Wtlnds_DOC_mgL,Wtlnds_DIC_mgL,Wtlnds_DC_mgL,Wtlnds_DN_mgL)

# Plot
tn <- ggplot()+
  geom_point(chem_merge,mapping=aes(x=DateTime,y=Weir_TN_ugL,color="Weir"))+
  geom_line(chem_merge,mapping=aes(x=DateTime,y=Weir_TN_ugL,color="Weir"))+
  geom_point(chem_merge,mapping=aes(x=DateTime,y=Wtlnds_TN_ugL,color="Wetlands"))+
  geom_line(chem_merge,mapping=aes(x=DateTime,y=Wtlnds_TN_ugL,color="Wetlands"))+
  theme_classic(base_size=15)

tp <- ggplot()+
  geom_point(chem_merge,mapping=aes(x=DateTime,y=Weir_TP_ugL,color="Weir"))+
  geom_line(chem_merge,mapping=aes(x=DateTime,y=Weir_TP_ugL,color="Weir"))+
  geom_point(chem_merge,mapping=aes(x=DateTime,y=Wtlnds_TP_ugL,color="Wetlands"))+
  geom_line(chem_merge,mapping=aes(x=DateTime,y=Wtlnds_TP_ugL,color="Wetlands"))+
  theme_classic(base_size=15)

nh4 <- ggplot()+
  geom_point(chem_merge,mapping=aes(x=DateTime,y=Weir_NH4_ugL,color="Weir"))+
  geom_line(chem_merge,mapping=aes(x=DateTime,y=Weir_NH4_ugL,color="Weir"))+
  geom_point(chem_merge,mapping=aes(x=DateTime,y=Wtlnds_NH4_ugL,color="Wetlands"))+
  geom_line(chem_merge,mapping=aes(x=DateTime,y=Wtlnds_NH4_ugL,color="Wetlands"))+
  theme_classic(base_size=15)

no3 <- ggplot()+
  geom_point(chem_merge,mapping=aes(x=DateTime,y=Weir_NO3NO2_ugL,color="Weir"))+
  geom_line(chem_merge,mapping=aes(x=DateTime,y=Weir_NO3NO2_ugL,color="Weir"))+
  geom_point(chem_merge,mapping=aes(x=DateTime,y=Wtlnds_NO3NO2_ugL,color="Wetlands"))+
  geom_line(chem_merge,mapping=aes(x=DateTime,y=Wtlnds_NO3NO2_ugL,color="Wetlands"))+
  theme_classic(base_size=15)

srp <- ggplot()+
  geom_point(chem_merge,mapping=aes(x=DateTime,y=Weir_SRP_ugL,color="Weir"))+
  geom_line(chem_merge,mapping=aes(x=DateTime,y=Weir_SRP_ugL,color="Weir"))+
  geom_point(chem_merge,mapping=aes(x=DateTime,y=Wtlnds_SRP_ugL,color="Wetlands"))+
  geom_line(chem_merge,mapping=aes(x=DateTime,y=Wtlnds_SRP_ugL,color="Wetlands"))+
  theme_classic(base_size=15)

doc <- ggplot()+
  geom_point(chem_merge,mapping=aes(x=DateTime,y=Weir_DOC_mgL,color="Weir"))+
  geom_line(chem_merge,mapping=aes(x=DateTime,y=Weir_DOC_mgL,color="Weir"))+
  geom_point(chem_merge,mapping=aes(x=DateTime,y=Wtlnds_DOC_mgL,color="Wetlands"))+
  geom_line(chem_merge,mapping=aes(x=DateTime,y=Wtlnds_DOC_mgL,color="Wetlands"))+
  theme_classic(base_size=15)

dic <- ggplot()+
  geom_point(chem_merge,mapping=aes(x=DateTime,y=Weir_DIC_mgL,color="Weir"))+
  geom_line(chem_merge,mapping=aes(x=DateTime,y=Weir_DIC_mgL,color="Weir"))+
  geom_point(chem_merge,mapping=aes(x=DateTime,y=Wtlnds_DIC_mgL,color="Wetlands"))+
  geom_line(chem_merge,mapping=aes(x=DateTime,y=Wtlnds_DIC_mgL,color="Wetlands"))+
  theme_classic(base_size=15)

dc <- ggplot()+
  geom_point(chem_merge,mapping=aes(x=DateTime,y=Weir_DC_mgL,color="Weir"))+
  geom_line(chem_merge,mapping=aes(x=DateTime,y=Weir_DC_mgL,color="Weir"))+
  geom_point(chem_merge,mapping=aes(x=DateTime,y=Wtlnds_DC_mgL,color="Wetlands"))+
  geom_line(chem_merge,mapping=aes(x=DateTime,y=Wtlnds_DC_mgL,color="Wetlands"))+
  theme_classic(base_size=15)

dn <- ggplot()+
  geom_point(chem_merge,mapping=aes(x=DateTime,y=Weir_DN_mgL,color="Weir"))+
  geom_line(chem_merge,mapping=aes(x=DateTime,y=Weir_DN_mgL,color="Weir"))+
  geom_point(chem_merge,mapping=aes(x=DateTime,y=Wtlnds_DN_mgL,color="Wetlands"))+
  geom_line(chem_merge,mapping=aes(x=DateTime,y=Wtlnds_DN_mgL,color="Wetlands"))+
  theme_classic(base_size=15)

ggarrange(tn,tp,nh4,no3,srp,doc,dic,dc,dn,common.legend=TRUE,legend="right",ncol=3,nrow=3)

# Save as a .csv
write_csv(chem_merge, path = "C:/Users/ahoun/OneDrive/Desktop/BVR-GLM/BVR-GLM/Data_Output/FCR_Chem_Ratios.csv")
