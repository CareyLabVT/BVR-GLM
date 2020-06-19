### Script to compare inflow GHGs (CH4) for BVR and FCR
### Can we use FCR as a proxy for CH4 inflows?
### A Hounshell, 19 Jun 2020

# Load in libraries
pacman::p_load(tidyverse,ggplot2,zoo)

# Load in FCR data
ghg <- read.csv("Dissolved_GHG_data_FCR_BVR_site50_inf_wet_15_19_not_final.csv", header=T) %>%
  dplyr::filter(Reservoir == "FCR") %>%
  dplyr::filter(Depth_m == 100) %>% #weir inflow
  select(DateTime, ch4_umolL) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  rename(time = DateTime, CAR_ch4 = ch4_umolL) %>%
  group_by(time) %>%
  summarise(CAR_ch4 = mean(CAR_ch4))

fcr_200 <- read.csv("Dissolved_GHG_data_FCR_BVR_site50_inf_wet_15_19_not_final.csv", header=T) %>%
  dplyr::filter(Reservoir == "FCR") %>%
  dplyr::filter(Depth_m == 200) %>% #weir inflow
  select(DateTime, ch4_umolL) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  rename(time = DateTime, CAR_ch4 = ch4_umolL) %>%
  group_by(time) %>%
  summarise(CAR_ch4 = mean(CAR_ch4))

# Load in BVR data (limited to RC field days)
ghg_100 <- read.csv("BVR_GHG_Inflow_20200619.csv", header=T) %>%
  dplyr::filter(Reservoir == "BVR") %>%
  dplyr::filter(Depth_m == 100) %>% #weir inflow
  select(DateTime, ch4_umolL) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%d-%b-%y", tz="EST"))) %>%
  rename(time = DateTime, CAR_ch4 = ch4_umolL) %>%
  group_by(time) %>%
  summarise(CAR_ch4 = mean(CAR_ch4))

# Load in BVR data (limited to RC field days)
ghg_200 <- read.csv("BVR_GHG_Inflow_20200619.csv", header=T) %>%
  dplyr::filter(Reservoir == "BVR") %>%
  dplyr::filter(Depth_m == 200) %>% #weir inflow
  select(DateTime, ch4_umolL) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%d-%b-%y", tz="EST"))) %>%
  rename(time = DateTime, CAR_ch4 = ch4_umolL) %>%
  group_by(time) %>%
  summarise(CAR_ch4 = mean(CAR_ch4))

# Plot
ggplot()+
  geom_line(ghg,mapping=aes(time,CAR_ch4,color="FCR 100"))+
  geom_point(ghg,mapping=aes(time,CAR_ch4,color="FCR 100"))+
  geom_line(fcr_200,mapping=aes(time,CAR_ch4,color="FCR 200"))+
  geom_point(fcr_200,mapping=aes(time,CAR_ch4,color="FCR 200"))+
  geom_point(ghg_100,mapping=aes(time,CAR_ch4,color="BVR 100"))+
  geom_line(ghg_100,mapping=aes(time,CAR_ch4,color="BVR 100"))+
  geom_point(ghg_200,mapping=aes(time,CAR_ch4,color="BVR 200"))+
  geom_line(ghg_200,mapping=aes(time,CAR_ch4,color="BVR 200"))+
  theme_classic(base_size=15)
