### Script to use NLDAS (2013 - 2018) preciptation data + Schuler equation (Ward et al. 2020; Sunapee Metadata)
### to calculate daily run-off volume for BVR
## A Hounshell, 27 Mar 2020
## NOTE: Assuming precipitation is reported in mm

# Load packages
pacman::p_load(tidyverse,ggplot2)

# Load in NLDAS data: currently from 01 Jan 2013 to 31 Dec 2018 (will need to update at some point : )
nldas <- read_csv("C:/Users/ahoun/OneDrive/Desktop/BVR_GLM/BVR-GLM/Raw_Data/FCR_GLM_NLDAS_010113_123118_GMTadjusted.csv")

# Remove rows with NA values (original file skipped every-other row)
nldas2 <- nldas[complete.cases(nldas),]

# Convert datetime
nldas2$time <- as.POSIXct(strptime(nldas2$time, "%m/%d/%Y %H:%M", tz = "GMT"))

# Use Schuler Equation to calculate daily run-off volume to BVR
# R = P * Pj * Rv where P = precipitation (mm), Pj = 0.9 (fraction of annual rainfall events that produce run-off), Rv = Runoff
# Coefficient (assuming no imprevious surface in BVR; Rv = 0.05)
# Converted to m (from mm) -> then multiply by total watershed surface area for BVR
nldas2 <- nldas2 %>% mutate(r = (Rain * 0.9 * 0.05)/1000)

### If you end up modelling overland flow + baseflow, what do 
### you need discharge for? Other than as a comparison for the days we have it? How is discharge used when we only have 6 points
### over 7 years?