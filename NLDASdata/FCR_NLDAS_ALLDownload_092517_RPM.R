###########################################################
### Downloading NLDAS2 data for meteorological hourly forcing 
### http://ldas.gsfc.nasa.gov/nldas/NLDAS2forcing.php
### Author: Hilary Dugan
#Modified by Ryan and then by CCC
### Last edit 08 Nov 2018
### Run on Windows computer for best results, does not seem to like Mac
###########################################################  
setwd("./NLDASData") #relative data path
#install.packages("RCurl","lubridate","raster","ncdf4","rgdal","httr")
library(RCurl)
library(lubridate)
library(raster)
library(ncdf4)
library(rgdal)
library(httr) #allows you to pull NLDAS data directly from website

###########################################################
### Enter password information 
###########################################################  
#https://urs.earthdata.nasa.gov/profile <-- GET A EARTHDATA LOGIN
username = "ryan333" #ryan's ok with us using his :)
password = "Ausable_101"

###########################################################
### Use shapefile of lake to set bounding box  
###########################################################
#list.files("~/NLDAS2_FCR/506_9", pattern='\\.shp$')
#file.exists("~/NLDAS2_FCR/506_9.shp")

# lakeShape = readOGR(dsn=path.expand("~/NLDAS2_FCR/506_9.shp"), layer="506_9")
# proj4string(lakeShape) 
# newShp <- spTransform(lakeShape, CRS("+proj=tmerc +lon_0=-84 +lat_0=0 +k=0.9999 +datum=WGS84"))
# lakeShape@proj4string <- "+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +datum=potsdam +units=m +no_defs"
# newShp <- spTransform(lakeShape, CRS("+proj=longlat + datum=WGS84"))
# extent = as.numeric(lakeShape@bbox)

#lakeShape = readOGR('../NLDAS2_FCR/',layer='506_9') ### I am coming across an error for this file. 

extent = c(-79.83906,  37.30272, -79.83601,  37.30934) #this is specifically FCR ONLY: you don't need to work with the shapefile above

###########################################################
### Set timeframe 
###########################################################  
out = seq.POSIXt(as.POSIXct('2018-12-31 12:00',tz = 'GMT'),as.POSIXct('2020-01-01 12:00',tz='GMT'),by = 'hour') #Already done: '2014-12-10 00:00' to '2019-12-31 23:00' #NLDAS ready ~3 days before present
#for s&g we did an overlap of one hour as a reality check to make sure that they lined up- might be good practice in the future?
vars = c('PEVAPsfc_110_SFC_acc1h', 'DLWRFsfc_110_SFC', 'DSWRFsfc_110_SFC', 'CAPE180_0mb_110_SPDY',
         'CONVfracsfc_110_SFC_acc1h', 'APCPsfc_110_SFC_acc1h', 'SPFH2m_110_HTGL', 
         'VGRD10m_110_HTGL', 'UGRD10m_110_HTGL', 'TMP2m_110_HTGL', 'PRESsfc_110_SFC')

# Create output list of tables
output = list()

###########################################################
### Need to know how many cells your lake falls within
### Can download one instance of data and see how many columns there are
###########################################################  
cellNum = 4 #How many output cells will there be? Need to check this beforehand
for (l in 1:11){
  colClasses = c("POSIXct", rep("numeric",cellNum))
  col.names = c('dateTime',rep(vars[l],cellNum))
  output[[l]] = read.table(text = "",colClasses = colClasses,col.names = col.names)
  attributes(output[[l]]$dateTime)$tzone = 'GMT'
}


###########################################################
### Run hourly loop 
###########################################################  
# Start the clock!
ptm <- proc.time()

for (i in 1:length(out)) {
  print(out[i])
  yearOut = year(out[i])
  monthOut = format(out[i], "%m")
  dayOut = format(out[i], "%d")
  hourOut = format(out[i], "%H%M")
  doyOut = format(out[i],'%j')

  filename = format(out[i], "%Y%m%d%H%M")
  
  #URL3 = paste('https://',username,':',password,'@hydro1.gesdisc.eosdis.nasa.gov/daac-bin/OTF/HTTP_services.cgi?',
  #             'FILENAME=%2Fdata%2FNLDAS%2FNLDAS_FORA0125_H.002%2F',yearOut,'%2F',doyOut,'%2FNLDAS_FORA0125_H.A',yearOut,monthOut,dayOut,'.',hourOut,'.002.grb&',
  #             'FORMAT=bmV0Q0RGLw&BBOX=',extent[2],'%2C',extent[1],'%2C',extent[4],'%2C',extent[3],'&',
  #             'LABEL=NLDAS_FORA0125_H.A',yearOut,monthOut,dayOut,'.',hourOut,'.002.2017013163409.pss.nc&',
  #             'SHORTNAME=NLDAS_FORA0125_H&SERVICE=SUBSET_GRIB&VERSION=1.02&DATASET_VERSION=002',sep='')
  
  
  # IMPORTANT MESSAGE Dec 05, 2016    The GES DISC will be migrating from http to https throughout December   
  # As part of our ongoing migration to HTTPS, the GES DISC will begin redirecting all HTTP traffic to HTTPS.  
  # We expect to have all GES DISC sites redirecting traffic by January 4th. For most access methods, the redirect will be transparent to the user.  
  # However, users with locally developed scripts or utilities that do not support an HTTP code 301 redirect may find that the scripts will fail.  
  # If you access our servers non-interactively (i.e. via a mechanism other than a modern web browser), you will want to modify your scripts to 
  # point to the HTTPS addresses to avoid the enforced redirect.
  
  #library(httr)
  #x = download.file(URL3,destfile = paste(filename,'.nc',sep=''),mode = 'wb',quiet = T)

  for (v in 1:11) {
    br = brick(paste(filename,'.nc',sep=''),varname = vars[v])
    output[[v]][i,1] = out[i]
    output[[v]][i,-1] = getValues(br[[1]])
  }
  rm(br)
  #Sys.sleep(2)
}
# Stop the clock
proc.time() - ptm

###########################################################
### Save all 11 variables from the output list 
###########################################################  

for (f in 1:11){
  write.csv(output[[f]],paste('FCR_NLDAS2_',vars[f],'.csv',sep=''),row.names=F)
}


