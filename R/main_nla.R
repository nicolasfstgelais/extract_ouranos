rm(list = ls()) 

library(ncdf4)
library(raster)
library(sf)
library(googledrive)
#https://googledrive.tidyverse.org/

#Download data from google drive

#simulation data
files=drive_ls("LakePulse/data/temp_rain")

for(i in 1:nrow(files)){
  #if the file does not exists in temp = download
if(!file.exists(paste0("data/temp/",files$name[i]))){
drive_download(file=as_id(files$id[i]),path = paste0("data/temp/",files$name[i]),overwrite = T)
}}

#nla coords
drive_download(file="LakePulse/data/NLA/nla_coords.csv",path = paste0("data/temp/nla_coords.csv"),overwrite = T)

nla_coords=read.csv("data/temp/nla_coords.csv")

#remove missing coords
nla_coords=nla_coords[!is.na(nla_coords$latitude),]

xy <- nla_coords[,c("longitude","latitude")]

stations=nla_coords$station

#read function
source("R/extract_ouranos.R")

###
# var: rsds (solar radiation), scfWind (wind), pr (precipitation), tas (temperature)
# stations: name of the stations
# lat_long: The lat and long of each station 
# years selectecd between 1955 and 2100
##

output=extract_ouranos(dir="data/temp/",
                       scenario=c("rcp45","rcp85"),
                       var=c("rsds","sfcWind"),
                       year_sel=c(seq(2020,2100,10)),
                       stations=stations,
                       lat_long=xy)    

write.csv(output,"data/output_nla.csv",row.names = F)
