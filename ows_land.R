rm(list = ls())
library(dplyr)
library(lubridate)
library(adehabitatLT)
library(proj4)

#prep data
load('all.RData')
 all$datetime <- as.POSIXct(all$datetime)

#convert lat/long to UTM
M <- as.matrix(cbind(all$gps_lon, all$gps_lat)) #create a matrix to project
xy <- project(M, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #project to NAD 1983 Alaska Albers
X <- xy[,1]
Y <- xy[,2]

all <- cbind(all,X,Y) #bind datasets

save(all, file="all.RData")

#Filter database 
load("all.RData")
ows.land<- dplyr::filter(all, land==1 & ows==1)
ows.land$datetime <- as.POSIXct(ows.land$datetime)

save(ows.land, file="ows_land.RData")
