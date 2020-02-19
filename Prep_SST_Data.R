library(raster)
library(colorRamps)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)

rm(list = ls())

#COBE
df = stack(paste0("/Users/", Sys.info()[7], "/Desktop/COBE_sst.nc"), varname = "sst")
df = raster::rotate(df) #rotate to -180:180
df = df[[241:2028]] #trim to 1870-2018
save(df, file = "~/extreme_normalizations/data/COBE_SST.RData")


#Hadley
df = stack(paste0("/Users/", Sys.info()[7], "/Desktop/HadI_sst.nc"), varname = "sst")
save(df, file = "~/extreme_normalizations/data/Hadl_SST.RData")
