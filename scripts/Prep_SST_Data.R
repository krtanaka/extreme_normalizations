library(raster)
library(colorRamps)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)

rm(list = ls())

# https://osf.io/mj8u7/

#COBE
df = stack(paste0("/Users/", Sys.info()[7], "/Desktop/COBE_sst.nc"), varname = "sst")
df = raster::rotate(df) #rotate to -180:180
df = df[[241:2040]] #trim to 1870-2019
assign("df", df, .GlobalEnv)
df = readAll(df)
save(df, file = paste0("/Users/", Sys.info()[7], "/Desktop/COBE_SST.RData"))


#Hadley
df = stack(paste0("/Users/", Sys.info()[7], "/Desktop/HadI_sst.nc"), varname = "sst")
# df = stack(paste0("/Users/", Sys.info()[7], "/Desktop/HadI_sst_ice.nc"), varname = "sic")
df = df[[1:1800]] #trim to 1870-2019
assign("df", df, .GlobalEnv)
df = readAll(df)
save(df, file = paste0("/Users/", Sys.info()[7], "/Desktop/HadI_SST.RData"))


#ERSST
df = stack(paste0("/Users/", Sys.info()[7], "/Desktop/ERSSTv5.nc"), varname = "sst")
df = raster::rotate(df) #rotate to -180:180
df = df[[193:1992]] #trim to 1870-2019
assign("df", df, .GlobalEnv)
df = readAll(df)
save(df, file = paste0("/Users/", Sys.info()[7], "/Desktop/ER_SST.RData"))
