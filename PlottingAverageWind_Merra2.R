#################################################################################
# Load Libraries 
#################################################################################
library(ggplot2) # plots 
library(tidyverse) # data cleanup
library(sf) # spatial
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial) # spatial ggplot
library(rgeos)
library(raster) # convert netcdf to raster
library(rworldmap)
library(shape)
library(ncmeta)
library(ncdf4)
library(RNetCDF)
library(nngeo)
library(lubridate) # Clean up dates
library(rgdal)
library(stars) 
library(lattice)
library(rasterVis) # vectorplot
library(colorRamps) # matlab.like
library(viridisLite)# color palette
library(DescTools) #closest
library(imputeTS) #na.interpolation
library(swfscMisc)
require(RColorBrewer) # color palette
library(viridis)# color palette
library(maps)# world maps for ggplot

#################################################################################
# Useful Links
#################################################################################
# File specification: https://gmao.gsfc.nasa.gov/pubs/docs/Bosilovich785.pdf
# How to download the data: https://daac.gsfc.nasa.gov/information/howto?title=How%20to%20Download%20MERRA-2%20Daily%20Mean%20Data
# How to calculate and plot wind speed using MERRA-2 wind component: https://disc.gsfc.nasa.gov/information/howto?title=How%20to%20calculate%20and%20plot%20wind%20speed%20using%20MERRA-2%20wind%20component%20data%20using%20Python


#################################################################################
# Download data
#################################################################################
# -80, -70, -20, -30 Extent

wind <- read_ncdf("/Users/tziporahserota/Desktop/Merra-2Datasets/Nov-Jan2019_hourly/MERRA2_400.inst1_2d_asm_Nx.20191128.nc4.nc4")
times<-st_get_dimension_values(wind, "time") # months
wind_u<-as(wind[1,,,], "Raster") # u-component
wind_v<-as(wind[2,,,], "Raster")# v-component 
wind_u.df = raster::as.data.frame(wind_u, xy = TRUE) # convert to data frame 









