########################################################################
# Load Libraries
########################################################################
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

# Code for plotting average wind data with tracks 
# https://renewable-analytics.netlify.app/2018/06/25/using-era-interim-in-r/

##########################################################################################
# AUXILIARY FUNCTIONS
# run before analyses to calculate wind speed 
##########################################################################################
# -----------------------------------------------------------------
# uv2ddff
# -----------------------------------------------------------------
uv2ddff <- function(u, v = NULL, rad = FALSE){
  # if input u is zoo or data.frame
  zoo_index <- NULL # Default
  if (inherits(u, c("zoo", "data.frame"))) {
    # If input 'u' is zoo: keep index
    if (inherits(u, "zoo")) zoo_index <- index(u)
    if (!all(c("u", "v") %in% names(u)))
      stop("necessary colums \"u\" and/or \"v\" missing")
    # If "v" is set in addition: warn
    if (!is.null(v)) {
      warning(sprintf("input \"u\" to uv2ddff is \"%s\":", class(u)),
              "\"v\" specified as well but will be ignored!")
    }
    v = as.numeric(u$v)
    u = as.numeric(u$u)
    # if u has 2 columns the second column is taken as v
  } else if (NCOL(u) == 2) {
    v <- u[,2]
    u <- u[,1]
  } else {
    if (is.null(v)) stop("input \"v\" missing")
    # If lenths do not match and none of them is of length 1: stop.
    if (!(length(u) == length(v)) & !any(c(length(u), length(v)) == 1L)) {
      stop("Length of \"u\" and \"v\" not identical")
      # Else recycle the one with length one to the length of the other one.
      # Damn it, so much one's in one sentence!
    } else if (length(u) == 1) {
      u <- rep(u, length(v))
    } else if (length(v) == 1) {
      v <- rep(v, length(u))
    }
  }
  # polar coordinates:
  ff <- sqrt(u^2 + v^2)
  dd <- atan(v/u) + (u < 0) * pi
  # Only non-na combis
  idx <- which(!is.na(dd) & !is.na(ff));   dd[idx] <- dd[idx] + 2 * pi
  # convert angle to meteorological convention
  dd <- 3 * pi / 2 - dd
  idx <- which(!is.na(dd) & !is.na(ff));   dd[idx] <- dd[idx] + 2 * pi
  # if rad (radiants) = F we have to convert to degrees.
  if (!rad) dd <- dd * 180 / pi
  res <- data.frame(dd, ff)
  if (is.null(zoo_index)) return(res) else return(zoo(res, zoo_index))
}

########################################################################
# 1. Import Wind Data (Downloaded as NetCDF grids from ECMWF) 
# Obtain wind dataset from ECMWF ERA5 (10m u component, 10m v component)
# Grid Extent: -30N -70S -80W -20E for Bird Island; 40N, 20S, 165W, -170E for Midway
# hourly, single level: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form
# monthly averaged, single level: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-monthly-means?tab=form
########################################################################
setwd("/Users/tziporahserota/Desktop/Thorne Lab/Analyses/Wind/Datasets/ERA5Datasets/")
wind <- read_ncdf("/Users/tziporahserota/Desktop/Thorne Lab/Analyses/Wind/Datasets/ERA5Datasets/2019_MonthlyAveraged.nc")
times<-st_get_dimension_values(wind, "time") # months
wind_u<-as(wind[1,,,], "Raster") # u-component
wind_v<-as(wind[2,,,], "Raster")# v-component 

########################################################################
# Option 1: Calculate annual average wind components
########################################################################
wind_u.df = raster::as.data.frame(wind_u, xy = TRUE) # convert to data frame 
colnames(wind_u.df) <- c("lon", "lat","January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December") # change column names

wind_v.df = raster::as.data.frame(wind_v, xy = TRUE) # convert to data frame 
colnames(wind_v.df) <- c("lon", "lat","January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December") # change column names


# annual average u and v component
wind_u.df$Avg_u <- rowMeans(wind_u.df[, c(-1, -2)])
wind_v.df$Avg_v <- rowMeans(wind_v.df[, c(-1, -2)])

wind_uAvg <- wind_u.df[, c(1, 2, 15)] # new dataframe with only averages
wind_vAvg <- wind_v.df[, c(1, 2, 15)]

wind_Avg <- full_join(wind_uAvg, wind_vAvg,by=c("lon", "lat")) # data frame of yearly averaged wind components

# Calculate wind speed 
# Requires mathematical to meteorological adjustment: use uv2ddff auxiliary function
# Important: our wind vectors were given in mathematical notation, not in meteorological convention. 
# Need to adjust by 270* - this is what uv2ddff does.
# http://colaweb.gmu.edu/dev/clim301/lectures/wind/wind-uv
# from uv2ddff: Wind direction is either in meteorological degrees (0 from North, from 90 East,
# 180 from South, and 270 from West) or in mathematical radiant if input \code{rad = TRUE}.

res<-uv2ddff(wind_Avg$Avg_u, wind_Avg$Avg_v)
wind_Avg$wind_speed<- res$ff

########################################################################
# Option 2: Calculate monthly average wind components
########################################################################
wind_u_NovDec2019 <- wind_u.df[,c(1, 2, 13, 14) ]
wind_v_NovDec2019 <- wind_v.df[,c(1, 2, 13, 14) ]


wind_u_NovDec2019$Avg_u <- rowMeans(wind_u_NovDec2019[, c(-1, -2)])
wind_v_NovDec2019$Avg_v <- rowMeans(wind_v_NovDec2019[, c(-1, -2)])


wind_u_NovDecAvg <- wind_u_NovDec2019[, c(1, 2, 5)] # new dataframe with only averages
wind_v_NovDecAvg <- wind_v_NovDec2019[, c(1, 2, 5)]

wind_NovDecAvg <- full_join(wind_u_NovDecAvg, wind_v_NovDecAvg,by=c("lon", "lat")) # data frame of averaged wind components Nov-Dec

res<-uv2ddff(wind_NovDecAvg$Avg_u, wind_NovDecAvg$Avg_v)
wind_NovDecAvg$wind_speed<- res$ff

########################################################################
# Option 3: Calculate wind components per month
########################################################################
wind_u_Nov2019 <- wind_u.df[,c(1, 2, 13) ]
wind_v_Nov2019 <- wind_v.df[,c(1, 2, 13) ]
wind_Nov2019 <- full_join(wind_u_Nov2019, wind_v_Nov2019,by=c("lon", "lat")) # join u and v components
colnames(wind_Nov2019) <- c("lon", "lat", "u", "v")
res<-uv2ddff(wind_Nov2019$u, wind_Nov2019$v)
wind_Nov2019$wind_speed<- res$ff

wind_u_Dec2019 <- wind_u.df[,c(1, 2, 14) ]
wind_v_Dec2019 <- wind_v.df[,c(1, 2, 14) ]
wind_Dec2019 <- full_join(wind_u_Dec2019, wind_v_Dec2019,by=c("lon", "lat")) # join u and v components
colnames(wind_Dec2019) <- c("lon", "lat", "u", "v")
res<-uv2ddff(wind_Dec2019$u, wind_Dec2019$v)
wind_Dec2019$wind_speed<- res$ff
########################################################################
# Plotting
########################################################################
# download, select, and clean data 
GPS <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data!/Conners_Analysis/Wind/wind_paired-data/2019-2020/bird-latlon-with-wind-bwa-state/ERA5_SingleLevels-10m/Appended/allaccwindstate_2019.csv")
GPS <- GPS[, 1:5]
GPS$lon <- GPS$lon-360

# Plot tracks of each species
GPS$labels <- GPS$species
GPS$labels <- factor(GPS$labels, levels = unique(GPS$species))
world<- map_data("world")

ggplot(wind_NovDecAvg,aes(lon,lat))+
  geom_raster(aes(fill=wind_speed),interpolate = T,alpha=.75)+
  scale_fill_viridis(name="Wind Speed")+
  scale_color_grey(name= "Species")+
  geom_path(data = GPS, mapping = aes(col = labels), size = 1) +
  geom_polygon(data=world,aes(x=long,y=lat, group=group),color='black',fill=NA)+
  coord_sf(xlim = c(-85, -20), ylim = c(-70, -40)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("2019 Tracks")+
  theme_bw()

dropdir <- '/Users/tziporahserota/Desktop/Thorne Lab/Analyses/Wind/Wind_maps/Maps/2019 Maps/'
ggsave(paste0(dropdir,'Tracks_2019speed.png'))


# only wind speed maps
ggplot(wind_Nov2019,aes(lon,lat))+
  geom_raster(aes(fill=wind_speed),interpolate = T,alpha=.75)+
  scale_fill_viridis(name="Wind Speed")+
  geom_polygon(data=world,aes(x=long,y=lat, group=group),color='black',fill=NA)+
  coord_sf(xlim = c(-85, -20), ylim = c(-70, -40)) +
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()

dropdir <- '/Users/tziporahserota/Desktop/Thorne Lab/Analyses/Wind/Wind_maps/Maps/2019 Maps/'
ggsave(paste0(dropdir,'NovAvgWindspeed.png'))