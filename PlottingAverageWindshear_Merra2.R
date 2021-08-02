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

#################################################################################
# Download and Import data
#################################################################################
# -80, -70, -20, -30 Extent

# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
#  Useful Links
# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
# File specification: https://gmao.gsfc.nasa.gov/pubs/docs/Bosilovich785.pdf
# How to download the data: https://daac.gsfc.nasa.gov/information/howto?title=How%20to%20Download%20MERRA-2%20Daily%20Mean%20Data
# How to calculate and plot wind speed using MERRA-2 wind component: https://disc.gsfc.nasa.gov/information/howto?title=How%20to%20calculate%20and%20plot%20wind%20speed%20using%20MERRA-2%20wind%20component%20data%20using%20Python
# Monthly averages: https://disc.gsfc.nasa.gov/datasets/M2IMNXASM_5.12.4/summary
########################################################################

setwd("/Users/tziporahserota/Desktop/Thorne Lab/Analyses/Wind/Datasets/Merra-2Datasets/Nov2019-Jan2020_monthly/")
a <- list.files("/Users/tziporahserota/Desktop/Thorne Lab/Analyses/Wind/Datasets/Merra-2Datasets/Nov2019-Jan2020_monthly/", pattern = ".nc")

# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
#  Isolate U and V components for 2 and 10 m 
# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
# U-component, 2 m 
col <- c("lon", "lat", "November", "December", "January", "February")

for (i in 1:length(a)) {
  mi<-read_ncdf(a[i])
  wind_u<-as(mi[2,,,], "Raster")
  wind_u.df= raster::as.data.frame(wind_u, xy = TRUE)
  if (i==1) {
    wind_U2M<-wind_u.df
  }else{
    wind_U2M<-cbind(wind_U2M,wind_u.df[, -c(1,2)])
  }
}
colnames(wind_U2M) <- col
  
  
# V-component, 2 m 
for (i in 1:length(a)) {
  mi<-read_ncdf(a[i])
  wind_v<-as(mi[5,,,], "Raster")
  wind_v.df= raster::as.data.frame(wind_v, xy = TRUE)
  if (i==1) {
    wind_V2M<-wind_v.df
  }else{
    wind_V2M<-cbind(wind_V2M,wind_v.df[, -c(1,2)])
  }
}
colnames(wind_V2M) <- col

# U-component, 10 m 
for (i in 1:length(a)) {
  mi<-read_ncdf(a[i])
  wind_u<-as(mi[1,,,], "Raster")
  wind_u.df= raster::as.data.frame(wind_u, xy = TRUE)
  if (i==1) {
    wind_U10M<-wind_u.df
  }else{
    wind_U10M<-cbind(wind_U10M,wind_u.df[, -c(1,2)])
  }
}
colnames(wind_U10M) <- col

# V-component, 10 m 
for (i in 1:length(a)) {
  mi<-read_ncdf(a[i])
  wind_v<-as(mi[4,,,], "Raster")
  wind_v.df= raster::as.data.frame(wind_v, xy = TRUE)
  if (i==1) {
    wind_V10M<-wind_v.df
  }else{
    wind_V10M<-cbind(wind_V10M,wind_v.df[, -c(1,2)])
  }
}
colnames(wind_V10M) <- col

########################################################################
# Calculate monthly wind speed and wind shear 
########################################################################
# November only 
wind_U2MAvg.Nov <- wind_U2M[, c(1, 2, 3)] # pull out lon, lat, November values
wind_V2MAvg.Nov <- wind_V2M[, c(1, 2, 3)]
wind_U10MAvg.Nov <- wind_U10M[, c(1, 2, 3)] 
wind_V10MAvg.Nov <- wind_V10M[, c(1, 2, 3)]

wind_Avg.Nov2M<- full_join(wind_U2MAvg.Nov, wind_V2MAvg.Nov,by=c("lon", "lat")) 
wind_Avg.Nov10M <- full_join(wind_U10MAvg.Nov, wind_V10MAvg.Nov, by=c("lon", "lat"))

colnames(wind_Avg.Nov2M) <- c("lon", "lat", "U2M", "V2M")
colnames(wind_Avg.Nov10M) <- c("lon", "lat", "U10M", "V10M")

res.2M<-uv2ddff(wind_Avg.Nov2M$U2M, wind_Avg.Nov2M$V2M)
wind_Avg.Nov2M$windspeed_2M <- res.2M$ff

res.10M<-uv2ddff(wind_Avg.Nov10M$U10M, wind_Avg.Nov10M$V10M)
wind_Avg.Nov10M$windspeed_10M <- res.10M$ff

windshear.Nov <- full_join(wind_Avg.Nov2M, wind_Avg.Nov10M, by=c("lon", "lat"))
windshear.Nov <- windshear.Nov[, c(1,2, 5, 8)]
windshear.Nov$windshear <- windshear.Nov$windspeed_10M- windshear.Nov$windspeed_2M
windshear.Nov <- windshear.Nov[, c(1, 2, 5)]

# December only
wind_U2MAvg.Dec <- wind_U2M[, c(1, 2, 4)] # pull out lon, lat, November values
wind_V2MAvg.Dec <- wind_V2M[, c(1, 2, 4)]
wind_U10MAvg.Dec <- wind_U10M[, c(1, 2, 4)] 
wind_V10MAvg.Dec <- wind_V10M[, c(1, 2, 4)]

wind_Avg.Dec2M<- full_join(wind_U2MAvg.Dec, wind_V2MAvg.Dec,by=c("lon", "lat")) 
wind_Avg.Dec10M <- full_join(wind_U10MAvg.Dec, wind_V10MAvg.Dec, by=c("lon", "lat"))

colnames(wind_Avg.Dec2M) <- c("lon", "lat", "U2M", "V2M")
colnames(wind_Avg.Dec10M) <- c("lon", "lat", "U10M", "V10M")

res.2M<-uv2ddff(wind_Avg.Dec2M$U2M, wind_Avg.Dec2M$V2M)
wind_Avg.Dec2M$windspeed_2M <- res.2M$ff

res.10M<-uv2ddff(wind_Avg.Dec10M$U10M, wind_Avg.Dec10M$V10M)
wind_Avg.Dec10M$windspeed_10M <- res.10M$ff

windshear.Dec <- full_join(wind_Avg.Dec2M, wind_Avg.Dec10M, by=c("lon", "lat"))
windshear.Dec <- windshear.Dec[, c(1,2, 5, 8)]
windshear.Dec$windshear <- windshear.Dec$windspeed_10M- windshear.Dec$windspeed_2M
windshear.Dec <- windshear.Dec[, c(1, 2, 5)]


# November - December, averaged
windshear <- full_join(windshear.Nov, windshear.Dec,by=c("lon", "lat"))
windshear$avg_windshear <- rowMeans(windshear[, c(-1, -2)])
windshear <- windshear[, c(1,2, 5)]
colnames(windshear) <- c("lon", "lat", "windshear")

########################################################################
# Plotting shear and tracks over shear 
########################################################################
# download, select, and clean GPS data 
GPS <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data!/Conners_Analysis/Wind/wind_paired-data/2019-2020/bird-latlon-with-wind-bwa-state/ERA5_SingleLevels-10m/Appended/allaccwindstate_2019.csv")
GPS <- GPS[, 1:5]
GPS$lon <- GPS$lon-360

# Plot tracks of each species
GPS$labels <- GPS$species
GPS$labels <- factor(GPS$labels, levels = unique(GPS$species))
world<- map_data("world")

ggplot(windshear,aes(lon,lat))+
  geom_raster(aes(fill=windshear),interpolate = T,alpha=.75)+
  scale_fill_viridis(name="Wind Shear")+
  scale_color_grey(name= "Species")+
  geom_path(data = GPS, mapping = aes(col = labels), size = 1) +
  geom_polygon(data=world,aes(x=long,y=lat, group=group),color='black',fill=NA)+
  coord_sf(xlim = c(-85, -20), ylim = c(-70, -40)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("2019 Tracks")+
  theme_bw()

dropdir <- '/Users/tziporahserota/Desktop/Thorne Lab/Analyses/Wind/Wind_maps/'
ggsave(paste0(dropdir,'Tracks_2019shear.png'))


# only wind shear maps
ggplot(windshear,aes(lon,lat))+
  geom_raster(aes(fill=windshear),interpolate = T,alpha=.75)+
  scale_fill_viridis(name="Wind Shear")+
  geom_polygon(data=world,aes(x=long,y=lat, group=group),color='black',fill=NA)+
  coord_sf(xlim = c(-85, -20), ylim = c(-70, -40)) +
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()

dropdir <- '/Users/tziporahserota/Desktop/Thorne Lab/Analyses/Wind/Wind_maps/'
ggsave(paste0(dropdir,'2019AvgWindshear.png'))