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
# Download and Import data
#################################################################################
# -80, -70, -20, -30 Extent

# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
#  Useful Links
# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
# File specification: https://gmao.gsfc.nasa.gov/pubs/docs/Bosilovich785.pdf
# How to download the data: https://daac.gsfc.nasa.gov/information/howto?title=How%20to%20Download%20MERRA-2%20Daily%20Mean%20Data
# How to calculate and plot wind speed using MERRA-2 wind component: https://disc.gsfc.nasa.gov/information/howto?title=How%20to%20calculate%20and%20plot%20wind%20speed%20using%20MERRA-2%20wind%20component%20data%20using%20Python
# Hourly Data: https://disc.gsfc.nasa.gov/datasets/M2I1NXASM_5.12.4/summary


setwd("/Users/tziporahserota/Desktop/Merra-2Datasets/Nov-Jan2019_hourly/")
a <- list.files("/Users/tziporahserota/Desktop/Merra-2Datasets/Nov-Jan2019_hourly/", pattern = ".nc4")

# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
#  Create list of times
# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
for (i in 1:length(a)) {
  mi<-read_ncdf(a[i])
  times <- st_get_dimension_values(mi, "time")
  if (i==1) {
    times_all<-times
  }else{
    times_all<-c(times_all,times)
  }
}

all_times<-as.tibble(times_all)
all_times.name<-as.character(all_times)
all_times_num<-as.numeric(unlist(all_times))

# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
#  Isolate U and V components for 2 and 10 m 
# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
# U-component, 2 m 
for (i in 1:length(a)) {
  mi<-read_ncdf(a[i])
  wind_u<-as(mi[1,,,], "Raster")
  wind_u.df= raster::as.data.frame(wind_u, xy = TRUE)
  if (i==1) {
    wind_U2M<-wind_u.df
  }else{
    wind_U2M<-cbind(wind_U2M,wind_u.df[, -c(1,2)])
  }
}

# V-component, 2 m 
for (i in 1:length(a)) {
  mi<-read_ncdf(a[i])
  wind_v<-as(mi[3,,,], "Raster")
  wind_v.df= raster::as.data.frame(wind_v, xy = TRUE)
  if (i==1) {
    wind_V2M<-wind_v.df
  }else{
    wind_V2M<-cbind(wind_V2M,wind_v.df[, -c(1,2)])
  }
}


# U-component, 10 m 
for (i in 1:length(a)) {
  mi<-read_ncdf(a[i])
  wind_u<-as(mi[6,,,], "Raster")
  wind_u.df= raster::as.data.frame(wind_u, xy = TRUE)
  if (i==1) {
    wind_U10M<-wind_u.df
  }else{
    wind_U10M<-cbind(wind_U10M,wind_u.df[, -c(1,2)])
  }
}

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


u_stack2M<-rasterFromXYZ(wind_U2M)
v_stack2M<-rasterFromXYZ(wind_V2M)
u_stack10M<-rasterFromXYZ(wind_U10M)
v_stack10M<-rasterFromXYZ(wind_V10M)

####################################################################################################
# 2. Read in hourly bird locations and gather wind data
####################################################################################################
int_now <- 3600

setwd(paste0('/Volumes/GoogleDrive/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data!/Conners_Bird_Island/2019_2020/Tag_Data/L1_cleaned-data/GPS_L1_3_interpolated/', int_now, 's/'))
files<-list.files(pattern = ".csv")

# Append all individual bird lat lon files to one file ------
for (i in 1:length(files)) {
  mi<-read.csv(files[i])
  if (i==1) {
    m<-mi
  }else{
    m<-rbind(m,mi)
  }
} 

# Add speed, distance, bearing columns to m -----------------
m$ground_speed_kmHr <- NA
m$distanceij_km <- NA
m$bearingij <- NA
m$U2M<- NA
m$V2M<- NA
m$U10M<- NA
m$V10M<- NA
m$wind_speed2M <- NA
m$wind_speed10M <- NA


trips<- unique(m$tripID)
for (i in 1:length(trips)) {
  tripi<-m[m$tripID==trips[i],]
  
  for (j in 1:length(tripi$id)-1) {
    hour_int<- int_now/3600 # int_now is in seconds (3600 seconds in one hour)
    tripi$distanceij_km[j]<-swfscMisc::distance(tripi$lat[j],tripi$lon[j],tripi$lat[j+1],tripi$lon[j+1], units = "km", method = "haversine")
    tripi$ground_speed_kmHr[j]<-tripi$distanceij_km[j]/hour_int    
    tripi$bearingij[j]<-as.numeric(bearing(tripi$lat[j],tripi$lon[j],tripi$lat[j+1],tripi$lon[j+1])[1])
  }
  
  m[m$tripID==trips[i],]<-tripi
  
}

# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
# Loop through m and add wind information: u, v, velocity for 2M and 10M
# ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
for ( j in 1:length(m$id)) {
  
  timej <- as.POSIXct(m$datetime[j], format = "%Y-%m-%d %H:%M:%S" , tz = "UTC")
  timej_num <- as.numeric(timej)
  # Find index of current_time in all times. Use that index to pull out relevant raster layer. 
  raster_dt_index <- as.numeric(which(abs(all_times_num-timej_num) == min(abs(all_times_num-timej_num))))
  
  # Isolate u and v rasters at time j
  ustack_timej2 <- subset(u_stack2M, raster_dt_index, drop=TRUE)
  vstack_timej2 <- subset(v_stack2M, raster_dt_index, drop=TRUE)
  ustack_timej10 <- subset(u_stack10M, raster_dt_index, drop=TRUE)
  vstack_timej10 <- subset(v_stack10M, raster_dt_index, drop=TRUE)
  
  # isolate coordinates
  xy_j <- as.data.frame(cbind(m$lon[j], m$lat[j]))
  colnames(xy_j) <- c("lon","lat")
  xy_j$lon <- Lon360to180(xy_j$lon) # not necessary for midway
  
  # Extract u and v components for time j at location x and y
  u_j2 <- extract(ustack_timej2, xy_j)
  v_j2 <- extract(vstack_timej2, xy_j)
  m$U2M[j]<- u_j2
  m$V2M[j]<- v_j2
  
  u_j10 <- extract(ustack_timej10, xy_j)
  v_j10 <- extract(vstack_timej10, xy_j)
  m$U10M[j]<- u_j10
  m$V10M[j]<- v_j10
  # -----------------------------------------------------------------------------
  # Get Wind Direction and Wind Velocity from U and V Components:
  # -----------------------------------------------------------------------------
  # Requires mathematical to meteorological adjustment:
  # Important: our wind vectors were given in mathematical notation, not in meteorological convention. 
  # Need to adjust by 270* - this is what uv2ddff does.
  # http://colaweb.gmu.edu/dev/clim301/lectures/wind/wind-uv
  # from uv2ddff: Wind direction is either in meteorological degrees (0 from North, from 90 East,
  # 180 from South, and 270 from West) or in mathematical radiant if input \code{rad = TRUE}.
  
  res2M<-uv2ddff(u_j2, v_j2)
  m$wind_speed2M[j] <- res2M$ff
  
  res10M<-uv2ddff(u_j10, v_j10)
  m$wind_speed10M[j] <- res10M$ff
}

spvec <- substr(m$id,1,4)
m<-m %>% mutate(species=spvec)

write_csv(m, '/Users/tziporahserota/Desktop/Thorne Lab/Analyses/Wind/wind_paired-data/2019-2020/hourly/allbirds_hourly.csv')


####################################################################################################
# 3. Interpolate wind data for 30 sec data
####################################################################################################

# Import 30s lat lon data and append individual bird files into one file. 
int_now <- 30

setwd(paste0('/Volumes/GoogleDrive/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data!/Conners_Bird_Island/2019_2020/Tag_Data/L1_cleaned-data/GPS_L1_3_interpolated/', int_now, 's/'))
files<-list.files(pattern=".csv")

# Append all individual bird lat lon files to one file ------
for (i in 1:length(files)) {
  mi<-read.csv(files[i])
  if (i==1) {
    m_hi<-mi
  }else{
    m_hi<-rbind(m_hi,mi)
  }
} 

# placeholders 
m_hi$distanceij_km <- NA
m_hi$ground_speed_kmHr <- NA
m_hi$bearingij <- NA
m_hi$U2M<- NA
m_hi$V2M<- NA
m_hi$U10M<- NA
m_hi$V10M<- NA
m_hi$wind_speed2M <- NA
m_hi$wind_speed10M <- NA
m_hi$windshear <- NA

# -----------------------------------------------------------------------------
# Add Bird Speed, Distance, Bearing
# -----------------------------------------------------------------------------

trips<- unique(m_hi$tripID)
for (i in 1:length(trips)) {
  tripi<-m_hi[m_hi$tripID==trips[i],]
  
  for (j in 1:length(tripi$id)-1) {
    hour_int<- int_now/3600 # int_now is in seconds (3600 seconds in one hour)
    tripi$distanceij_km[j]<-swfscMisc::distance(tripi$lat[j],tripi$lon[j],tripi$lat[j+1],tripi$lon[j+1], units = "km", method = "haversine")
    tripi$ground_speed_kmHr[j]<-tripi$distanceij_km[j]/hour_int
    tripi$bearingij[j]<-as.numeric(bearing(tripi$lat[j],tripi$lon[j],tripi$lat[j+1],tripi$lon[j+1])[1])
  }
  
  m_hi[m_hi$tripID==trips[i],]<-tripi
  
}

# -----------------------------------------------------------------------------
# Add Wind Metrics
# -----------------------------------------------------------------------------

# Import hourly lat lon with wind u and v
m_lo <- m
birds<-unique(m_lo$id)

for (i in 1:length(birds)) {
  birdi<-birds[i]
  
  mi_lo = m_lo %>% filter(id == birds[i])
  mi_hi = m_hi %>% filter(id == birds[i])
  
  # Match times in datasets and extract u and v into hi res dataset every hour from hourly dataset
  hi_res_timematch<-as.numeric(as.POSIXct(as.character(mi_hi$datetime)))
  
  for (j in 1:length(mi_lo$id)) {
    j_match<-as.numeric(as.POSIXct(as.character(mi_lo$datetime[j])))
    match_ix<-Closest( hi_res_timematch, j_match, which=TRUE) # Which in m_hi is nearest j_match (m$datetime[j])
    mi_hi$U2M[match_ix] <- mi_lo$U2M[j]
    mi_hi$V2M[match_ix] <- mi_lo$V2M[j]
    mi_hi$U10M[match_ix] <- mi_lo$U10M[j]
    mi_hi$V10M[match_ix] <- mi_lo$V10M[j]
  }
  rm(j)
  # Interpolate between hourly u and v
  # !!!! Here, used linear, but may want to explore other options:
  # such as Guassian processes (recommended by Petar Djuric) >> ask Levi, Andy?
  mi_hi$U2M<-na_interpolation(mi_hi$U2M, option = "linear")
  mi_hi$V2M<-na_interpolation(mi_hi$V2M, option = "linear")
  mi_hi$U10M<-na_interpolation(mi_hi$U10M, option = "linear")
  mi_hi$V10M<-na_interpolation(mi_hi$V10M, option = "linear")
  
  for (j in 1:length(mi_hi$id)) {
    # Calculate Wind Speed and DIrection from u and v and BWA, BWAClass
    # uv2ddff for wind direction and velocity
    ddff2M <- uv2ddff(mi_hi$U2M[j],mi_hi$V2M[j])
    mi_hi$wind_speed2M[j] <- ddff2M$ff
    
    ddff10M <- uv2ddff(mi_hi$U10M[j],mi_hi$V10M[j])
    mi_hi$wind_speed10M[j] <- ddff10M$ff
    
    mi_hi$windshear[j]<-  mi_hi$wind_speed10M[j]- mi_hi$wind_speed2M[j]
  }
  spvec<-substr(birdi, 1, 4)
  mi_hi$species<-spvec
  
  
  # write individual bird file
  dir_i<-paste0('/Users/tziporahserota/Desktop/Thorne Lab/Analyses/Wind/wind_paired-data/2019-2020/indiv_birds/', birdi, '_wind_bwa.csv')
  write_csv(mi_hi, dir_i)
  
  rm(list=ls()[! ls() %in% c("wrap360", "uv2ddff", "Lon360to180", "birds", "m_lo", "m_hi", "i", "int_now")])
  
}