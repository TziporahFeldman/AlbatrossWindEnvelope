########################################################################
# Load Libraries
########################################################################
library(ggplot2) # plots 
library(tidyverse) # data cleanup
library(lubridate) # Clean up dates

########################################################################
# Prep Data
########################################################################
# -----------------------------------
# Append 30s data for individual birds into one file
# -----------------------------------
setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2019-2020/indiv_birds/")
a <- list.files(pattern = ".csv")
path_out <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2019-2020/indiv_birds/Appended/"

# Append 30s 
for (i in 1: length(a)) {
  m <- read.csv(a[i])
  if (i==1) {
    mat <- m 
  }else{
    mat <- rbind(mat,m)
  }
}

write.csv(mat, file = paste0(path_out, "allaccwindstate_2019.csv"), row.names=FALSE)


MERRA <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2019-2020/indiv_birds/Appended/allaccwindstate_2019.csv")

# Download 2019-2020 GPS/wind/HMM state file 
ERA <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Conners_Analysis/Wind/wind_paired-data/2019-2020/bird-latlon-with-wind-bwa-state/data_30s/ERA5_SingleLevels-10m/Appended/allaccwindstate30s_2019_withphase.csv")
ERA$state <- as.character(ERA$state) 
ERA$date <- substr(ERA$datetime, 1, 10)
ERA$time <- substr(ERA$datetime, 12, 19)
ERA$datetime <- paste(ERA$date, ERA$time)
ERA$datetime <- as.POSIXct(ERA$datetime, tz="UTC")
ERA <- ERA[, 1:21]

# -----------------------------------
# Match to HMM state
# -----------------------------------

Compare <- inner_join(ERA, MERRA, by=c("datetime", "lon", "lat", "id", "ground_speed_kmHr", "tripID", "distanceij_km", "bearingij", "species"))
Compare <- Compare[,c(1:5, 12:15, 19:21, 26:32)]
Compare$bwa_class.y[1] <- "Cross-Wind"

###############################################################################
# Compare
############################################################################### 
Compare$speed_difference <- Compare$wind_speed10M- Compare$wind_speed
Compare$dir_difference <- Compare$wind_dir36010M- Compare$wind_dir360
Compare$bwa_difference <- Compare$bwa.y- Compare$bwa.x
Compare$bwa_compare <- Compare$bwa_class.y==Compare$bwa_class.x

hist(Compare$bwa_difference)
barchart(Compare$bwa_compare)
