library(tidyr)
library(dplyr)

###############################################################################
# 1. Append Data for each dataset and Define breeding phase 
###############################################################################
# -----------------------------------
# a. 2020-2021 Bird Island
# -----------------------------------
# append 30s
setwd("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2020-2021/indiv_birds/")
a <- list.files(pattern = ".csv")
path_out <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2020-2021/indiv_birds/appended/"

for (i in 1: length(a)) {
  m <- read.csv(a[i])
  if (i==1) {
    mat <- m 
  }else{
    mat <- rbind(mat,m)
  }
}

write.csv(mat, file = paste0(path_out, "Wind30sAppended_2020.csv"), row.names=FALSE)

# Define breeding phase, location, season
m <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2020-2021/indiv_birds/appended/Wind30sAppended_2020.csv")
meta <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Conners_Bird_Island/2020_2021/Metadata/Deployment Meta Data/BirdIsland2021_Deployment_Data.csv")

m$season <- "2020-2021"
m$location <- "Bird Island"

n_rows <- thirty %>% count(id)
colnames(n_rows) <- c("Deployment_ID", "n")

meta_filter <- meta %>% select(Deployment_ID, Capture_Nest_Contents)
match <- na.omit(left_join(meta_filter, n_rows))
match$phase <- NA

match$phase <-  ifelse(match$Capture_Nest_Contents== "C","Brood-guard", "Incubation" )
n_occur <- match$n
n_phase <- match$phase


df <- uncount(match, n)
colnames(df) <- "phase"

m <- data.frame(thirty,df)
write.csv(m, file = paste0(path_out, "Wind30sAppendedWithPhase_2020.csv"), row.names=FALSE)

# -----------------------------------
# b. 2019-2020 Bird Island
# -----------------------------------
setwd("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2019-2020/indiv_birds/")
a <- list.files(pattern = ".csv")
path_out <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2019-2020/indiv_birds/appended/"

# Append 30s 
for (i in 1: length(a)) {
  m <- read.csv(a[i])
  if (i==1) {
    mat <- m 
  }else{
    mat <- rbind(mat,m)
  }
}

write.csv(mat, file = paste0(pathout, "Wind30sAppended_2019.csv"), row.names=FALSE)

# Define breeding phase, location, season
m <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2019-2020/indiv_birds/appended/Wind30sAppended_2019.csv")
meta <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Conners_Bird_Island/2019_2020/Metadata/Metadata_Deployment-Data/L1_1_deployment-data_PROOFED_with-masses/BirdIsland_2019-2020_BreedingPhase.csv")

m$season <- "2019-2020"
m$location <- "Bird Island"

n_rows <- m %>% count(id)
colnames(n_rows) <- c("Deployment_ID", "n")
n_rows$Num <- str_split_fixed(n_rows$Deployment_ID, "_", 2)[,2]
n_rows$Num = str_remove(n_rows$Num, "^0+")

match <- na.omit(left_join(meta, n_rows))

df <- uncount(match, n)
df <-data.frame( df[, 3:4])
colnames(df) <- c("phase", "id")
df <- arrange(df, df$id)

m <- data.frame(m,df)
m <- m[,1:20]

write.csv(m, file = paste0(path_out, "Wind30sAppendedWithPhase_2019.csv"), row.names=FALSE)

# -----------------------------------
# c. 2019-2020 Bird Island, WAAL, incubation
# -----------------------------------
# append 30s
setwd("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2019-2020_WAAL_incubation/indiv_birds/")
pathout <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2019-2020_WAAL_incubation/indiv_birds/appended/"

a <- list.files(pattern = ".csv")

for (i in 1: length(a)) {
  m <- read.csv(a[i])
  if (i==1) {
    mat <- m 
  }else{
    waal_inc <- rbind(mat,m)
  }
}
write.csv(waal_inc, file = paste0(pathout, "Wind30sAppended_2019WAAL.csv"), row.names=FALSE)


# Define breeding phase, location, season
waal_inc$phase <- "Incubation"
waal_inc$season <- "2019-2020"
waal_inc$location <- "Bird Island"

write.csv(waal_inc, file = paste0(path_out, "Wind30sAppendedWithPhase_2019WAAL.csv"), row.names=FALSE)

# -----------------------------------
# d. 2018-2019 Bird Island, WAAL, brood-guard
# -----------------------------------
# append 30s
setwd("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2019-2020_WAAL_brood-guard/indiv_birds/")
a <- list.files(pattern = ".csv")
path_out <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2019-2020_WAAL_brood-guard/indiv_birds/appended/"

for (i in 1: length(a)) {
  m <- read.csv(a[i])
  if (i==1) {
    mat <- m 
  }else{
    waal_brd <- rbind(mat,m)
  }
}
write.csv(waal_inc, file = paste0(pathout, "Wind30sAppended_2018WAAL.csv"), row.names=FALSE)

# Define breeding phase, location, season
waal_brd$phase <- "Brood-guard"
waal_brd$season <- "2018-2019"
waal_brd$location <- "Bird Island"
write.csv(waal_brd, file = paste0(path_out, "Wind30sAppendedWithPhase_2018WAAL.csv"), row.names=FALSE)

# -----------------------------------
# e. 2018-2019 Bird Island, BBAL brood-guard
# -----------------------------------
setwd("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2018-2019_BBAL/indiv_birds/")
a <- list.files(pattern = ".csv")
path_out <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2018-2019_BBAL/indiv_birds/appended/"

m$season <- "2018-2019"
m$location <- "Bird Island"

for (i in 1: length(a)) {
  m <- read.csv(a[i])
  if (i==1) {
    mat <- m 
  }else{
    bbal_brd <- rbind(mat,m)
  }
}

write.csv(bbal_brd, file = paste0(path_out, "Wind30sAppended_2018BBAL.csv"), row.names=FALSE)

# Define breeding phase, location, season
bbal_brd$phase <- "Brood-guard"
write.csv(bbal_brd, file = paste0(path_out, "Wind30sAppendedWithPhase_2018BBAL.csv"), row.names=FALSE)

# -----------------------------------
# f. 2018-2019 Midway
# -----------------------------------
# append 30s
setwd("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2018-2019_Midway/indiv_birds/")
a <- list.files(pattern = ".csv")
path_out <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2018-2019_Midway/indiv_birds/appended/"

# Append 30s 
for (i in 1: length(a)) {
  m <- read.csv(a[i])
  if (i==1) {
    mat <- m 
  }else{
    mat <- rbind(mat,m)
  }
}

write.csv(mat, file = paste0(path_out, "Wind30sAppended_2018-2019Midway.csv"), row.names=FALSE)

# Define breeding phase, location, season
m <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2018-2019_Midway/indiv_birds/appended/Wind30sAppended_2018-2019Midway.csv")
meta <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Conners_Midway/2018-2019/Metadata/Deployment_Data/Midway2019_AlbatrossDeployments_04172019_transposed.csv")

m$season <- "2018-2019"
m$location <- "Midway"

n_rows <- m %>% count(id)
colnames(n_rows) <- c("Deployment_ID", "n")
n_rows$bird_num <- str_split_fixed(n_rows$Deployment_ID, "_", 2)[,2]
n_rows$bird_num  = str_remove(n_rows$bird_num , "^0+")


match <- na.omit(full_join(meta, n_rows))
match$phase <- NA

match$phase <-  ifelse(match$deploy_nest_content== "Chick","Brood-guard", "Incubation" )

df <- uncount(match, n)
df <-data.frame( df[, 51:52])
colnames(df) <- c("Deployment_ID", "phase")
df <- arrange(df, df$id)



m <- data.frame(m,df)
m <- m[,1:20]
m$spp_phase <- str_c(m$species, m$phase, sep = "_")
m$bwa_class<-as.factor(m$bwa_class)

write.csv(m, file = paste0(path_out, "Wind30sAppendedWithPhase_2018-2019Midway.csv"), row.names=FALSE)


###############################################################################
# Append all 30s data from all datasets into one file 
###############################################################################
setwd("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/L1_appended/L1_1_30sAppended_byseason/")
a <- list.files(pattern = ".csv")
path_out <- "/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/L1_appended/L1_2_30sAppended/"

# Append 30s 
for (i in 1: length(a)) {
  m <- read.csv(a[i])
  if (i==1) {
    mat <- m 
  }else{
    mat <- rbind(mat,m)
  }
}

write.csv(mat, file = paste0(path_out, "Wind30sAppended_2018-2019Midway.csv"), row.names=FALSE)