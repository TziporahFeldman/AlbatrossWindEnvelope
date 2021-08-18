########################################################################
# Load Libraries
########################################################################
library(ggplot2) # plots 
library(tidyverse) # data cleanup
library(lubridate) # Clean up dates
library(mgcv) # GAMs
library(MASS) # GLMMs
library(lme4) # GLMMs
library(sjPlot)

########################################################################
# Load and clean up data
########################################################################
#  Download 2019-2020 GPS/wind MERRA-2 file 
MERRA <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Feldman_Analysis/Wind/wind_paired-data/2019-2020/indiv_birds/Appended/allaccwindstate_2019.csv")
MERRA$bwa_class[1] <- "Cross-Wind"
MERRA$datetime <- as.POSIXct(MERRA$datetime, tz="UTC")

# Download 2019-2020 GPS/wind/HMM state file 
ERA <- read.csv("/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data/Conners_Analysis/Wind/wind_paired-data/2019-2020/bird-latlon-with-wind-bwa-state/data_30s/ERA5_SingleLevels-10m/Appended/allaccwindstate30s_2019_withphase.csv")
ERA$state <- as.character(ERA$state) 
ERA$date <- substr(ERA$datetime, 1, 10)
ERA$time <- substr(ERA$datetime, 12, 19)
ERA$datetime <- paste(ERA$date, ERA$time)
ERA$datetime <- as.POSIXct(ERA$datetime, tz="UTC")

ERA <- ERA[,c( 1:6, 19:21)]

# -----------------------------------
# Match to HMM state
# -----------------------------------

MERRA <- inner_join(ERA, MERRA, by=c("datetime", "lon", "lat", "id", "tripID", "species"))


rm(list=setdiff(ls(), "MERRA"))

MERRA$soaring <-  ifelse(MERRA$state== "2","1", "0" )
MERRA$soaring <- as.numeric(MERRA$soaring)
MERRA_sample <- MERRA[1:5000,]
###############################################################################
# Exploratory Graphs 
###############################################################################
dropdir <- "/Users/tziporahserota/Desktop/Thorne Lab/Analyses/Wind/Wind_maps/Figures/Graphs/Exploratory/"
attach(MERRA)
# boxplot

png(file=paste0(dropdir, 'WindshearSoaring_boxplot.png'))
boxplot(windshear~soaring, varwidth=TRUE)
dev.off()

png(file=paste0(dropdir, 'WindspeedSoaring_boxplot.png'))
boxplot(wind_speed2M~soaring, varwidth=TRUE)
dev.off()

png(file=paste0(dropdir, 'WindshearState_boxplot.png'))
boxplot(windshear~state, varwidth=TRUE)
dev.off()

png(file=paste0(dropdir, 'WindspeedState_boxplot.png'))
boxplot(wind_speed2M~state, varwidth=TRUE)
dev.off()


# scatterplot
png(file=paste0(dropdir, 'WindshearSoaring_scatterplot.png'))
plot(windshear, soaring)
dev.off()

png(file=paste0(dropdir, 'WindspeedSoaring_scatterplot.png'))
plot(wind_speed2M, soaring)
dev.off()
###############################################################################
# Fixed Model 
###############################################################################
dropdir <- "/Users/tziporahserota/Desktop/Thorne Lab/Analyses/Wind/Wind_maps/Figures/Graphs/Models/"
# -----------------------------------
# GLM
# -----------------------------------
attach(MERRA)
# wind speed only
fit <- glm(soaring~wind_speed2M, family = "binomial")
summary(fit)
MyData<-data.frame(wind_speed2M=seq(min(wind_speed2M),max(wind_speed2M)))
Pred <- predict(fit, newdata = MyData, type = "response")

png(file=paste0(dropdir, 'WindspeedSoaring_model.png'))
plot(x = MERRA$wind_speed2M, y = MERRA$soaring, xlab="Wind Speed", ylab="Soaring")
lines(MyData$wind_speed2M, Pred)
dev.off()

# wind shear only
fit2 <- glm(soaring~windshear, family = "binomial")
summary(fit2)
MyData<-data.frame(windshear=seq(min(windshear),max(windshear)))
Pred <- predict(fit2, newdata = MyData, type = "response")

png(file=paste0(dropdir, 'WindshearSoaring_model.png'))
plot(x = MERRA$windshear, y = MERRA$soaring, xlab="Wind Shear", ylab="Soaring")
lines(MyData$windshear, Pred)
dev.off()

# bivariate, no interaction
fit3 <- glm(soaring~windshear+wind_speed2M+ fspecies, family = "binomial")
summary(fit3)

op <- par(mfrow=c(2,2))
plot(fit3)
par(op)

# bivariate, interaction
fit4 <- glm(soaring~windshear*wind_speed2M+ fspecies, family = "binomial")
summary(fit4)
step(fit4)

op <- par(mfrow=c(2,2))
plot(fit4)
par(op)

# -----------------------------------
# GAM
# -----------------------------------
fit_gamm <- gamm(soaring~ s(windshear)+ s(wind_speed2M)+ fspecies, family = binomial)
summary(fit_gamm)
plot(fit_gamm$gam)

###############################################################################
# Mixed Models 
###############################################################################
# -----------------------------------
# center data 
# -----------------------------------


# -----------------------------------
# GLMM
# -----------------------------------
MERRA$fid <- factor(id)
MERRA$fspecies <- factor(species)
MERRA$fwinshear <- factor(windshear)
# fixed
fit_fixed <- glm(soaring~windshear*wind_speed2M+fid, 
                 family = "binomial")
summary(fit_fixed)

# MASS
fit_mixed <- glmmPQL(soaring~windshear*wind_speed2M, 
                 random=~1|fid,
                 family = "binomial")
summary(fit_mixed)

op <- par(mfrow=c(2,2))
plot(fit_mixed)
par(op)

# lme4
fit_mixed2 <- glmer(soaring~ windshear * wind_speed2M + fspecies+ (1|fid), nAGQ=0,
                     family = binomial) # https://stats.stackexchange.com/questions/304132/glmer-not-converging
summary(fit_mixed2)

plot_model(fit_mixed2)
# -----------------------------------
# GAMM
# -----------------------------------
fit_gammMix <- gamm(soaring~ s(windshear)+ s(wind_speed2M)+ fspecies, random = list(fid=~1), family= binomial)
plot(fit_gammMix$gam)

