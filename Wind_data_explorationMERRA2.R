w
library(dplyr)
library(hrbrthemes)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(stringr)
library(RColorBrewer)
library(gridExtra) 
library(ggiraphExtra)

########################################################################
# Set environment
########################################################################
dropdir <- "/Users/tziporahserota/Desktop/Thorne Lab/Analyses/Wind/Wind_maps/Figures/Graphs/"


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

###############################################################################
# Summary Statistics
############################################################################### 
summary <- MERRA  %>% group_by(species, phase, state)%>% 
  summarise(avg_speed2M=mean(wind_speed2M), 
            avg_speed10M=mean(wind_speed10M), 
            q1_2M = quantile(wind_speed2M, 0.25),
            q3_2M = quantile(wind_speed2M, 0.75),
            q1_10M = quantile(wind_speed10M, 0.25),
            q3_10M = quantile(wind_speed10M, 0.75), 
            avg_bwa=mean(bwa, na.rm=TRUE), 
            max_bwa=max(bwa, na.rm=TRUE), 
            min_bwa=min(bwa, na.rm=TRUE),
            avg_shear=mean(windshear))

summary_nophase <- MERRA  %>% group_by(species, state)%>% 
  summarise(avg_speed2M=mean(wind_speed2M), 
            avg_speed10M=mean(wind_speed10M), 
            q1_2M = quantile(wind_speed2M, 0.25),
            q3_2M = quantile(wind_speed2M, 0.75),
            q1_10M = quantile(wind_speed10M, 0.25),
            q3_10M = quantile(wind_speed10M, 0.75), 
            avg_bwa=mean(bwa, na.rm=TRUE), 
            max_bwa=max(bwa, na.rm=TRUE), 
            min_bwa=min(bwa, na.rm=TRUE),
            avg_shear=mean(windshear))

write.csv(summary, "/Users/tziporahserota/Desktop/Thorne Lab/Analyses/Wind/Summary Statistics/wind_summarystatistics_phase.csv")
write.csv(summary_nophase, "/Users/tziporahserota/Desktop/Thorne Lab/Analyses/Wind/Summary Statistics/wind_summarystatistics.csv")

###############################################################################
# Wind Envelope and state, phase
############################################################################### 
# -----------------------------------
# Bar plot
# -----------------------------------
summary$spp_phase <-str_c(summary$species,summary$phase, sep = "_")
summary$state <- as.character(summary$state)
summary_2 <- summary%>% filter(state=="2")

avg_windspeed <- ggplot(summary, aes(x=spp_phase, y=avg_speed2M, fill=state))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.text = element_text(size = 6)
  )+
  ylab("Average Wind Speed m/s")+
  xlab("Species phase")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+
  theme(legend.position = "top")


avg_bwa<- ggplot(summary, aes(x=spp_phase, y=avg_bwa, fill=state))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.text = element_text(size = 6)
  )+
  ylab("Average bwa")+
  xlab("Species phase")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+
  theme(legend.position = "top")

avg_shear<- ggplot(summary, aes(x=spp_phase, y=avg_shear, fill=state))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.text = element_text(size = 6)
  )+
  ylab("Average Wind Shear (m/s)")+
  xlab("Species phase")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+
  theme(legend.position = "top")



# -----------------------------------
# Bar plot, soaring only
# ----------------------------------- 
avg_windspeed <- ggplot(summary_2, aes(x=spp_phase, y=avg_speed2M, fill=spp_phase))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb', '#e78ac3'))+
  theme_bw()+
  theme(legend.position="none")+
  ylab("Average Wind Speed m/s")+
  xlab("Species phase")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+
  theme(legend.position = "top")


avg_bwa <- ggplot(summary_2, aes(x=spp_phase, y=avg_bwa, fill=spp_phase))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb', '#e78ac3'))+
  theme_bw()+
  theme(legend.position="none")+
  ylab("Average bwa")+
  xlab("Species phase")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+
  theme(legend.position = "top")

avg_shear <- ggplot(summary_2, aes(x=spp_phase, y=avg_shear, fill=spp_phase))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb', '#e78ac3'))+
  theme_bw()+
  theme(legend.position="none")+
  ylab("Average Wind Shear(m/s)")+
  xlab("Species phase")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+
  theme(legend.position = "top")

grid.arrange(avg_windspeed, avg_bwa, avg_shear, ncol=3)

# -----------------------------------
# Ridge Plots
# -----------------------------------
soaring <- MERRA %>% filter(state=="2")

ggplot(soaring, aes(y=spp_phase, x=wind_speed2M, fill=spp_phase)) +
  geom_density_ridges(alpha=0.8, bandwidth=.5, quantile_lines=TRUE, rel_min_height = 0.01) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("2019 Soaring Wind Speed")+
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines")
  )

ggsave("Wind_speed_soaring.png", path = dropdir)

ggplot(soaring, aes(y=spp_phase, x=windshear, fill=spp_phase)) +
  geom_density_ridges(alpha=0.8, bandwidth=.2, quantile_lines=TRUE, rel_min_height = 0.01) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("2019 Soaring Wind Shear(m/s)")+
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines")
  )

ggsave("Wind_shear_soaring.png", path = dropdir)

###############################################################################
# Wind Speed Stacked Bars
###############################################################################
# -----------------------------------
# All States
# -----------------------------------
# Count
breaks <- rep(0:21, 1)
names <- c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18","18-19","19-20","20-21")

MERRA$wind_speed_cat <- cut(MERRA$wind_speed2M, breaks = breaks, labels = names)

bbal <- MERRA %>% filter(species=="BBAL")
ghal <- MERRA %>% filter(species=="GHAL")

m_bar_bbal <- bbal %>% count(wind_speed_cat, state)
m_bar_ghal <- ghal %>% count(wind_speed_cat, state)

stacked_bbal_speed <- ggplot(m_bar_bbal, aes(y=n, x=wind_speed_cat, fill=state)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(labels=m_bar_bbal$state, values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Wind Speed (m/s)")+
  ylab("Count")+
  ylim(0, 70000)+
  ggtitle("BBAL")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))


stacked_ghal_speed <- ggplot(m_bar_ghal, aes(fill=state, y=n, x=wind_speed_cat)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Speed")+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.text = element_text(size = 6)
  )+
  theme(axis.title.y = element_blank())+
  ylim(0, 70000)+
  ggtitle("GHAL")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))

grid.arrange(stacked_bbal_speed,stacked_ghal_speed, ncol=2)


# Proportion
bbal_prop <- ggplot(m_bar_bbal, aes(fill=state, y=n, x=wind_speed_cat)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Speed")+
  ylab("Proporion")+
  ggtitle("BBAL")+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))

ghal_prop <- ggplot(m_bar_ghal, aes(fill=state, y=n, x=wind_speed_cat)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Speed")+
  theme(axis.title.y = element_blank())+
  ggtitle("GHAL")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))

grid.arrange(bbal_prop,ghal_prop, ncol=2)

# -----------------------------------
# Soaring only
# ----------------------------------
# Count
MERRA$soaring <-  ifelse(MERRA$state== "2","Soaring", "Non-Soaring" ) 
bbal <- MERRA %>% filter(species=="BBAL")
ghal <- MERRA %>% filter(species=="GHAL")
m_bar_bbal <- bbal %>% count(wind_speed_cat, soaring)
m_bar_ghal <- ghal %>% count(wind_speed_cat, soaring)

stacked_bbal <- ggplot(m_bar_bbal, aes(fill=soaring, y=n, x=wind_speed_cat)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Wind Speed")+
  ylab("Count")+
  ylim(0, 70000)+
  ggtitle("BBAL")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))


stacked_ghal <- ggplot(m_bar_ghal, aes(fill=soaring, y=n, x=wind_speed_cat)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Speed")+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.text = element_text(size = 6)
  )+
  theme(axis.title.y = element_blank())+
  ylim(0, 70000)+
  ggtitle("GHAL")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+ 
  theme(legend.title = element_blank())



grid.arrange(stacked_bbal,stacked_ghal, ncol=2)


# Proportion
bbal_prop <- ggplot(m_bar_bbal, aes(fill=soaring, y=n, x=wind_speed_cat)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Speed")+
  ylab("Proporion")+
  ggtitle("BBAL")+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))

ghal_prop <- ggplot(m_bar_ghal, aes(fill=soaring, y=n, x=wind_speed_cat)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Speed")+
  theme(axis.title.y = element_blank())+
  ggtitle("GHAL")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+ 
  theme(legend.title = element_blank())+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.text = element_text(size = 6)
  )
  

grid.arrange(bbal_prop,ghal_prop, ncol=2)


###############################################################################
# Wind Shear Stacked Bars
###############################################################################
# remove negative wind shear values 
MERRA <- MERRA[MERRA$windshear > 0, ]

# -----------------------------------
# All States
# -----------------------------------
# Count
breaks <- seq(0, 5, by=.5)
names <- c("0-0.5","0.5-1","1-1.5","1.5-2","2-2.5","2.5-3","3-3.5","3.5-4","4-4.5","4.5-5")

MERRA$wind_shear_cat <- cut(MERRA$windshear, breaks = breaks, labels = names)

bbal <- MERRA %>% filter(species=="BBAL")
ghal <- MERRA %>% filter(species=="GHAL")

m_bar_bbal <- bbal %>% count(wind_shear_cat, state)
m_bar_ghal <- ghal %>% count(wind_shear_cat, state)

stacked_bbal_shear <- ggplot(m_bar_bbal, aes(y=n, x=wind_shear_cat, fill=state)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(labels=m_bar_bbal$state, values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Wind Shear (m/s)")+
  ylab("Count")+
  ylim(0, 123000)+
  ggtitle("BBAL")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))


stacked_ghal_shear <- ggplot(m_bar_ghal, aes(y=n, x=wind_shear_cat, fill=state)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(labels=m_bar_ghal$state, values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Wind Shear (m/s)")+
  ylab("Count")+
  ylim(0, 123000)+
  ggtitle("GHAL")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.text = element_text(size = 6)
  )

grid.arrange( stacked_bbal_shear,stacked_ghal_shear, ncol=2)


# Proportion
bbal_prop <- ggplot(m_bar_bbal, aes(fill=state, y=n, x=wind_shear_cat)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Shear (m/s)")+
  ylab("Proporion")+
  ggtitle("BBAL")+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))

ghal_prop <- ggplot(m_bar_ghal, aes(fill=state, y=n, x=wind_shear_cat)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Shear (m/s)")+
  theme(axis.title.y = element_blank())+
  ggtitle("GHAL")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))

grid.arrange(bbal_prop,ghal_prop, ncol=2)

# -----------------------------------
# Soaring only
# ----------------------------------
# Count
MERRA$soaring <-  ifelse(MERRA$state== "2","Soaring", "Non-Soaring" ) 
bbal <- MERRA %>% filter(species=="BBAL")
ghal <- MERRA %>% filter(species=="GHAL")
m_bar_bbal <- bbal %>% count(wind_shear_cat, soaring)
m_bar_ghal <- ghal %>% count(wind_shear_cat, soaring)

stacked_bbal <- ggplot(m_bar_bbal, aes(fill=soaring, y=n, x=wind_shear_cat)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Wind Shear")+
  ylab("Count")+
  ylim(0, 123000)+
  ggtitle("BBAL")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))


stacked_ghal <- ggplot(m_bar_ghal, aes(fill=soaring, y=n, x=wind_shear_cat)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Shear")+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.text = element_text(size = 6)
  )+
  theme(axis.title.y = element_blank())+
  ylim(0, 123000)+
  ggtitle("GHAL")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+ 
  theme(legend.title = element_blank())



grid.arrange(stacked_bbal,stacked_ghal, ncol=2)


# Proportion
bbal_prop <- ggplot(m_bar_bbal, aes(fill=soaring, y=n, x=wind_shear_cat)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Shear")+
  ylab("Proporion")+
  ggtitle("BBAL")+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))

ghal_prop <- ggplot(m_bar_ghal, aes(fill=soaring, y=n, x=wind_shear_cat)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb'))+
  theme_bw()+
  xlab("Wind Shear")+
  theme(axis.title.y = element_blank())+
  ggtitle("GHAL")+
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5))+ 
  theme(legend.title = element_blank())+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.text = element_text(size = 6)
  )


grid.arrange(bbal_prop,ghal_prop, ncol=2)

