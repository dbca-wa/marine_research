# four plot panel for conceptual spatial diagrams for enviromental predictors explaining variation in seagrass loss in SBMP

library(ggplot2)
library(sf)
library(rgdal)
library(grid)
library(gridExtra)
library(cowplot)
library(tidyverse)

setwd("C:/Users/simonestrydom/Documents/R/SBMP/SST/SST_2010_2014/MapOutputs/Maps")
getwd()
# polygon of shark bay
zones <-  readOGR(dsn = "./shapefile",
                  layer = "SB_Geo_Oceanograpthic_Monitoring_Regions_final_GDA_z49")

# correct zone names
zones@data$Zones <- c("HP", "FS", "FB", "WG", "OC", "WB") 

# stats output previously created
stats <- read_csv("C:/Users/simonestrydom/Documents/R/SBMP/SST/SST_2010_2014/MapOutputs/Maps/SB_CRW3_1_SST_Ausbathy_SBWH_SG_2010_2014_Step4_SG_density_STATUS_AA2803_Ext2010.csv")

stats_df <- stats %>%
  group_by(zone) %>%
  summarise(Depth = mean(zero_corrected_site_depth),
            Hotmean = mean(hotmean),
            Streaksover = mean(streaksover),
            Density = mean(`2010_SG_Dense`)) %>%
  mutate(Zones = c(NA, "FB", "WG", "HP", "FS", "OC", "WB")) %>%
  slice(-1) %>%
  select(-zone)

# join the summarised stats to the attribute table within the zone shape file
zones@data <- full_join(zones@data, stats_df, by = "Zones")

# grab a coastline shape file
poly_sf <- st_read(dsn = "./shapefile/Island_and_mainland_SharkBay_final_mask_GDA94_MGA49_v2.shp")
zones_sf <- st_as_sf(zones)

# generate 4 plots using ggplot and pkg `sf`` to handle shape files
# all plots utilising colorbrewer palettes
HotP <- ggplot() +
  geom_sf(data = poly_sf) +
  geom_sf(data = zones_sf, aes(fill = factor(round(Hotmean, 2)))) + 
  coord_sf(datum = sf::st_crs(28349)) +
  xlim(692912, 850000) + 
  ylim(7020000, 7270000) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.position = c(0.8, 0.8)) +
  scale_fill_brewer(type = "seq", palette = "Reds", direction = 1, name = "HotMeanTemp") +
  guides(fill = guide_legend(reverse = TRUE))


densityP <- ggplot() +
  geom_sf(data = poly_sf) +
  geom_sf(data = zones_sf, aes(fill = factor(round(Density, 0)))) + 
  coord_sf(datum = sf::st_crs(28349)) +
  xlim(692912, 850000) + 
  ylim(7020000, 7270000) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.position = c(0.8, 0.8)) +
  scale_fill_brewer(type = "seq", palette = "Greens", direction = 1, name = "Mean\nDensity") +
  guides(fill = guide_legend(reverse = TRUE))


depthP <- ggplot() +
  geom_sf(data = poly_sf) +
  geom_sf(data = zones_sf, aes(fill = factor(round(Depth, 1)))) + 
  coord_sf(datum = sf::st_crs(28349)) +
  xlim(692912, 850000) + 
  ylim(7020000, 7270000) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.position = c(0.8, 0.8)) +
  scale_fill_brewer(type = "seq", palette = "Blues", direction = -1, name = "Mean Depth\n(m)") +
  guides(fill = guide_legend(reverse = TRUE))


streaksP <- ggplot() +
  geom_sf(data = poly_sf) +
  geom_sf(data = zones_sf, aes(fill = factor(round(Streaksover, 2)))) + 
  coord_sf(datum = sf::st_crs(28349)) +
  xlim(692912, 850000) + 
  ylim(7020000, 7270000) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.position = c(0.8, 0.8)) +
  scale_fill_brewer(type = "seq", palette = "Reds", direction = 1, name = "Streaks\nOver") +
  guides(fill = guide_legend(reverse = TRUE))


# pkg 'cowplot' sorts out grid layout letter labeling
# final sizing of saved out graphic will probably necessitate adjustment
# of font sizing and possibly legend position
four_panel <- plot_grid(overP, depthP, streaksP, densityP, labels = "AUTO", ncol = 2, align = 'v')

# now view your maps and export
HotP
densityP
depthP
streaksP
four_panel



