##R script for catch summary for plot in paper: Driftnet Fisheries: a sink for Indian Ocean cetaceans##
#Last update: 18 octobre 2023

#set wd and load packages
setwd("/Users/briannaelliott/Desktop/PhDYr4/Introduction")
library(ggplot2)
library(tidyverse) 
library(dplyr)
#install.packages("sf")
library(sf)
#library(leaflet) # web-embeddable interactive maps
library(rnaturalearth) # map data
library(rnaturalearthdata)# map data
#install.packages("ggspatial")
library(ggspatial) # scale bars and north arrows
#install.packages("ggrepel")
library(ggrepel) #moving text around
#install.packages("ggthemes")
library(ggthemes)
#install.packages("ggspatial", type = "binary")
library(ggspatial)
#devtools::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)# map data for countries in the whole world
library(cowplot)
#install.packages("lwgeom")
library(lwgeom)
#install.packages("googleway")
library("googleway")
#install.packages("scales")
library(scales)
library("ggplot2")
library("sf")
library("readxl")
theme_set(theme_bw())
options(scipen = 999)

##### general mapping #####

#ne_countries pulls country data
world <- ne_countries(scale="medium", returnclass = "sf")
class(world)

#check world map was properly retrieved and converted into sf object
ggplot(data = world) +
  geom_sf(fill="darkgrey")

#mapworld
ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), "countries)"))

#set map to a region (IOTC-ish) and add scale bar and north arrow
basiciotcmap <- ggplot(data = world) +
  geom_sf(fill="darkgrey") +
  annotation_scale(location = "bl", width_hint = 0.2) +
  #annotation_north_arrow(location = "bl", which_north = "true", 
  #pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(18, 148), ylim = c(-62, 34), expand = FALSE) +
  #theme(panel.grid.major = element_line(color = gray(.2), linetype = "dashed", size = 0.5)) +
  theme(panel.background = element_rect(fill = "aliceblue"))

#add in IOTC AOC, downloaded from here: https://geonetwork.d4science.org/geonetwork/srv/en/main.home
iotc <- read_sf("IOTC_area.shp")

#add AOC to basic IOTC map
basiciotcmap_withAOC <- basiciotcmap +
  geom_sf(data = iotc, col = "black", fill = NA, size = .5) +
  coord_sf(xlim = c(18, 148), ylim = c(-62, 34), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") 

##################################

##### ADD EEZS and CLIP TO IOTC #####
#downloaded from 'https://www.marineregions.org/downloads.php' (World EEZ v11, 0 to 360 degrees)

#read just the boundaries
#eez_boundaries <- sf::st_read("World_EEZ_v11_20191118_HR_0_360", layer="eez_boundaries_v11_0_360")

# Read the polygons
eez <- sf::st_read("World_EEZ_v11_20191118_HR_0_360", layer="eez_v11_0_360")

#avoid invalid warnings
sf_use_s2(FALSE)

# Validity check
valid_check <- st_is_valid(eez)
all(valid_check)

#filter just IOTC CPCs with reported GN catch (May 2022 data) + relevant columns + no disputed EEZs
eez_iotc_GN <- eez %>% filter(TERRITORY1 %in% c("Australia", "Bangladesh", "Comores", "Eritrea", 
                                                "India", "Indonesia",
                                                "Iran", "Kenya", "Malaysia", 
                                                "Mozambique", "Oman", "Pakistan", "Sri Lanka", 
                                                "Sudan", "Tanzania", "Thailand", "Yemen"))  %>%
  select (-c(MRGID, GEONAME, MRGID_TER1, MRGID_SOV1, SOVEREIGN1, 
             MRGID_TER2, MRGID_SOV2, TERRITORY2, ISO_TER2, SOVEREIGN2, MRGID_TER3, 
             MRGID_SOV3, TERRITORY3, ISO_TER3, SOVEREIGN3, ISO_SOV1, ISO_SOV2, ISO_SOV3, 
             UN_SOV2, UN_SOV3, UN_TER1, UN_TER2, UN_TER3)) %>%
  filter(POL_TYPE=='200NM') 

#export shp to crop EEZs to IOTC AOC in QGIS, used Vector->Geoprocessing->Clip
st_write(eez_iotc_GN, "R_EEZs.shp", append=FALSE)
st_write(iotc, "iotc.shp", append=FALSE)
#reload new clipped shp 
clippedeez <- sf::st_read("ClippedEEZs.shp")

#test mapping eezs on basemap
iotcEEZbasemap <- basiciotcmap_withAOC + 
  geom_sf(data = iotc, col = "black", fill = NA, size = .5) +
  #coord_sf(xlim = c(18, 148), ylim = c(-62, 38), expand = FALSE) +
  geom_sf(data = clippedeez, col = "grey", fill = NA, size = .8) +
  coord_sf(xlim = c(18, 148), ylim = c(-62, 34), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") 

#####################################

### add catch and Anderson data ### 
#note: see "IOTC_catch-summary.R" for code for data summary code 
catchdata_EEZplot <- read_excel("DataforR_updatedOctober.xlsx", sheet = 1)
catchdata_EEZplot <- catchdata_EEZplot %>%              #need to add Sudan and Kenya twice to match w/ EEZs
  add_row(Country = "Sudan", meancatch=30.85, Andersonestimates = 0) %>% 
  add_row(Country = "Kenya", meancatch=111.89, Andersonestimates = 77) %>% 
  arrange(Country)  #put in alpha order by country
catchdata_EEZplot <- mutate(catchdata_EEZplot, #add column for merge
                            TERRITORY1 = catchdata_EEZplot$Country)
eez_iotc_GN <- arrange(clippedeez,TERRITORY1)        #also put EEZs in alpha order

#merge DFs
EEZ_WITH_CATCH_ANDERSON =
  clippedeez %>% 
  merge(catchdata_EEZplot, by = "TERRITORY1") 

#plot just catch
EEZcatchmap <- iotcEEZbasemap +
  geom_sf(data = EEZ_WITH_CATCH_ANDERSON, aes(fill = meancatch), size = .8) +
  coord_sf(xlim = c(18, 148), ylim = c(-62, 34), expand = FALSE) +
  scale_fill_viridis_c(option = "rocket", direction = -1, begin = 0.20, end = 0.90, name = "Annual mean driftnet 
catch from 2012-2016 (tons)", 
guide = guide_colourbar(reverse = TRUE), label = scales::comma) + 
  scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))

#plot catch and bycatch
#for bycatch only: filter values over zero so don't plot Sudan and Aus 
EEZ_WITH_CATCH_ANDERSON_NoAusSudan  <- filter(EEZ_WITH_CATCH_ANDERSON , Andersonestimates >= 10) 
#final plot
bycatchEEZmap <- EEZcatchmap +
  geom_point(data = EEZ_WITH_CATCH_ANDERSON_NoAusSudan, aes(x = eezlongpoint, y = eezlatpoint, size = Andersonestimates), color = "yellow", 
             alpha=0.80) +
  scale_size_continuous(name = "Annual mean bycatch 
from 2012-2016 (#s)", label = scales::comma)
