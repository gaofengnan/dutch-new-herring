
rm(list = ls())

library(dplyr) # not needed, but used in examples below
# library(cbsodataR)

# detach(ours.df)
# setwd("~/Dropbox/fengnan_richard/code")
# setwd("/Users/richardgill/Desktop/Haring/code")
library(readxl)
library(tidyverse)

# source('./ancillary.R')

ours.df <- read_excel("datasets/ours.xlsx")
ours.df <- arrange(ours.df, vollaard_id)
names(ours.df)
str(ours.df)

vollaard.df <- read_excel("datasets/vollaard.xlsx")
names(vollaard.df)

year <- vollaard.df$yr2017
year <- as.character(year)
year[year == "0"] <- "2016"
year[year == "1"] <- "2017"
year <- factor(year, levels = c("2016", "2017"))

ours.maps.df <- select(ours.df, c("name","address","unique_id","vollaard_id", "cijfer"))
ours.maps.df <- cbind(ours.maps.df, year)

# ours.maps.df$munipality_id <- "EMPTY"

# vendor_geocodes <- ggmap::geocode(ours.maps.df$address) 
# vendor_geocodes[148,] <- ggmap::geocode("Visspecialist Theo Tax malden")
# save(vendor_geocodes, file="datasets/vendor_geocodes.Rdata")
load("datasets/vendor_geocodes.Rdata")
ours.maps.df <- cbind(ours.maps.df,vendor_geocodes)


library(sf)
vendors_loc_sf <- st_as_sf(ours.maps.df,coords=c("lon","lat"), crs = 4326)




pop_by_municipality_2016 <- read_excel("datasets/dutch_municipality_2016.xlsx")
# as.numeric(pop_by_municipality_2016$Gemeentecode) - pop_by_municipality_2016$CBS_code
pop_by_municipality_2016$statcode <- paste0("GM", pop_by_municipality_2016$Gemeentecode)


# library(tidyverse)
library(sf)


# for plotting
library(extrafont)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(scico)
# library(vapoRwave)
# for data wrangling
 
  
# Retrieve data with municipal boundaries from PDOK
municipalBoundaries <- st_read("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_gemeente_2017_gegeneraliseerd&outputFormat=json")

statcode_2017 <- municipalBoundaries$statcode
statcode_2016 <- pop_by_municipality_2016$statcode

not_2017_idx <- statcode_2016 %in% statcode_2016[!(statcode_2016 %in% statcode_2017)]


pop_density_2017 <- pop_by_municipality_2016[!not_2017_idx,]
pop_density_2017 <- select(pop_density_2017, "Municipality_2017", "statcode", "Population_density")
pop_density_2017 <- rbind(pop_density_2017, c("Meierijstad", "GM1948", 439))
pop_density_2017$Population_density <- as.numeric(pop_density_2017$Population_density)

 
data_new <- 
  municipalBoundaries %>%
  left_join(pop_density_2017, by=c(statcode="statcode"))

vendor_plot_title <- "Venders of 2016 and 2017, Dutch New Herring" # (with pop. density)"
# Create a thematic map
p <- 
  data_new %>%
  ggplot() +
  geom_sf(aes(fill = Population_density)) +
  # geom_sf() +
  # scale_fill_viridis_c("pop. density\nby municipality", option = "A") + 
  scale_fill_gradient("pop. density\nby community",trans = "log", 
                      low = alpha("white",0.5), high = alpha("black",0.75), 
                      guide = "colorbar")+
  geom_sf(aes(size = cijfer, color=year), alpha = 0.75, # size = 1.7, # shape = 20, 
          data = vendors_loc_sf) +
  # scale_shape_manual(values = c(15, 16)) +
  scale_color_manual(values = c("darkred", "cyan")) +
  scale_size_area("score", max_size = 4) +
  # geom_point(data = ours.maps.df, aes(x = lon, y = lat), size = 4, 
  #            shape = 23, fill = "darkred")+
  labs(title = vendor_plot_title, fill = "") +
  theme_void()
p
# dev.off()

# this returns transparent png
# png('tr_tst1.png',width=2000,height=2000,units="px",bg = "transparent")
# p
# dev.off()


## krigging

