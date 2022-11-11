rm(list = ls())
# detach(ours.df)
# setwd("~/Dropbox/fengnan_richard/code")
# setwd("/Users/richardgill/Desktop/Haring/code")
library(readxl)
library(tidyverse)

# source('./ancillary.R')

ours.df <- read_excel("ours.xlsx")
ours.df <- arrange(ours.df, vollaard_id)
names(ours.df)
str(ours.df)

ours.maps.df <- select(ours.df, c("name","address","unique_id","vollaard_id"))

ours.maps.df$munipality_id <- "EMPTY"

# vendor_geocodes <- ggmap::geocode(ours.maps.df$address) 
# vendor_geocodes[148,] <- ggmap::geocode("Visspecialist Theo Tax malden")
ours.maps.df <- cbind(ours.maps.df,vendor_geocodes)


library(sf)
vendors_loc_sf <- st_as_sf(ours.maps.df,coords=c("lon","lat"), crs = 4326)


# for plotting
library(extrafont)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(scico)
library(vapoRwave)
# for data wrangling
library(dplyr)


nl_gemeente <- spatialrisk::nl_gemeente

# library(ggmap)
# t <- ggmap::get_map("Netherlands", source="google", maptype = "terrain-lines",
#              zoom = 8) 
# ggmap::ggmap(t)






