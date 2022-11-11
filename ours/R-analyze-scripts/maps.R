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

vollaard.df <- read_excel("vollaard.xlsx")
names(vollaard.df)

year <- vollaard.df$yr2017
year <- as.character(year)
year[year == "0"] <- "2016"
year[year == "1"] <- "2017"
year <- factor(year, levels = c("2016", "2017"))

ours.maps.df <- select(ours.df, c("name","address","unique_id","vollaard_id"))
ours.maps.df <- cbind(ours.maps.df, year)

# ours.maps.df$munipality_id <- "EMPTY"

# vendor_geocodes <- ggmap::geocode(ours.maps.df$address) 
# vendor_geocodes[148,] <- ggmap::geocode("Visspecialist Theo Tax malden")
# save(vendor_geocodes, file="vendor_geocodes.Rdata")
load("vendor_geocodes.Rdata")
ours.maps.df <- cbind(ours.maps.df,vendor_geocodes)


library(sf)
vendors_loc_sf <- st_as_sf(ours.maps.df,coords=c("lon","lat"), crs = 4326)





nl_gemeente <- spatialrisk::nl_gemeente

# library(ggmap)
# t <- ggmap::get_map("Netherlands", source="google", maptype = "terrain-lines",
#              zoom = 8) 
# ggmap::ggmap(t)






