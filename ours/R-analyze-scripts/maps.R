
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

ours.maps.df <- dplyr::select(ours.df, c("name","address","unique_id","vollaard_id", "cijfer"))
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
pop_density_2017 <- dplyr::select(pop_density_2017, "Municipality_2017", "statcode", "Population_density")
pop_density_2017 <- rbind(pop_density_2017, c("Meierijstad", "GM1948", 439))
pop_density_2017$Population_density <- as.numeric(pop_density_2017$Population_density)

 
commune_density <- 
  municipalBoundaries %>%
  left_join(pop_density_2017, by=c(statcode="statcode"))

vendor_plot_title <- "Venders of 2016 and 2017, Dutch New Herring" # (with pop. density)"
# Create a thematic map
p <- 
  commune_density %>%
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


# add the covariate of municipality population density
# fill in later
# for (idx in 1:nrow(ours.maps.df)) {
#   commune <- substring(str_extract(ours.maps.df$address[idx], '\\s-\\s.+'),4)
#   pop_density_2017[Municipality_2017==commune,]
# }
# non_zero_grade_idx <- ours.df$eindcijfer!=0
# vollaard.df.non.zero <- as.data.frame(vollaard.df[non_zero_grade_idx,])
ours.df$lon <- ours.maps.df$lon
ours.df$lat <- ours.maps.df$lat 
attach(ours.df)
quad.fit <- lm(cijfer~1+lon+I(lon**2)+lat+I(lat**2)+I(lon*lat))
lon_grid <- 3.2 + 1:100*4/100; lat_grid <- 50.5 + 1:100*3/100
lon_lat_grid <- as.data.frame(expand_grid(lon_grid, lat_grid))
lon_lat_contour <- lon_lat_grid %>% add_column(Incpt = 1, lonSq = lon_lat_grid[,1]**2, latSq = lon_lat_grid[,2]**2, lonXlat=lon_lat_grid[,1]*lon_lat_grid[,2])
lon_lat_contour <- lon_lat_contour[,c(3,1,4,2,5,6)]
quad.fit.coef <- coefficients(quad.fit)
pred.values.contour <- as.matrix(lon_lat_contour) %*% as.matrix(quad.fit.coef)

get_countries <-  function(long, lat)
{ 
  points <- cbind(long, lat)
  countriesSP <- rworldmap::getMap(resolution = 'high')
  pointsSP = sp::SpatialPoints(points, sp::CRS(sp::proj4string(countriesSP)))  
  sp::over(pointsSP, countriesSP)$ADMIN
}



# NL_or_not <- (get_countries(lon_lat_grid[,1], lon_lat_grid[,2])=="Netherlands")
NL_or_not[is.na(NL_or_not)] <- FALSE
quad.fit.contour.NL <- data_frame(lon=lon_lat_grid[,1], lat=lon_lat_grid[,2], quad.fit.value=pred.values.contour)

# utrecht_gps <- c(5.116667, 52.083333)
# ret <- apply(lon_lat_grid[NL_or_not,], 1, function(v) {sum((v-utrecht_gps)^2)})
utrecht_grid_idx <- 4753 # in original grid
utrecht_drift <- as.numeric(6- quad.fit.contour.NL[utrecht_grid_idx,]$quad.fit.value)

quad.fit.contour.NL <- quad.fit.contour.NL[NL_or_not,] 
quad.fit.contour.NL$quad.fit.value <- quad.fit.contour.NL$quad.fit.value + utrecht_drift
quad.fit.contour.NL.sf <- st_as_sf(quad.fit.contour.NL,coords=c("lon","lat"), crs = 4326)




NL_map <- ggmap::get_map(location = c(left=3.2,right=7.5,bottom=50.5,top=54), source = 'stamen', maptype = 'toner') 
NL <- ggmap::ggmap(NL_map,extent="normal")
NL + geom_contour_filled(data=quad.fit.contour.NL, aes(lon,lat,z=quad.fit.value), breaks=(1:14)/2)+ ggtitle("without covariates") +
  # scale_fill_manual(values=heat.colors(15)[c(1:12,15)],drop=FALSE)
  guides(fill=guide_legend(reverse=TRUE))  + 
    # scale_fill_viridis_b()
    scale_fill_manual(values = viridis::viridis_pal()(13),drop=FALSE)
 
quad.fit <- lm(cijfer~1+lon+I(lon**2)+lat+I(lat**2)+I(lon*lat)+ripeness +price_per_100g+weight+cleanness+fat_percentage+micro)

summary(quad.fit)

lon_lat_contour <- lon_lat_grid %>% add_column(Incpt = 1, lonSq = lon_lat_grid[,1]**2, latSq = lon_lat_grid[,2]**2, lonXlat=lon_lat_grid[,1]*lon_lat_grid[,2])
lon_lat_contour <- lon_lat_contour[,c(3,1,4,2,5,6)]
quad.fit.coef <- coefficients(quad.fit)
pred.values.contour <- as.matrix(lon_lat_contour) %*% as.matrix(quad.fit.coef[1:6]) 

# NL_or_not <- (get_countries(lon_lat_grid[,1], lon_lat_grid[,2])=="Netherlands")
# NL_or_not[is.na(NL_or_not)] <- FALSE
quad.fit.contour.NL <- data_frame(lon=lon_lat_grid[,1], lat=lon_lat_grid[,2], quad.fit.value=pred.values.contour)

utrecht_drift <- as.numeric(6- quad.fit.contour.NL[utrecht_grid_idx,]$quad.fit.value)

quad.fit.contour.NL <- quad.fit.contour.NL[NL_or_not,] 
quad.fit.contour.NL$quad.fit.value <- quad.fit.contour.NL$quad.fit.value + utrecht_drift

# quad.fit.contour.NL.sf <- st_as_sf(quad.fit.contour.NL,coords=c("lon","lat"), crs = 4326)

# NL_map <- ggmap::get_map(location = c(left=3.2,right=7.5,bottom=50.5,top=54), source = 'stamen', maptype = 'toner') 
NL <- ggmap::ggmap(NL_map,extent="normal")
NL + geom_contour_filled(data=quad.fit.contour.NL, 
                         aes(lon,lat,z=quad.fit.value),
                         breaks = (6+(1:8))/2) +
  ggtitle("with covariates") +
  # scale_fill_manual(values = heat.colors(15)[-c((1:6),13,14)],drop=FALSE) +
  # theme_void() + scale_color_continuous("pred. score") 
  guides(fill=guide_legend(reverse=TRUE))  + 
  # scale_fill_viridis_b()
  scale_fill_manual(values = viridis::viridis_pal()(13)[-(1:6)],drop=FALSE)




## krigging
# generate a SPDF
herring <- ours.maps.df[ours.maps.df$cijfer!=0,]


# sp::coordinates(herring) <- ~ lon + lat
# her_vgm <- gstat::variogram(cijfer~1, herring)
# plot(her_vgm)
# her_fit <- gstat::fit.variogram(her_vgm, model=vgm(psill = 8, "Exp", range = NA, 1))



