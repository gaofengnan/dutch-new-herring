
rm(list = ls())

library(dplyr) # not needed, but used in examples below
# library(cbsodataR)

# detach(ours.df)
# setwd("~/Dropbox/fengnan_richard/code")
# setwd("/Users/richardgill/Desktop/Haring/code")
library(readxl)
library(tidyverse)

source('./ancillary.R')
source('./import_data_universal.R')


ours.maps.df <- dplyr::select(ours.df, c("name","address","unique_id","vollaard_id", "supplier_AD", "cijfer"))
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
# municipalBoundaries <- st_read("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_gemeente_2017_gegeneraliseerd&outputFormat=json")

# save(municipalBoundaries, file="datasets/municipalBoundaries.Rdata")

load(file="datasets/municipalBoundaries.Rdata")

municipalBoundaries <- st_transform(municipalBoundaries, crs=4326)

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
  geom_sf(aes(size = cijfer, color=year), alpha = 0.5, # size = 1.7, # shape = 20, 
          data = dplyr::filter(vendors_loc_sf, supplier_AD=="A")) +
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
lon <- ours.maps.df$lon
lat <- ours.maps.df$lat 
# attach(ours.df)
quad.fit <- lm(cijfer~1+lon+I(lon**2)+lat+I(lat**2)+I(lon*lat))
lon_grid <- 3.2 + 1:400*4/400; lat_grid <- 50.5 + 1:400*3/400
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



NL_or_not <- (get_countries(lon_lat_grid[,1], lon_lat_grid[,2])=="Netherlands")
NL_or_not[is.na(NL_or_not)] <- FALSE
quad.fit.contour.NL <- tibble(lon=lon_lat_grid[,1], lat=lon_lat_grid[,2], quad.fit.value=pred.values.contour)

utrecht_gps <- c(5.116667, 52.083333)
ret <- apply(lon_lat_grid[NL_or_not,], 1, function(v) {sum((v-utrecht_gps)^2)})
utrecht_grid_idx <- 76611 # in original grid
utrecht_drift <- as.numeric(6- quad.fit.contour.NL[utrecht_grid_idx,]$quad.fit.value)

quad.fit.contour.NL <- quad.fit.contour.NL[NL_or_not,] 
quad.fit.contour.NL$quad.fit.value <- quad.fit.contour.NL$quad.fit.value + utrecht_drift
# quad.fit.contour.NL.sf <- st_as_sf(quad.fit.contour.NL,coords=c("lon","lat"), crs = 4326)




# NL_map <- ggmap::get_map(location = c(left=3.2,right=7.3,bottom=50.7,top=53.7), source = 'stamen', maptype = 'terrain') 
# save(NL_map, file="datasets/NL_map.Rdata")

load(file="datasets/NL_map.Rdata")
NL <- ggmap::ggmap(NL_map,extent="normal")
NL + geom_contour_filled(data=quad.fit.contour.NL, aes(lon,lat,z=quad.fit.value), alpha=0.8, breaks=(1:14)/2)+ ggtitle("without covariates") +
  # scale_fill_manual(values=heat.colors(15)[c(1:12,15)],drop=FALSE)
  guides(fill=guide_legend(reverse=TRUE))  + 
    # scale_fill_viridis_b()
    scale_fill_manual(values = viridis::viridis_pal()(13),drop=FALSE)
 

vollaard_orig.df <- read.csv("./datasets/scores_herringtest_2016_2017.csv", sep = ";")
micro_v <- as.factor(vollaard_orig.df$micro_num)
levels(micro_v) <- c("Good", "Sufficient", "Bad", "Warning", "Reject")
table(micro_v)


ripeness_v <- as.factor(vollaard_orig.df$ripeness_num)
levels(ripeness_v) <- c("Light", "Average", "Strong", "Rotten")
table(ripeness_v)

cleaning_v <- as.factor(vollaard_orig.df$cleaning_num)
levels(cleaning_v) <- c("Very good", "Good", "Poor", "Bad")
table(cleaning_v)

final_score_v <- vollaard_orig.df$finalscore - 7.5

quad.fit <- lm(final_score_v~1+lon+I(lon**2)+lat+I(lat**2)+I(lon*lat)+ weight+ 
                 price_cat+ temp_cat + fat_cat + freshly_cleaned+ 
                 micro_v+ripeness_v+cleaning_v+year) # +factor(atlantic))
summary(quad.fit)

# library(tmap)
# data(NLD_prov)
# get_commune <- function(long, lat)
# {
#   points <- cbind(long, lat) 
#   NLD_provinces_SP <- cx
# }

limburg <- (lat<51.3) & (lon>5.7)
sum(limburg)



lin.fit <- lm(final_score_v~1+lon+lat+ weight+ 
                 price_cat+ temp_cat + fat_cat + freshly_cleaned+ 
                 micro_v+ripeness_v+cleaning_v+year)
summary(lin.fit)

  
plain.lm <- lm(final_score_v~1+ weight+ temp_cat + fat_cat + freshly_cleaned+ 
                 price_cat + micro_v+ripeness_v+cleaning_v+year) # +factor(atlantic))
summary(plain.lm)
anova(quad.fit,plain.lm) # p-value of quadratic effects in lieu of k30, 0.047
anova(lin.fit,plain.lm) # p-value of linear effects in lieu of k30, 0.005!

vollaard.lm <- lm(formula = final_score_v ~
                    weight + temp_cat + fat_cat + price_cat + freshly_cleaned +
                    micro_v + ripeness_v + cleaning_v + year + k30)
summary(vollaard.lm)


lon_lat_contour <- lon_lat_grid %>% add_column(Incpt = 1, lonSq = lon_lat_grid[,1]**2, latSq = lon_lat_grid[,2]**2, lonXlat=lon_lat_grid[,1]*lon_lat_grid[,2])
lon_lat_contour <- lon_lat_contour[,c(3,1,4,2,5,6)]
quad.fit.coef <- coefficients(quad.fit)
lin.fit.coef <- coefficients(lin.fit)
quad.pred.values.contour <- as.matrix(lon_lat_contour) %*% as.matrix(quad.fit.coef[1:6]) 
lin.pred.values.contour <- as.matrix(lon_lat_contour[,c(1,2,4)]) %*% as.matrix(lin.fit.coef[1:3])


# NL_or_not <- (get_countries(lon_lat_grid[,1], lon_lat_grid[,2])=="Netherlands")
# NL_or_not[is.na(NL_or_not)] <- FALSE
quad.fit.contour.NL <- tibble(lon=lon_lat_grid[,1], lat=lon_lat_grid[,2], quad.fit.value=quad.pred.values.contour)
lin.fit.contour.NL <- tibble(lon=lon_lat_grid[,1], lat=lon_lat_grid[,2], lin.fit.value=lin.pred.values.contour)

utrecht_drift <- as.numeric(6- quad.fit.contour.NL[utrecht_grid_idx,]$quad.fit.value)

quad.fit.contour.NL <- quad.fit.contour.NL[NL_or_not,] 
quad.fit.contour.NL$quad.fit.value <- quad.fit.contour.NL$quad.fit.value + utrecht_drift

utrecht_drift <- as.numeric(6- lin.fit.contour.NL[utrecht_grid_idx,]$lin.fit.value)
lin.fit.contour.NL <- lin.fit.contour.NL[NL_or_not,] 
lin.fit.contour.NL$lin.fit.value <- lin.fit.contour.NL$lin.fit.value + utrecht_drift


# quad.fit.contour.NL.sf <- st_as_sf(quad.fit.contour.NL,coords=c("lon","lat"), crs = 4326)

NL <- ggmap::ggmap(NL_map,extent="normal")
quad.p <- NL + geom_contour_filled(data=quad.fit.contour.NL, 
                         aes(lon,lat,z=quad.fit.value),
                         breaks = (8+(1:6))/2, alpha = 0.7) +
  ggtitle("   quadratic spatial effect w/ covariates") +
  # scale_fill_manual(values = heat.colors(15)[-c((1:6),13,14)],drop=FALSE) +
  # theme_void() + scale_color_continuous("pred. score") 
  guides(fill=guide_legend(reverse=TRUE))  + 
  # scale_fill_viridis_b()
  scale_fill_manual(values = viridis::viridis_pal()(11)[-(1:6)],drop=FALSE) +
  #geom_point(data=ours.maps.df, aes(x=lon,y=lat, color=supplier_AD), alpha=0.5) +
  theme_void()
quad.p
ggsave(quad.p, file="plots/spatial_right.tiff", device="tiff", dpi=300,bg='transparent',width = 15, height = 15, units = "cm")

lin.p <- NL + geom_contour_filled(data=lin.fit.contour.NL, 
                         aes(lon,lat,z=lin.fit.value),
                         breaks = (8+(1:6))/2, alpha = 0.7) +
  ggtitle("   linear spatial effect w/ covariates") +
  # scale_fill_manual(values = heat.colors(15)[-c((1:6),13,14)],drop=FALSE) +
  # theme_void() + scale_color_continuous("pred. score") 
  guides(fill=guide_legend(reverse=TRUE))  + 
  # scale_fill_viridis_b()
  scale_fill_manual(values = viridis::viridis_pal()(11)[-(1:6)],drop=FALSE) +
  #geom_point(data=ours.maps.df, aes(x=lon,y=lat, color=supplier_AD), alpha=0.5) +
  theme_void()
lin.p


# ggsave('plots/spatial_right.png', p, dpi=300, bg='transparent',width = 15, height = 15, units = "cm")

## clean version

vendor_plot_title <- "Venders of 2016 and 2017, Dutch New Herring" # (with pop. density)"
# Create a thematic map
p <- 
  commune_density %>%
  ggplot() +
  geom_sf(aes(fill = Population_density)) +
  scale_fill_gradient("pop. density\nby community",trans = "log10", 
                      low = alpha("white",0.5), high = alpha("black",0.75),  guide = "colorbar")+
  ggnewscale::new_scale_fill() +
  # geom_contour_filled(data=quad.fit.contour.NL, 
  #                    aes(lon,lat,z=quad.fit.value),
  #                    breaks = (6+(1:8))/2, alpha = 0.7) +
  # guides(fill=guide_legend(reverse=TRUE,ncol=2))  + 
  # scale_fill_manual("quad. pred. w. cov.", values = viridis::viridis_pal()(13)[-(1:6)],drop=FALSE)+
  geom_point(data=ours.maps.df, aes(x=lon,y=lat,size=cijfer,color=year), alpha=0.5) +
   scale_color_manual(values = c("darkred", "cyan")) +
   scale_size_area("score", max_size = 4) + 
   labs(title = vendor_plot_title, fill = "") +
  theme_void()
p
ggsave('plots/spatial_left.tiff', device="tiff", p, dpi=300, bg='transparent',width = 15, height = 15, units = "cm")

if (FALSE) {
attach(ours.df)

table(Repeat)
Repeat[is.na(Repeat)] <- "_"
table(Repeat)

final_score <- eindcijfer

weight <- weight

temp <- temp

temp_cat <- cut(temp, breaks = c(-3, 7, 10, 20), right = TRUE)

fat <- fat_percentage

fat_cat <-cut(fat_percentage, c(0, 10, 14, 20), right = TRUE)

price <- price_per_100g

price_cat <- cut(ours.df$price_per_100g, breaks = c(1, 2.84, 3.48, 5.1 ), right = TRUE)

freshly_cleaned <- vers_van_het_mes

freshly_cleaned[freshly_cleaned == "Ja"] <- "Yes"
freshly_cleaned[freshly_cleaned == "Nee"] <- "No"
freshly_cleaned <- factor(freshly_cleaned, levels = c("No", "Yes"))

micro[micro == "Zeer goed"] <- "Goed"
micro[micro == "Goed"] <- "Good"
micro[micro == "Voldoende"] <- "Sufficient"
micro[micro == "Slecht"] <- "Bad"
micro[micro == "Waarschuwingsfase"] <- "Warning"
micro[micro == "Afkeurenswaardig"] <- "Danger"
micro[micro == "Afgekeurd"] <- "Danger"

micro <- factor(micro, levels = c("Good", "Sufficient", "Bad", "Warning", "Danger"))

ripeness[ripeness == "Groen"] <- "Green"
ripeness[ripeness == "Licht"] <- "Light"
ripeness[ripeness == "Gemiddeld"] <- "Average"
ripeness[ripeness == "Sterk"] <- "Strong"
ripeness[ripeness == "Bedorven"] <- "Rotten"
ripeness <- factor(ripeness, levels = c("Light", "Average", "Green", "Strong", "Rotten")) # light is the reference category of Vollaard

cleaning <- cleanness

cleaning[cleaning == "Zeer goed"] <- "Very good"
cleaning[cleaning == "Goed"] <- "Good"
cleaning[cleaning == "Matig"] <- "Poor"
cleaning[cleaning == "Slecht"] <- "Bad"

cleaning <- factor(cleaning, levels = c("Very good", "Good", "Poor", "Bad"))

summary(weight)
str(weight)

summary(temp)
table(temp_cat)
str(temp_cat)

summary(fat)
table(fat_cat)
str(fat_cat)

summary(price)
table(price_cat)
str(price_cat)

table(freshly_cleaned)
str(freshly_cleaned)

table(micro)
str(micro)

table(ripeness)
str(ripeness)

table(cleaning)
str(cleaning)

vollaard.df <- read_excel("datasets/vollaard.xlsx")
# vollaard.df.rescaled <- scale(vollaard.df)
names(vollaard.df)

k30 <- vollaard.df$k30
k30 <- as.character(k30)
k30[k30 == "0"] <- "<30km"
k30[k30 == "1"] <- ">30km"
k30 <- factor(k30, levels = c("<30km", ">30km"))

year <- vollaard.df$yr2017
year <- as.character(year)
year[year == "0"] <- "2016"
year[year == "1"] <- "2017"
year <- factor(year, levels = c("2016", "2017"))

atlantic <- (supplier_AD == "A")

Return <- (Repeat == "*")


## stupid artificial indicator
# first we find out the outlets that are in the most favorable region
most_favorable <- rep(FALSE,nrow(ours.maps.df))
excellent_objective <- rep(FALSE,nrow(ours.maps.df))
for (i in 1:nrow(ours.maps.df)) {
  outlet <- ours.maps.df[i,]
  # outlet_extra <- ours.df[i,]
  grid_i <- as.numeric(round((outlet["lon"]-3.2)/0.04))
  grid_j <- as.numeric(round((outlet["lat"]-50.5)/0.03))
  idx_grid <- (grid_i-1)*100 +grid_j
  # lon_lat_grid[idx_grid,] 
  most_favorable[i] <- (pred.values.contour[idx_grid] + utrecht_drift>6)
  excellent_objective[i] <- (freshly_cleaned[i]=="Yes" & micro[i]=="Good" & cleaning[i]=="Very good" & (temp[i] < 7) & (fat[i]>12))
}

Nrep <- 1000
pv <- rep(-1, Nrep)
for (iter in 1:Nrep) {
  iter <- 68
  set.seed(iter+2023)
electable <- (1:nrow(ours.df))[(most_favorable & excellent_objective)]
ridi_indi <- rep(FALSE,nrow(ours.maps.df))
ridi_set <- (1:nrow(ours.df))[sample(electable, size=10, replace=FALSE)] 
ridi_indi[ridi_set] <- TRUE
ridi_indi <- factor(ridi_indi)

source('./ancillary.R')

final_score_vollaardized <- score_pm_rearrange(final_score,eps_new = 0.2) + 7.5

first.lm <- lm(final_score_vollaardized ~ weight + temp_cat + fat_cat + price_cat + freshly_cleaned + micro + ripeness + cleaning + year + ridi_indi )
# summary(first.lm)
pv[iter] <- tail(summary(first.lm)$coefficients[,4],1)
}
range(pv)
ours.ridi.df <- ours.df[ridi_set,]

## krigging
# generate a SPDF
herring <- ours.maps.df[ours.maps.df$cijfer!=0,]


# sp::coordinates(herring) <- ~ lon + lat
# her_vgm <- gstat::variogram(cijfer~1, herring)
# plot(her_vgm)
# her_fit <- gstat::fit.variogram(her_vgm, model=vgm(psill = 8, "Exp", range = NA, 1))

}