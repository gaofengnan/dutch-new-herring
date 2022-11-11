library(dplyr) # not needed, but used in examples below
library(cbsodataR)



# cbs_toc <- cbs_get_toc()
# datasets <- cbs_get_datasets() 
# 
# datasets %>% 
#   filter(Language == "en") %>% # only English tables
#   select(Identifier, ShortTitle) 
# 
# regional_key_figures <- cbsodata4::cbs4_get_data("70072ned")


# tmp <- read_delim("dutch_municipality_2016_area.txt",delim = " ") 
# print(tmp[,1])
# 
# write.csv(tmp[,1], file = 'area.csv',row.names=FALSE)

pop_by_municipality_2016 <- read_excel("dutch_municipality_2016.xlsx")
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
library(dplyr)
 
  
# Retrieve data with municipal boundaries from PDOK
municipalBoundaries <- st_read("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_gemeente_2017_gegeneraliseerd&outputFormat=json")

statcode_2017 <- municipalBoundaries$statcode
statcode_2016 <- pop_by_municipality_2016$statcode

not_2017_idx <- statcode_2016 %in% statcode_2016[!(statcode_2016 %in% statcode_2017)]


pop_density_2017 <- pop_by_municipality_2016[!not_2017_idx,]
pop_density_2017 <- select(pop_density_2017, "Municipality_2017", "statcode", "Population_density")
pop_density_2017 <- rbind(pop_density_2017, c("Meierijstad", "GM1948", 439))
pop_density_2017$Population_density <- as.numeric(pop_density_2017$Population_density)

# Find out which columns are available
metadata <- cbs_get_meta("83765NED")
print(metadata$DataProperties$Key)


data_new <- 
  municipalBoundaries %>%
  left_join(pop_density_2017, by=c(statcode="statcode"))

vendor_plot_title <- "Venders of 2016 and 2017, Dutch New Herring" # (with pop. density)"
# Create a thematic map
data_new %>%
  ggplot() +
  geom_sf(aes(fill = Population_density)) +
  # geom_sf() +
  # scale_fill_viridis_c("pop. density\nby municipality", option = "A") + 
  scale_fill_continuous("pop. density\nby municipality",low = "white", high = "black", guide = "colorbar")+
  geom_sf(aes(shape=year,color=year), alpha = 0.65, size = 1.7, # shape = 20, 
          data = vendors_loc_sf) +
  # scale_shape_manual(values = c(15, 16)) +
  # geom_point(data = ours.maps.df, aes(x = lon, y = lat), size = 4, 
  #            shape = 23, fill = "darkred")+
  labs(title = vendor_plot_title, fill = "") +
  theme_void()
