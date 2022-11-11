library(dplyr) # not needed, but used in examples below
library(cbsodataR)



cbs_toc <- cbs_get_toc()
datasets <- cbs_get_datasets() 

datasets %>% 
  filter(Language == "en") %>% # only English tables
  select(Identifier, ShortTitle) 

regional_key_figures <- cbsodata4::cbs4_get_data("70072ned")


pop <- read_delim("Population_dynamics__region_11112022_101336.csv", 
                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
# pop %>% rename("Population on 1 January (number)", "Population")
pop <- pop[pop$Periods =="2021",]
pop <- pop[!is.na(pop$`Population on 1 January (number)`),]
nrow(pop)
pop$Regions <- str_trim(pop$Regions)

library(tidyverse)
library(sf)

# Find out which columns are available
metadata <- cbs_get_meta("83765NED")
print(metadata$DataProperties$Key)

# Download birth rates and delete spaces from regional identifiers
data <- cbs_get_data("83765NED", 
                     select=c("WijkenEnBuurten","Gemeentenaam_1","GeboorteRelatief_25")) %>%
                     # select=c("WijkenEnBuurten","GeboorteRelatief_25")) %>%
  mutate(WijkenEnBuurten = str_trim(WijkenEnBuurten),
         births = GeboorteRelatief_25)

# Retrieve data with municipal boundaries from PDOK
municipalBoundaries <- st_read("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_gemeente_2017_gegeneraliseerd&outputFormat=json")


# Link data from Statistics Netherlands to geodata
data <- 
  municipalBoundaries %>%
  left_join(data, by=c(statcode="WijkenEnBuurten"))

data$pop_density <- runif(nrow(data))

vendor_plot_title <- "Venders of 2016 and 2017, Dutch New Herring" # (with pop. density)"
# Create a thematic map
data %>%
  ggplot() +
  # geom_sf(aes(fill = pop_density)) +
  geom_sf() +
  # scale_fill_viridis_c("pop. density\nby municipality") + 
  scale_fill_continuous("pop. density\nby municipality",low = "white", high = "black", guide = "colorbar")+
  geom_sf(color = "red", alpha = 0.5, size = 2,fill = "darkred", shape = 20, 
          data = vendors_loc_sf) +
  # geom_point(data = ours.maps.df, aes(x = lon, y = lat), size = 4, 
  #            shape = 23, fill = "darkred")+
  labs(title = vendor_plot_title, fill = "") +
  theme_void()
