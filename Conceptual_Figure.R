# https://rviews.rstudio.com/2021/12/08/the-r-package-covid19/
# https://github.com/joachim-gassen/tidycovid19
# 
require(sf)
require(mapview)
require(raster)
library(rnaturalearth)
require("rnaturalearthdata")
require(wesanderson)
require(viridis)
require(terra)
require(COVID19) # See also https://github.com/joachim-gassen/tidycovid19
require(tidycovid19) # https://rviews.rstudio.com/2021/12/08/the-r-package-covid19/
library(dplyr)
library(ggplot2)

# Load old RS layers - can be skipped ####
# bio1 = raster('/Users/diegoellis/Downloads/CHELSA_bio1_1981-2010_V.2.1.tif')
# bio1_usa = raster('/Users/diegoellis/Downloads/Bioclims/CHELSA_bio1_1981-2010_v2-1_NA30x30_cropped.tif')
# continents <- ne_countries(scale = "medium", returnclass = "sf")
# america_continents <- continents[continents$continent %in% c("North America", "South America"), ]
# america_continents <- st_transform(america_continents, crs(bio1))
# bio1_americas_masked <- mask(bio1, america_continents)
# mapview(bio1_americas_masked)
# writeRaster(bio1_americas_masked, file = '/Users/diegoellis/Downloads/bio1_americas_masked.tif')
# Load chelsa biolcim
# bio1_americas = raster('/Users/diegoellis/Downloads/bio1_americas_masked.tif')
# # Load human footprint
# human_mod = raster('/Users/diegoellis/Downloads/7283087/gHM/gHM.tif')

# america_continents <- st_transform(america_continents, crs(human_mod))
# human_mod_americas_masked <- mask(human_mod, america_continents)
# human_mod_americas_masked = crop(human_mod_americas_masked, america_continents)
# human_mod_americas_masked = raster::projectRaster(human_mod_americas_masked, crs = crs(bio1_americas))
# writeRaster(human_mod_americas_masked, file = '/Users/diegoellis/Downloads/hmod_americas_masked.tif')
# bio1_extent <- ext(-180.000, 179.9999, -90.00014, 83.99986)
####

# Cropped RS Vars ####
# Crop and mask the heatwave raster to match the bio1_masked extent
heatwave_days = raster('/Users/diegoellis/Downloads/PressPulsePause/sdei-high-res-extreme-heat-estimates-1983-2016-wbgtmax30-counts-geotiff/GEHE-v1-wbgtmax30-counts-1983-2016/wbgtmax30.count.2016.tif')
continents <- ne_countries(scale = "medium", returnclass = "sf")
america_continents <- continents[continents$continent %in% c("North America", "South America"), ]
america_continents_trans = st_transform(america_continents, crs(heatwave_days))
# heatwave_days_masked <- crop(heatwave_days, bio1_extent)



# Easy stringency index: https://github.com/terranovafr/Covid19StringencyIndexClassification?tab=readme-ov-file
# Can do this by country: https://como-ph.github.io/oxcgrt/reference/calculate_index.html
# Load Puma rangemap
puma_rangemap = st_read('/Users/diegoellis/Downloads/PressPulsePause/redlist_species_data_43a589bb-8d87-4bf9-8efc-ef3646dfa69c/data_0.shp') |>
  st_transform(,crs(bio1))
puma_rangemap = st_read('/Users/diegoellis/Downloads/redlist_species_data_ba42152d-ac5f-4203-a17f-84069b293cd5/data_0.shp') |>
  st_transform(,crs(bio1))

# Load current press 
human_mod_americas_masked = raster('/Users/diegoellis/Downloads/PressPulsePause/hmod_americas_masked.tif')
continents <- ne_countries(scale = "medium", returnclass = "sf")
bio1_masked = raster('/Users/diegoellis/Downloads/PressPulsePause/bio1_americas_masked.tif')
# bio1_cropped <- crop(bio1, puma_rangemap)
# Clip and keep

human_mod_americas_masked_clipped = mask(human_mod_americas_masked, puma_rangemap)
heatwave_days_masked = mask(heatwave_days, puma_rangemap)


bio1_masked_americas = mask(bio1_masked, puma_rangemap)

# Mapview
custom_palette <- viridis(30, option = "C", begin = 0.1, end = 0.8)

# Human footprint
map_h <- mapview(human_mod_americas_masked_clipped, col.regions = custom_palette) +
  mapview(puma_rangemap, col.regions = "transparent",
          alpha.regions = 0.1,
          color='black',
          lwd=3)
# Center to the US
# map_h@map = map_h@map %>% leaflet::setView(lng = -95.7129, lat = 37.0902, zoom = 4)
map_h@map <- map_h@map %>% leaflet::setView(lng = -51.9253, lat = -14.2350, zoom = 4)

# map_h@map <- map_h@map %>% leaflet::setView(lng = -74.2973, lat = 4.5709, zoom = 6)

map_h

# Mean annual temperature
wes_palette <- wes_palette("Zissou1", 100, type = "continuous")

map_t <- mapview(bio1_masked_americas, col.regions = wes_palette) +
  mapview(
    puma_rangemap,
    col.regions = "transparent",    
    color = "black",                
    alpha.regions = 0.0,            
    lwd = 2                         
  )
# Center to the US
map_t@map = map_t@map %>% leaflet::setView(lng = -95.7129, lat = 37.0902, zoom = 4)
map_t@map <- map_t@map %>% leaflet::setView(lng = -51.9253, lat = -14.2350, zoom = 4)

map_t

# Heatwaves
wes_palette <- wes_palette("Zissou1", 50, type = "continuous")
map_heat <- mapview(heatwave_days_masked, col.regions = wes_palette) +
  mapview(
    puma_rangemap,
    col.regions = "transparent",    
    color = "black",                
    alpha.regions = 0.0,            
    lwd = 2                         
  )
# Center to the US
# map_h@map = map_h@map %>% leaflet::setView(lng = -95.7129, lat = 37.0902, zoom = 4)
map_heat@map <- map_heat@map %>% leaflet::setView(lng = -51.9253, lat = -14.2350, zoom = 4)

map_heat



# Look at Pauses: 
x <- covid19() |>
  dplyr::filter(date == '2020-04-20') |> dplyr::select(iso_alpha_3, iso_alpha_2, date, 
                                                       stringency_index,
                                                       administrative_area_level_1,
                                                       international_movement_restrictions, 
                                                       internal_movement_restrictions)

covid_april_20 <- x %>%
filter(date == "2020-04-20") %>%
select(iso_alpha_3, stringency_index, internal_movement_restrictions)

# Join COVID data with spatial data
america_with_covid <- america_continents %>%
  left_join(covid_april_20, by = c("adm0_a3" = "iso_alpha_3")) |>
  dplyr::select(stringency_index, adm0_a3)

america_with_covid <- america_continents %>%
  left_join(covid_april_20, by = c("adm0_a3" = "iso_alpha_3")) |>
  dplyr::select(internal_movement_restrictions,
                stringency_index,
                adm0_a3)

covid_within_puma <- st_intersection(america_with_covid, puma_rangemap)

map_covid <- mapview(
  covid_within_puma,
  zcol = "stringency_index",  # Specify the column for color scaling
  # zcol = "internal_movement_restrictions",  # Specify the column for color scaling
  col.regions = wes_palette("Zissou1", 9, type = "continuous")  # Use a color palette
) +
  mapview(
    puma_rangemap,
    col.regions = "transparent",    
    color = "black",                
    alpha.regions = 0.0,            
    lwd = 2                         
  )
# Center to the US
# map_h@map = map_h@map %>% leaflet::setView(lng = -95.7129, lat = 37.0902, zoom = 4)
map_covid@map <- map_covid@map %>% leaflet::setView(lng = -51.9253, lat = -14.2350, zoom = 4)

map_covid



# st_intersection(america_with_covid, heatwave_days_masked)

# america_with_covid <- st_transform(america_with_covid, crs = crs(heatwave_days_masked))
# heatwave_days_masked_cropped <- crop(heatwave_days_masked, extent(america_with_covid))
# 


# Plot the map
ggplot(data = america_with_covid) +
  geom_sf(aes(fill = stringency_index), color = "black") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", name = "Stringency Index") +
  labs(
    title = "COVID-19 Stringency Index in the Americas",
    subtitle = "April 20, 2020",
    caption = "Data: COVID-19 Dataset & Natural Earth"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )



# Get specific mobility and stringency
# 43 key_google_mobility

covid19 = read.csv('/Users/diegoellis/Downloads/PressPulsePause/owid-covid-data.csv') |>
  dplyr::filter(continent %in% c('North America', 'South America') & date == '2020-04-20') |>
  dplyr::select(location, date, stringency_index, iso_code)

ggplot() +
  geom_sf(data = puma_rangemap, fill = "lightgray", color = "black") +  # Puma range map
  geom_sf(data = covid_within_puma, aes(fill = stringency_index), color = "blue") + # America with COVID
  scale_fill_viridis_c(option = "plasma", na.value = "white") +  # Add color gradient for stringency index
  labs(title = "America with COVID Cases within Puma Range",
       fill = "Stringency Index") +
  theme_minimal()

# Plot number of heatwave events from the remote sensing layer from NASA: Different number of pulse events and the stringency index in April across its species range ! By country !!!!

# Then at the individual level

# Different number of future cities expansions

# Assuming `x` is your COVID data
# Assuming `america_continents` is your spatial data for the Americas

