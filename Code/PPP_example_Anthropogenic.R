# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# Replace population numbers with human footprint?
# Please revise the mobiltiy plot:

library(tidyverse)
library(WDI)      
library(tidycovid19)    # For Google Mobility & Oxford COVID-19 data
library(lubridate)      # For date manipulations
library(cowplot)        # For arranging multiple plots
library(WDI)

###############################################################################
# 1. Increase in Human Population (Press) ---------------------------------------

# 2A. Download global population data from 1980 to 2021
pop_data <- WDI(
  country = "1W",            # 1W = world. # UY
  indicator = "SP.POP.TOTL", # total population
  start = 1980,
  end   = 2021
)

# 2B. Clean and filter
pop_data_clean <- pop_data %>%
  rename(
    population = SP.POP.TOTL,
    Year = year
  ) %>%
  filter(!is.na(population))

# 2C. Plot with “press” styling
population_plot <- ggplot(pop_data_clean, aes(x = Year, y = population)) +
  geom_line(size = 1.5, color = "firebrick") +        # thick red line
  geom_smooth(color = "black", se = FALSE, size = 1.5) + # optional trend line
  theme_classic(base_size = 16) +
  labs(
    x = "Year",
    y = "Population",
    title = "Global Population Over Time"
  ) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.key.size = unit(1.5, "lines")
  )

# Display population plot
print(population_plot)


###############################################################################
# 2. Dates of the Summer Olympic Games (Event Timeline) ------------------------
#    Analogous to the ENSO event timeline, but using real human-event data.
###############################################################################
# Here, we create a small dataset of Summer Olympics. 
# You could also do “German elections” if you have a direct dataset.

olympics_data <- tibble::tribble(
  ~Year, ~City,           ~Country,
  1896,  "Athens",        "Greece",
  1900,  "Paris",         "France",
  1904,  "St. Louis",     "USA",
  1908,  "London",        "UK",
  1912,  "Stockholm",     "Sweden",
  # 1916: Cancelled (WWI)
  1920,  "Antwerp",       "Belgium",
  1924,  "Paris",         "France",
  1928,  "Amsterdam",     "Netherlands",
  1932,  "Los Angeles",   "USA",
  # 1936,  "Berlin",        "Germany",   # add more if you like
  # 1940/44: Cancelled (WWII)
  1948,  "London",        "UK",
  1952,  "Helsinki",      "Finland",
  1956,  "Melbourne",     "Australia",
  1960,  "Rome",          "Italy",
  1964,  "Tokyo",         "Japan",
  1968,  "Mexico City",   "Mexico",
  1972,  "Munich",        "West Germany",
  1976,  "Montreal",      "Canada",
  1980,  "Moscow",        "USSR",
  1984,  "Los Angeles",   "USA",
  1988,  "Seoul",         "South Korea",
  1992,  "Barcelona",     "Spain",
  1996,  "Atlanta",       "USA",
  2000,  "Sydney",        "Australia",
  2004,  "Athens",        "Greece",
  2008,  "Beijing",       "China",
  2012,  "London",        "UK",
  2016,  "Rio de Janeiro","Brazil",
  2020,  "Tokyo",         "Japan"  # Held in 2021 due to COVID, but "official" 2020
)

# For an "event timeline," we can just plot points or bars at each Olympic year.
olympics_plot <- ggplot(olympics_data, aes(x = Year, y = 1)) +
  geom_point(color = "blue", size = 3) +
  geom_segment(aes(xend = Year, y = 0.95, yend = 1), color = "blue") +
  scale_y_continuous(limits = c(0.9, 1.1), breaks = NULL) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Summer Olympic Games Timeline",
    x = "Year",
    y = ""
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.text.x  = element_text(size = 12),
    axis.ticks.y = element_blank(),
    axis.text.y  = element_blank()
  )


# Let's focus on a single country, e.g., Germany (country_code = "DE").
# Then plot "Retail & recreation" mobility changes vs. time,
# highlighting “weekend” vs. “weekday.”

head(x)
retail_and_recreation_percent_change_from_baseline
google_cmr = x

google_de <- google_cmr %>%
  filter(iso_alpha_2 == "DE") %>%
  mutate(
    date = as.Date(date),
    day_of_week = wday(date, label = TRUE),  # lubridate
    weekend     = day_of_week %in% c("Sat", "Sun")
  )



# Refine and look into end of 2020 december those two red spikes ####
# Build a short-term pulse plot of weekend vs weekday mobility.
ggplot(google_de, aes(x = date, y = retail_and_recreation_percent_change_from_baseline)) +
  geom_line(aes(color=weekend), size = 1) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"),
                     labels = c("Weekday", "Weekend")) +
  theme_classic(base_size = 14) +
  labs(
    title = "Retail & Recreation Mobility in Germany (Google CMR)",
    x = "Date",
    y = "Change from Baseline (%)",
    color = "Day Type"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    axis.text  = element_text(size = 12),
    legend.position = "top"
  )+xlim(as.Date(c('2020-09-01', '2020-10-01')))


ggplot(google_de, aes(x = date, y = retail_and_recreation_percent_change_from_baseline)) +
#  geom_line(aes(color=weekend), size = 1) +
  geom_line(color='brown', size = 1) +
  # scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"),
  #                    labels = c("Weekday", "Weekend")) +
  theme_classic(base_size = 14) +
  labs(
    title = "Anthropulse Retail & Recreation Mobility in Germany (Google CMR)",
    x = "Date",
    y = "Change from Baseline (%)",
    color = "Day Type"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    axis.text  = element_text(size = 12),
    legend.position = "top"
  )+xlim(as.Date(c('2020-12-01', '2021-01-31')))




mobility_plot <- ggplot(google_de, aes(x = date, y = retail_and_recreation_percent_change_from_baseline)) +
  geom_line(aes(color = weekend), size = 1) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"),
                     labels = c("Weekday", "Weekend")) +
  theme_classic(base_size = 14) +
  labs(
    title = "Retail & Recreation Mobility in Germany (Google CMR)",
    x = "Date",
    y = "Change from Baseline (%)",
    color = "Day Type"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    axis.text  = element_text(size = 12),
    legend.position = "top"
  )


# Look at Anthropause Argentina ####


# google_uru <- google_cmr %>%
#   filter(iso_alpha_2 == "UY") %>%
#   mutate(
#     date = as.Date(date),
#     day_of_week = wday(date, label = TRUE),  # lubridate
#     weekend     = day_of_week %in% c("Sat", "Sun")
#   )

google_uru <- google_cmr %>%
  filter(iso_alpha_2 == "UY") %>%
  mutate(
    date = as.Date(date),
    day_of_week = wday(date, label = TRUE),  # lubridate
    weekend     = day_of_week %in% c("Sat", "Sun")
  )


ggplot(google_arg, aes(x = date, y = retail_and_recreation_percent_change_from_baseline)) +
  geom_line(color = 'brown', size = 1) +
  theme_classic(base_size = 14) +
  labs(
    title = "Retail & Recreation Mobility in Germany (Google CMR)",
    x = "Date",
    y = "Change from Baseline (%)",
    color = "Day Type"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    axis.text  = element_text(size = 12),
    legend.position = "top"
  )+xlim(as.Date(c('2020-03-01', '2021-12-31')))


# Refine and look into end of 2020 december those two red spikes ####
# Build a short-term pulse plot of weekend vs weekday mobility.
mobility_plot <- ggplot(google_arg, aes(x = date, y = retail_and_recreation_percent_change_from_baseline)) +
  geom_line(aes(color = weekend), size = 1) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"),
                     labels = c("Weekday", "Weekend")) +
  theme_classic(base_size = 14) +
  labs(
    title = "Retail & Recreation Mobility in Germany (Google CMR)",
    x = "Date",
    y = "Change from Baseline (%)",
    color = "Day Type"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    axis.text  = element_text(size = 12),
    legend.position = "top"
  )


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Change in HFI through time
# Make Insets
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

require(terra)
library(sf)
library(ggplot2)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)

a = raster('/Users/diegoellis/Downloads/hfp2000.tif')
Stack = rast(a)
terra::global(Stack, "mean", na.rm=TRUE)
# Chage in mean human footprint across countries?
# Or make stimuli across all geographic variaiton|

# Obtain world country data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define the countries for insets
inset_countries <- c("Brazil", "Australia", "India", "South Africa")

# Extract the selected countries
countries_sf <- world[world$name %in% inset_countries, ]
# countries_sf |> st_transform(st_crs(Stack)) # |> mapview()

countries_sf_tmp = raster::extract(raster(Stack), countries_sf)





























# countries_sf_hfp = countries_sf |> 
#   st_intersection() # |> terra::global("mean", na.rm=TRUE)
# 
# # Assign each country to a list for easier handling
# countries_list <- split(countries_sf, countries_sf$name)
# 
# # Create the global base map
# global_map <- ggplot(data = world) +
#   geom_sf(fill = "antiquewhite") +
#   theme_minimal() +
#   theme(
#     panel.background = element_rect(fill = "lightblue"),
#     panel.grid = element_line(color = "gray70")
#   ) +
#   labs(title = "Global Map with Country Insets",
#        subtitle = "Showing Selected Countries with Mean Values")
# 
# create_inset <- function(country_sf, main_map_extent) {
#   ggplot(data = country_sf) +
#     geom_sf(fill = "black") +
#     theme_void() +
#     theme(panel.border = element_rect(color = "black", fill=NA, size=0.5))
# }
# 
# inset_plots <- lapply(countries_list, create_inset)
# 
# # Check CRS of raster and vector data
# raster_crs <- crs(Stack)
# vector_crs <- st_crs(world)$proj4string
# 
# # If they differ, transform the vector data to match raster CRS
# if (raster_crs != vector_crs) {
#   countries_sf <- st_transform(countries_sf, crs = raster_crs)
# }
# 
