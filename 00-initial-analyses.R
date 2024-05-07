#------------------------------------UCDP Data---------------------------------#
#-Author: Francisca Castro ----------------------------- Created: May 06, 2024-#
#-R Version: 4.4.0 ------------------------------------- Revised: May 06, 2024-#

# 0) Load packages

pacman::p_load(dplyr, sf, ggplot2, magrittr, rmapshaper, lwgeom)

# 1) Load UCDP data

ucdp_events <- readRDS("data/ucdp/ucdp-events.rds")

# 2) Assessment of observations

sudan_data_per_year <- ucdp_events %>%
  filter(country == "Sudan") %>%
  group_by(year) %>%
  summarise(observations = n())


print(sudan_data_per_year, n = Inf) #110 observations for 2022

# 3) Clean dataset

#- Keep only Sudan and only year > 2022

sudan_2022_data <- ucdp_events %>%
  filter(country == "Sudan", year >= 2021) #187 obs for 2021-2022

# 4) Map 

sudan_2022_data_sf <- st_as_sf(sudan_2022_data, wkt = "geom_wkt", crs = 4326)

#- Read Sudan's  shapefile
sudan_shape <- st_read("data/map/shapefiles/sdn_admbnda_adm1_cbs_nic_ssa_20200831.shp")
nile_river <- st_read("data/map/sudan_river_nile_line/Sudan_River_Nile_Line.shp")
nile_river <- st_transform(nile_river, crs = st_crs(sudan_shape)) #transform so is the same crs as sudan

# Intersect the Nile shapefile with the Sudan boundary
sf_use_s2(FALSE)
nile_in_sudan <- st_intersection(nile_river, sudan_shape)

plot(st_geometry(sudan_shape))

conflict_ucdp <- ggplot() +
  geom_sf(data = sudan_shape, fill = "whitesmoke", color = "black") + 
  geom_sf(data = sudan_2022_data_sf, color = "red", size = 1, shape = 21, fill = "red") +  # Add conflict points
  geom_sf(data = nile_in_sudan, color = "deepskyblue", size = 1) +  
  ggtitle("Conflict Locations in Sudan 2021-2022 (UCDP Data)") +
  theme_minimal()

conflict_ucdp

ggsave("outputs/conflict_sudan_ucdp.png", plot = conflict_ucdp, dpi = 300, width = 8, height = 6)

#- Baseline survey locations

baseline_survey <- read_excel("data/baseline/baseline_survey.xlsx")

#- Transform latitude and longitude

coords_data_baseline <- baseline_survey %>%
  select(
    latitude = `1.11.1 Latitude (x.y °)`, 
    longitude = `1.11.2 Longitude (x.y °)`
  ) %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  na.omit()  # Remove rows with NA values

coords_data_baseline_sf <- st_as_sf(coords_data_baseline, coords = c("longitude", "latitude"), crs = 4326)

baseline_map <- ggplot() +
  geom_sf(data = sudan_shape, fill = "whitesmoke", color = "black") +  # Plot Sudan shapefile
  geom_sf(data = coords_data_baseline_sf, color = "blue", size = 1, shape = 21, fill = "blue") +  # Add conflict points
  geom_sf(data = nile_in_sudan, color = "deepskyblue", size = 1) +  
  ggtitle("Locations Baseline Participants (N= 2,985)") +
  theme_minimal()

baseline_map

ggsave("outputs/baseline_map.png", plot = baseline_map, dpi = 300, width = 8, height = 6)


#- ACLED data

acled_sudan <- read_csv("data/acled/acled_sudan.csv")

# Filter out 'Strategic developments' right after reading the data
acled_sudan %<>%
  filter(disorder_type != "Strategic developments")

coords_acled <- st_as_sf(acled_sudan, coords = c("longitude", "latitude"), crs = 4326)

acled_map <- ggplot() +
  geom_sf(data = sudan_shape, fill = "whitesmoke", color = "black") +  # Plot Sudan shapefile
  geom_sf(data = coords_acled, aes(color = disorder_type), size = 1, shape = 21) +  # Add conflict points with colors
  scale_color_manual(values = c("Demonstrations" = "blue2", 
                                "Political violence" = "red2", 
                                "Political violence; Demonstrations" = "magenta1")) +
  ggtitle("Conflict Locations in Sudan Jan 2021-Jan 2023 (ACLED Data)") +
  geom_sf(data = nile_in_sudan, color = "deepskyblue", size = 1) +  
  theme_minimal() +
  labs(color = "Type of Event")  # Add a legend title

acled_map


ggsave("outputs/acled_map.png", plot = acled_map, dpi = 300, width = 8, height = 6)


#- Combined map

combined_map <- ggplot() +
  geom_sf(data = sudan_shape, fill = "lightgray", color = "black") +  # Plot Sudan shapefile
  geom_sf(data = coords_data_baseline_sf, color = "blue", size = 1, shape = 21, fill = "blue") +
  geom_sf(data = sudan_2022_data_sf, color = "red", size = 2, shape = 21, fill = "red") +  
  geom_sf(data = coords_acled, color = "green", size = 2, shape = 21, fill = "green") +  # Add conflict points
  ggtitle("Destribution of Participants and Conflict Data") +
  theme_minimal()

combined_map

ggsave("outputs/combined_map.png", plot = combined_map, dpi = 300, width = 8, height = 6)