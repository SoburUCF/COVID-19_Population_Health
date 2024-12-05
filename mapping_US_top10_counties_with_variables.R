# Date: 12/05/2024
# Author: Sobur Ali
# Project: Role of Crowding and poverty on COVID-19 outcome among at risk communities

# install.packages devtools to intall the urbnmapr packages
devtools::install_github("UrbanInstitute/urbnmapr", force = TRUE)

# Other requried packages
library(tidyverse)
library(urbnmapr)
library(sf)
library(readxl)


# Import COVID-19 disparity data
dispa <- read_excel("data/COVID19_health_disparities_2021-04-20.xlsx", sheet = 1)
dispa$GEOID <- substr(dispa$GEOID, nchar(dispa$GEOID) - 4, nchar(dispa$GEOID)) # modify the GEOID to keep last 5 digit using substr() function

#Import caserate and death rate data
casedeath <- read_excel("data/covid19-county_data_2022-05-19_for_amaya_2022-11-17.xlsx", sheet = 1)

# Add a "0" in the beginning of GEOID to match with the disparity data
casedeath$GEOID <- paste0("0", as.character(casedeath$GEOID))
casedeath <- casedeath %>% select(c("GEOID","State","StateFIPS", "STATENAME", "COUNTYNAME", "Region", "year", "pop_total", "caseRatePer100k", "deathRatePer100k")) 

# Merge the disparity and new case death rate data
dispa <- merge(dispa, casedeath, by = "GEOID", all.x = TRUE)


# Mapping States using urbnmapr
states_sf <- get_urbn_map("states", sf = TRUE)
states_sf %>% 
  ggplot(aes()) +
  geom_sf(fill = "white", color = "black")+
  geom_sf_text(data = get_urbn_labels(map = "states", sf = TRUE), 
               aes(label = state_abbv), 
               size = 3)+
  theme_void()


#Mapping Counites
# Get county shape data from the package urbnmapr
counties_sf <- get_urbn_map("counties", sf = TRUE)

# Rename GEOID
dispa <- dispa %>% rename(county_fips = GEOID)

# Remove any leading or trailing spaces
dispa$county_fips <- trimws(dispa$county_fips)
counties_sf$county_fips <- trimws(counties_sf$county_fips)


# Combine the disparaity data with the county shapefile data
dispa_df <- left_join(counties_sf,dispa, by = "county_fips")



###################################################
################## 1. African American ############
###################################################


# Define the breaks and labels
breaks <- c(0, 2.40, 6.40, 11.20, 20.90, 87.20)
labels <- c("0.00-2.40", "2.41-6.40", "6.41-11.20", "11.21-20.90", "20.91-87.20")

# Create the new column
dispa_sf$african_american <- cut(dispa_sf$per_black_african_amer,
                                 breaks = breaks,
                                 labels = labels,
                                 right = TRUE,
                                 include.lowest = TRUE)

# Convert factor to character and ensure NA values are preserved
dispa_sf$african_american <- as.character(dispa_sf$african_american)
dispa_sf$african_american[is.na(dispa_sf$per_black_african_amer)] <- NA



# Define the color mapping
color_mapping <- c("0.00-2.40" = "#194D33",
                   "2.41-6.40" = "#0D6986",
                   "6.41-11.20" = "#BEDB39",
                   "11.21-20.90" = "#DBA507",
                   "20.91-87.20" = "#DB073D",
                   "Non Included" = "white")

# Convert african_american to a factor with the desired levels
dispa_sf$african_american <- factor(dispa_sf$african_american, levels = c(names(color_mapping)[-which(names(color_mapping) == "Non Included")], "Non Included"))

# Create the plot
dispa_sf %>%
  ggplot() +
  geom_sf(mapping = aes(fill = african_american),
          color = "grey", size = 0.05) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.7) + 
  scale_fill_manual(values = color_mapping, na.value = "white", guide = guide_legend(override.aes = list(fill = color_mapping))) +
  labs(fill = "% African-American") +
  theme_void() +
  geom_sf_text(data = get_urbn_labels(map = "states", sf = TRUE), 
               aes(label = state_abbv), 
               size = 4)+
  theme(
    legend.position = c(0.95, 0.35),# Place the legend at the bottom
    legend.title = element_text(size = 10), # Adjust legend title size
    legend.text = element_text(size = 8)   # Adjust legend text size
  )


###################################################
########## 2. Hispanic/Latine ethnic density ######
###################################################

sort(dispa_sf$per_Hisp_Latino)

# Define the breaks and labels
breaks_hl <- c(0, 4.40, 8.50, 17.10, 31.00, 100)
labels_hl <- c("0.00-4.40", "4.41-8.50", "8.51-17.10", "17.11-31.00", "31.01-100")

# Create the new column
dispa_sf$hispanic_latina <- cut(dispa_sf$per_Hisp_Latino,
                                 breaks = breaks_hl,
                                 labels = labels_hl,
                                 right = TRUE,
                                 include.lowest = TRUE)

# Convert factor to character and ensure NA values are preserved
dispa_sf$hispanic_latina <- as.character(dispa_sf$hispanic_latina)
dispa_sf$hispanic_latina[is.na(dispa_sf$per_Hisp_Latino)] <- NA



# Define the color mapping
color_mapping_hl <- c("0.00-4.40" = "#33691E",
                   "4.41-8.50" = "#1976D2",
                   "8.51-17.10" = "#9C27B0",
                   "17.11-31.00" = "#FA9600",
                   "31.01-100" = "#DB073D",
                   "Non Included" = "white")

# Convert african_american to a factor with the desired levels
dispa_sf$hispanic_latina <- factor(dispa_sf$hispanic_latina, levels = c(names(color_mapping_hl)[-which(names(color_mapping_hl) == "Non Included")], "Non Included"))

# Create the plot
dispa_sf %>%
  ggplot() +
  geom_sf(mapping = aes(fill = hispanic_latina),
          color = "grey", size = 0.005) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.7) + 
  scale_fill_manual(values = color_mapping_hl, na.value = "white", guide = guide_legend(override.aes = list(fill = color_mapping_hl))) +
  labs(fill = "% Hispanic/Latino") +
  theme_void() +
  geom_sf_text(data = get_urbn_labels(map = "states", sf = TRUE), 
               aes(label = state_abbv), 
               size = 4)+
  theme(
    legend.position = c(0.95, 0.35),# Place the legend at the bottom
    legend.title = element_text(size = 10), # Adjust legend title size
    legend.text = element_text(size = 8)   # Adjust legend text size
  )



###################################################
############# 4. Living in Poverty ################
###################################################

sort(dispa_sf$Below_Poverty)

# Define the breaks and labels
breaks_bp <- c(0, 4.9, 9.9, 14.9, 19.9, 36.9)
labels_bp <- c("0-4.9", "5-9.9", "10-14.9", "15-19.9", "20-36.9")

# Create the new column
dispa_sf$poverty <- cut(dispa_sf$Below_Poverty,
                                breaks = breaks_bp,
                                labels = labels_bp,
                                right = TRUE,
                                include.lowest = TRUE)

# Convert factor to character and ensure NA values are preserved
dispa_sf$poverty <- as.character(dispa_sf$poverty)
dispa_sf$poverty[is.na(dispa_sf$Below_Poverty)] <- NA



# Define the color mapping
color_mapping_bp <- c("0-4.9" = "#33691E",
                      "5-9.9" = "#1976D2",
                      "10-14.9" = "#9C27B0",
                      "15-19.9" = "#FA9600",
                      "20-36.9" = "#DB073D",
                      "Non Included" = "white")

# Convert poverty ranges to a factor with the desired levels
dispa_sf$poverty <- factor(dispa_sf$poverty, levels = c(names(color_mapping_bp)[-which(names(color_mapping_bp) == "Non Included")], "Non Included"))

# Create the plot
dispa_sf %>%
  ggplot() +
  geom_sf(mapping = aes(fill = poverty),
          color = "grey", size = 0.005) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.7) + 
  scale_fill_manual(values = color_mapping_bp, na.value = "white", guide = guide_legend(override.aes = list(fill = color_mapping_bp))) +
  labs(fill = "% Living in Poverty") +
  theme_void() +
  geom_sf_text(data = get_urbn_labels(map = "states", sf = TRUE), 
               aes(label = state_abbv), 
               size = 4)+
  theme(
    legend.position = c(0.95, 0.35),# Place the legend at the bottom
    legend.title = element_text(size = 10), # Adjust legend title size
    legend.text = element_text(size = 8)   # Adjust legend text size
  )



###################################################
################## 3. Crowding ####################
###################################################

# As the data is updated for crowding, new data need to added with map data

# Import COVID-19 county level disparity data V2
dispa2 <- read.csv("data/covid19-county_data__2022-05-05.csv")

# GEOID is an integer 
dispa2$GEOID <- as.integer(dispa2$GEOID)

# Add leading zeros and convert to character
dispa2$GEOID <- sprintf("%05d", dispa2$GEOID)

# Maping Sates
states_sf <- get_urbn_map("states", sf = TRUE)


# Get county shape data from the package
counties_sf <- get_urbn_map("counties", sf = TRUE)

# Update the CRS of the counties_sf object
counties_sf <- st_transform(counties_sf, st_crs(counties_sf))
counties_sf <- st_transform(counties_sf, 4326)  # Example with EPSG:4326


# Rename GEOID
dispa2 <- dispa2 %>% rename(county_fips = GEOID)

# Remove any leading or trailing spaces
dispa2$county_fips <- trimws(dispa2$county_fips)
counties_sf$county_fips <- trimws(counties_sf$county_fips)


# Combine the disparaity data with the county shapefile data
dispa_df2 <- left_join(counties_sf,dispa2, by = "county_fips")


# Convert the data in sf file
dispa_sf2 <- st_as_sf(dispa_df2)


# Define the breaks and labels
breaks_cr <- c(0, 1.50, 2.10, 3.10, 4.90, 46.80)
labels_cr <- c("0.00-1.50", "1.51-2.10", "2.11-3.10", "3.11-4.90", "4.91-46.80")

# Create the new column
dispa_sf2$crowd_per <- cut(dispa_sf2$percentCrowding,
                                breaks = breaks_cr,
                                labels = labels_cr,
                                right = TRUE,
                                include.lowest = TRUE)

# To confirm the frequencies within this range
intervals <- cut(dispa_sf2$percentCrowding,
                 breaks = c(0, 1.5, 2.1, 3.1, 4.9, 46.8),
                 include.lowest = TRUE)


# Convert factor to character and ensure NA values are preserved
dispa_sf2$crowd_per <- as.character(dispa_sf2$crowd_per)
dispa_sf2$crowd_per[is.na(dispa_sf2$percentCrowding)] <- NA



# Define the color mapping
color_mapping_cr <- c("0.00-1.50" = "#33691E",
                      "1.51-2.10" = "#1976D2",
                      "2.11-3.10" = "#9C27B0",
                      "3.11-4.90" = "#FA9600",
                      "4.91-46.80" = "#DB073D",
                      "Non Included" = "white")

# Convert range to a factor with the desired levels
dispa_sf2$crowd_per <- factor(dispa_sf2$crowd_per, levels = c(names(color_mapping_cr)[-which(names(color_mapping_cr) == "Non Included")], "Non Included"))

# Create the plot
dispa_sf2 %>%
  ggplot() +
  geom_sf(mapping = aes(fill = crowd_per),
          color = "grey", size = 0.005) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.7) + 
  scale_fill_manual(values = color_mapping_cr, na.value = "white", guide = guide_legend(override.aes = list(fill = color_mapping_cr))) +
  labs(fill = "% Crowding") +
  theme_void() +
  geom_sf_text(data = get_urbn_labels(map = "states", sf = TRUE), 
               aes(label = state_abbv), 
               size = 4)+
  theme(
    legend.position = c(0.95, 0.35),# Place the legend at the bottom
    legend.title = element_text(size = 10), # Adjust legend title size
    legend.text = element_text(size = 8)   # Adjust legend text size
  )



#########################################################
### Mapping Case rate and Death Rate with the new data###
#########################################################

# Case Rate

# Replace 0 values in caseRate with NA
dispa_sf2$caseRate[dispa_sf2$caseRate == 0] <- NA

# Map for death rate (caseRate in this case)
dispa_sf2 %>% 
  ggplot() +
  geom_sf(mapping = aes(geometry = geometry, fill = caseRate), color = "grey", size = 0.005) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.7) + 
  scale_fill_gradientn(
    name = "Case Rate",
    colors = c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58'),
    values = scales::rescale(c(min(dispa_sf2$caseRate, na.rm = TRUE), max(dispa_sf2$caseRate, na.rm = TRUE))),
    na.value = "white" # White color for NA values
  ) +
  theme_void() +
  geom_sf_text(data = get_urbn_labels(map = "states", sf = TRUE), 
               aes(label = state_abbv), 
               size = 3) +
  theme(legend.position = c(0.96, 0.4),   # Place the legend
        legend.title = element_text(size = 10), # Legend title size
        legend.text = element_text(size = 8))   # Legend text size


# Death Rate

# Replace 0 values in caseRate with NA
dispa_sf2$caseRate[dispa_sf2$caseRate == 0] <- NA

# Map for death rate (caseRate in this case)
dispa_sf2 %>% 
  ggplot() +
  geom_sf(mapping = aes(geometry = geometry, fill = deathRate), color = "grey", size = 0.005) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.7) + 
  scale_fill_gradientn(
    name = "Death Rate",
    colors = c('#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026'),
    values = scales::rescale(c(min(dispa_sf2$deathRate, na.rm = TRUE), max(dispa_sf2$deathRate, na.rm = TRUE))),
    na.value = "white" # White color for NA values
  ) +
  theme_void() +
  geom_sf_text(data = get_urbn_labels(map = "states", sf = TRUE), 
               aes(label = state_abbv), 
               size = 3) +
  theme(legend.position = c(0.96, 0.4),   # Place the legend
        legend.title = element_text(size = 10), # Legend title size
        legend.text = element_text(size = 8))   # Legend text size






