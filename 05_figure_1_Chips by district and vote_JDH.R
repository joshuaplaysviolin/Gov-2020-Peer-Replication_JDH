rm(list = ls())

library(tidyverse)
library(ggmap)
library(ggplot2)
library(sf)
library(tigris)
library(rnaturalearth)
library(rnaturalearthhires)
library(ggpattern)

chips <- read.csv("C:/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/chips_districts_votes.csv")


#### Map of Chips Districts ####

#create congressional district geometry
cd116 <- congressional_districts(cb = TRUE, resolution = "20m", year = 2020)

#prepare congressional geometry data to be combined with chips data
state_fips <- data.frame(
  state = c("AK", "AL", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  fips = c("02", "01", "04", "05", "06", "08", "09", "10", "12", "13",
           "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
           "25", "26", "27", "28", "29", "30", "31", "32", "33", "34",
           "35", "36", "37", "38", "39", "40", "41", "42", "44", "45",
           "46", "47", "48", "49", "50", "51", "53", "54", "55", "56"))

cd116 <- cd116 %>%
  mutate(
    code = paste0(STATEFP, str_pad(CD116FP, 2, pad = "0")))

chips$code <- str_pad(chips$code, width = 4, pad = "0")

code_list <- chips$code

# Convert cd116 to sf if it isn't already
cd116_sf <- st_as_sf(cd116, wkt = "geometry")

# Filter congressional districts to only those in chips data and add chips voting data
cd116_filtered_sf <- cd116_sf %>%
  filter(code %in% code_list) %>%
  left_join(chips %>% select(code, Party, Vote, `X2020.Vote`), by = "code")

# Load U.S. state boundaries
us_states <- ne_states(country = "united states of america", returnclass = "sf")

## Create Map ##

ggplot() +
  # Add state borders
  geom_sf(data = us_states, fill = NA, color = "black", size = 0.75) +  # State borders
  # Add congressional districts with gradient fill and pattern
  geom_sf_pattern(
    data = cd116_filtered_sf,
    aes(fill = `X2020.Vote`, pattern = as.factor(Vote)),
    color = "black", size = 0.5,
    pattern_density = 0.1,  # Adjust density of the pattern
    pattern_fill = "yellow"  # Color of the pattern lines
  ) +
  scale_fill_gradient2(
    low = "red",       # Color for values less than 50
    mid = "#CBC3E3",   # Neutral color around 50
    high = "blue",     # Color for values greater than 50
    midpoint = 50,     # Middle of the gradient scale
    name = "2020 Presidential Democratic Vote Share (%)"
  ) +
  scale_pattern_manual(
    values = c("0" = "stripe", "1" = "none"),  # Stripe pattern for 0, no pattern for 1
    name = "Representative Vote",  # Custom title for the legend
    labels = c("0" = "Voted Against", "1" = "Voted For")  # Custom labels for the legend
  ) +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50), expand = FALSE) +  # Limit to the U.S. bounds
  theme_void() +  # Remove background, grids, and axis labels
  theme(panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +  # Set background to white 
  labs(title = "CHIPS Funding Allocation by Congressional District and Representative Vote")

ggsave("extension/figure_chips_by_district_and_vote.png", width = 20, height = 7, dpi = 300)
