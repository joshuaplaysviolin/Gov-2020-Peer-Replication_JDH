rm(list = ls())

library(tidyverse)
library(lfe)
library(fixest)
library(modelsummary)

load("C:/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/chips_counties_with_pres_vote.RData")


chips_states <- c(
  "arizona", "colorado", "florida", "georgia", "idaho", "indiana",
  "minnesota", "missouri", "north carolina", "new hampshire", "new mexico",
  "new york", "ohio", "oregon", "texas", "utah", "vermont"
)


#organize data for models
df <- merged_data %>% 
  filter(state %in% chips_states) %>%
  select(state, county, county_fips, dem_vote_2016, dem_vote_2020, chips_county_project, chips_county, chips_mm, chips_cz) %>%
  pivot_longer(cols = c(dem_vote_2016, dem_vote_2020),
               names_to = "year",
               values_to = "dem_percent",
               names_prefix = "dem_vote_",
               names_transform = list(year = as.integer)) %>%
  mutate(treatment = chips_county, 
         post = ifelse( year == 2020, 1, 0))


# DID Models --------------------------------------------------------------

# 1. Naive model with clustered SEs at county_fips
naive_model <- feols(dem_percent ~ treatment, 
                     data = df %>% filter(state %in% chips_states), 
                     cluster = ~county_fips)

# 2. Two-way fixed effects (TWFE) model with state and year fixed effects
model_twfe <- feols(dem_percent ~ treatment | state + year, 
                    data = df %>% filter(state %in% chips_states), 
                    cluster = ~county_fips)

# 3. Commuting zone fixed effects
model_cz <- feols(dem_percent ~ treatment | chips_cz + year + state, 
                  data = df %>% filter(state %in% chips_states), 
                  cluster = ~county_fips)

# 4. Media market fixed effects
model_mm <- feols(dem_percent ~ treatment | chips_mm + year + state, 
                  data = df %>% filter(state %in% chips_states), 
                  cluster = ~county_fips)

# 5. Media market and commuting zone interaction
model_mm_cz <- feols(dem_percent ~ treatment | chips_mm * chips_cz + year + state, 
                     data = df %>% filter(state %in% chips_states), 
                     cluster = ~county_fips)

summary(model_mm_cz)

