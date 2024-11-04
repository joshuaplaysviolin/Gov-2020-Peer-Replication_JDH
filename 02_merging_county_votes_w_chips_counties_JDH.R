rm(list = ls())

library(tidyverse)
library(readxl)
library(janitor)

#Load Presidential vote by county 2016 and 2020 
data <- read.csv("C:/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/countypres_2000-2020.csv", colClasses = c(county_fips = "character")) %>%
  filter(year == 2016 | year == 2020) %>%
  filter(party == "DEMOCRAT" | party == "REPUBLICAN")
  

# Create total vote counts for each year-state-county combination
county_total_check <- data %>%
  group_by(year, state, county_name, party) %>%
  summarize(has_total = any(mode == "TOTAL"), .groups = 'drop')

#Calculate sum of 'candidatevotes' for counties that lack a 'TOTAL' mode
new_total_rows <- data %>%
  filter(mode != "TOTAL") %>%
  group_by(year, state, county_name, county_fips, party, totalvotes) %>%
  summarize(candidatevotes = sum(candidatevotes, na.rm = TRUE), .groups = 'drop') %>%
  left_join(county_total_check, by = c("year", "state", "county_name", "party")) %>%
  filter(!has_total) %>%
  mutate(mode = "TOTAL")

#Bind the new 'TOTAL' rows with the original data
final_data <- bind_rows(data, new_total_rows) %>%
  arrange(year, state, county_name, party) %>% 
  filter(mode == "TOTAL")


#Clean and prepare data for merge (2016-2020)
vote <- final_data %>%
  mutate(
    county_fips = str_pad(county_fips, width = 5, pad = "0"), # Add leading zero if needed
    vote_percent = round((candidatevotes / totalvotes) * 100, 2)
  ) %>%
  group_by(year, state, county_name, county_fips, party) %>%
  summarise(vote_percent = mean(vote_percent, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = c(party, year),
    values_from = vote_percent,
    names_sep = "_"
  ) %>%
  rename(
    dem_vote_2016 = DEMOCRAT_2016,
    rep_vote_2016 = REPUBLICAN_2016,
    dem_vote_2020 = DEMOCRAT_2020,
    rep_vote_2020 = REPUBLICAN_2020,
    county = county_name)

## Prep for merge
load("C:/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/chips_counties.rdata")

vote <- vote %>%
  mutate(
    county = tolower(county),
    county = str_replace_all(county, "[[:punct:]]", ""),  # Remove punctuation
    county = str_replace_all(county, " ", ""),  # Remove spaces
    county = str_trim(county),  # Trim leading and trailing spaces
    county = gsub("county|parish|borough|censusarea|municipality", "", county),  # Remove suffixes
    state = tolower(state)
  )

vote <- vote %>%
  mutate(county = ifelse(county_fips == "29510" & county == "stlouiscity", "stlouis", county)) %>%
  filter(county_fips != "2938000")
  
# Further clean and standardize 'county' and 'state' columns in chips_counties
chips_counties <- chips_counties %>%
  mutate(
    county = tolower(county),
    county = str_replace_all(county, "[[:punct:]]", ""),  # Remove punctuation
    county = str_replace_all(county, " ", ""),  # Remove spaces
    county = str_trim(county),  # Trim leading and trailing spaces
    county = gsub("county|parish|borough|censusarea|municipality", "", county),  # Remove suffixes
    state = tolower(state)
  )

# Attempt the merge again
merged_data <- vote %>%
  left_join(chips_counties, by = c("state", "county_fips", "county"))

write.csv(merged_data, "C:/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/chips_counties_with_pres_vote.csv")
save(merged_data, file = ":/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/chips_counties_with_pres_vote.RData")

