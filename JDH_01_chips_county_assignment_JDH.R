library(tidyverse)
library(janitor)
library(stringi)
library(readxl)
library(writexl)


##load in updated CHIPS data (with county names)
chips_updated <- read_excel("C:/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/data-AXG8O_updated.xlsx") %>% 
  clean_names() %>% 
  mutate(county = tolower(county)) #%>% 
  #mutate(project_size_num_individual = as.numeric(format(project_size_num_individual, scientific = FALSE)))

##load in commuting zone data https://sites.psu.edu/psucz/data/ 
commuting_zone_20 <- read_excel("C:/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/commuting zone 20.xlsx") %>% 
  clean_names() %>% 
  rename(county_fips = geoid)

##and media market data https://public.tableau.com/app/profile/amirgnyawali/viz/TVDMAmap/Sheet1
media_markets <- read_excel("C:/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/media_markets.xlsx") %>% 
  clean_names() %>% 
  mutate(county = tolower(county), state = tolower(state)) %>%
  mutate(county = str_trim(county, side = "both"), state = str_trim(state, side = "both"))

media_markets <- media_markets[, c(1:3)]

#data to match with county fips codes
geocodes <- read_excel("C:/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/all-geocodes-v2020.xlsx") %>% 
  clean_names() %>% 
  rename(county = area_name_including_legal_statistical_area_description) %>% 
  mutate(county_fips = paste0(state_code_fips, county_code_fips, sep = "")) %>% 
  select(county, county_fips) %>% 
  distinct(county_fips, .keep_all = T) %>% 
  filter(county != "United States") %>% 
  mutate(state = ifelse(endsWith(county_fips, '000') == "TRUE", county, NA)) %>% 
  fill(state, .direction = "down") %>% 
  filter(county != state) %>% 
  mutate(county = tolower(county), state = tolower(state)) %>% 
  mutate(county = gsub("county", "", county), county = gsub("parish", "", county), county = gsub("census area", "", county), 
         county = gsub("municipality", "", county), county = gsub("borough", "", county), county = gsub("city", "", county), ) %>% 
  mutate(county = str_trim(county, side = "both"), state = str_trim(state, side = "both")) %>% 
  mutate(county = gsub('[[:punct:] ]+', ' ', county)) %>% 
  mutate(county = stringi::stri_trans_general(county, "Latin-ASCII")) %>% 
  filter(state != "puerto rico")
 
##final county cleaning
c_matching <- read_excel("C:/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/c_matching.xlsx")[, c(1:3)] %>% 
  mutate(county_new = gsub("de", "de ", county)) %>%
  mutate(county_new = gsub("du", "du ", county_new)) %>% 
  mutate(county_new = gsub("la", "la ", county_new)) %>% 
  mutate(county_new = ifelse(county_new == "hills", "hillsborough", county_new)) %>%
  add_column(x = "x")


geocodes <- geocodes %>% 
  left_join(., c_matching) %>%
  mutate(county = ifelse(!is.na(x), county_new, county)) 

geocodes <- geocodes[, c(1:3)]


##Merge counties data together 
counties <- geocodes %>% 
  left_join(., media_markets) %>% 
  left_join(., commuting_zone_20) %>% 
  rename(media_market = tvdma, commuting_zone = cz20)

counties <- counties %>% 
  mutate(commuting_zone = ifelse(county_fips == "09001", 388, commuting_zone)) %>% 
  mutate(commuting_zone = ifelse(is.na(commuting_zone), 88, commuting_zone)) ##mismatch between Connecticut counties and new administrative zones


##consolidate CHIPS data to county level 

chips_distinct <- chips_updated %>% 
  group_by(county_fips) %>% 
  summarize(chips_county_project = sum(project_size_num_individual, na.rm = T)) %>% 
  add_column(chips_county = 1)

##Merge with counties data
chips_counties <- counties %>% 
  left_join(., chips_distinct) %>% 
  mutate(chips_county = ifelse(is.na(chips_county), 0, chips_county), 
         chips_county_project = ifelse(is.na(chips_county_project), 0, chips_county_project)) 

##Media markets and commuting zones with CHIPS investment 
chip <- chips_counties %>% 
  filter(chips_county == 1)

chip_mm <- chip$media_market

chip_cz <- chip$commuting_zone

chips_counties <- chips_counties %>% 
  mutate(chips_mm = ifelse(media_market %in% chip_mm, 1, 0)) %>% 
  mutate(chips_cz = ifelse(commuting_zone %in% chip_cz, 1, 0))



##One strategy we discussed: same media market but different commuting zone
chips_counties_mm <- chips_counties %>% 
  filter(chips_mm == 1)

save(chips_counties, chips_counties_mm, file = "C:/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/chips_counties.rdata")

