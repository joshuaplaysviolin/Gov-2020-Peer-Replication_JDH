library(tidyverse)
library(janitor)
library(stringi)
library(readxl)
library(writexl)
##load in updated CHIPS data (with county names)
  chips_updated <- read_excel("C:/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/data-AXG8O_updated.xlsx") %>% 
      clean_names() %>% 
      mutate(county = tolower(county)) #%>% 
  
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
          Joining with `by = join_by(county, county_fips, state)`
          geocodes <- geocodes[, c(1:3)]
          ##Merge counties data together 
            counties <- geocodes %>% 
              left_join(., media_markets) %>% 
              left_join(., commuting_zone_20) %>% 
              rename(media_market = tvdma, commuting_zone = cz20)
          Joining with `by = join_by(county, state)`
          Joining with `by = join_by(county_fips)`
          counties <- counties %>% 
              mutate(commuting_zone = ifelse(county_fips == "09001", 388, commuting_zone)) %>% 
              mutate(commuting_zone = ifelse(is.na(commuting_zone), 88, commuting_zone)) ##mismatch between Connecticut counties and new administrative zones
          chips_distinct <- chips_updated %>% 
              group_by(county_fips) %>% 
              summarize(chips_county_project = sum(project_size_num_individual, na.rm = T)) %>% 
              add_column(chips_county = 1)
          ##Merge with counties data
            chips_counties <- counties %>% 
              left_join(., chips_distinct) %>% 
              mutate(chips_county = ifelse(is.na(chips_county), 0, chips_county), 
                                chips_county_project = ifelse(is.na(chips_county_project), 0, chips_county_project)) 
          Joining with `by = join_by(county_fips)`
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
          Error in gzfile(file, "wb") : cannot open the connection
          In addition: Warning message:
            In gzfile(file, "wb") :
            cannot open compressed file ':/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/chips_counties_with_pres_vote.RData', probable reason 'Invalid argument'
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
          # 1. Naive model with clustered SEs at county_fips
            naive_model <- feols(dem_percent ~ treatment, 
                                                        data = df %>% filter(state %in% chips_states), 
                                                        cluster = ~county_fips)
          NOTE: 2 observations removed because of NA values (RHS: 2).
          # 2. Two-way fixed effects (TWFE) model with state and year fixed effects
            model_twfe <- feols(dem_percent ~ treatment | state year, 
                                                      data = df %>% filter(state %in% chips_states), 
                                                      cluster = ~county_fips)
          NOTE: 2 observations removed because of NA values (RHS: 2).
          # 3. Commuting zone fixed effects
            model_cz <- feols(dem_percent ~ treatment | chips_cz year state, 
                                                  data = df %>% filter(state %in% chips_states), 
                                                  cluster = ~county_fips)
          NOTE: 2 observations removed because of NA values (RHS: 2, Fixed-effects: 2).
          # 4. Media market fixed effects
            model_mm <- feols(dem_percent ~ treatment | chips_mm year state, 
                                                  data = df %>% filter(state %in% chips_states), 
                                                  cluster = ~county_fips)
          NOTE: 2 observations removed because of NA values (RHS: 2, Fixed-effects: 2).
          # 5. Media market and commuting zone interaction
            model_mm_cz <- feols(dem_percent ~ treatment | chips_mm * chips_cz year state, 
                                                        data = df %>% filter(state %in% chips_states), 
                                                        cluster = ~county_fips)
          NOTE: 2 observations removed because of NA values (RHS: 2, Fixed-effects: 2).
          summary(model_mm_cz)
          OLS estimation, Dep. Var.: dem_percent
          Observations: 2,536
          Fixed-effects: chips_mm: 2,  chips_cz: 2,  year: 2,  state: 17,  chips_mm:chips_cz: 2
          Standard-errors: Clustered (county_fips) 
          Estimate Std. Error t value  Pr(>|t|)    
          treatment  9.28365    3.04686 3.04696 0.0023595 ** 
            ---
            Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
          RMSE: 13.1     Adj. R2: 0.269049
          Within R2: 0.008172
          
            
            
            library(tidyverse)
          library(stringr)
          ##load data 
            chips <- read_csv("C:/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/chips.csv") %>% 
              filter(law == "CHIPS")
          Rows: 116695 Columns: 31                                                               
          ── Column specification ─────────────────────────────────────────────────────────────────
          Delimiter: ","
          chr (30): what, title, company, amount, summary, external_id, law, aln, subtier_agenc...
          dbl  (1): id
          
          ℹ Use `spec()` to retrieve the full column specification for this data.
          ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
          Warning message:
            One or more parsing issues, call `problems()` on your data frame for details, e.g.:
            dat <- vroom(...)
          problems(dat) 
          cd_pres_vote <- read_excel("C:/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/cd_pres_vote.xlsx") %>% 
              select(District, Dem_2020, Dem_2016)
          chips_districts <- chips$cds                                                         
          chips_districts2 <- c("AZ-09", "AZ-02", "AZ-03", "AZ-01", "AZ-04", "AZ-08", "AZ-07", "AZ-04", "AZ-05", "AZ-02", "CO-05", "CO-07",                               
                                              "ID-02", "ID-01", "NH-02", "NM-01", "NM-03", "NM-02", "NY-20",  "NY-22",  "OH-03", "OH-12", "OH-06",                         
                                              "OR-03", "OR-05",   "OR-01", "OR-06", "TX-31", "TX-17", "TX-10", "TX-37", "TX-35", "TX-10", "TX-17", "TX-21", "VT-00") #36 districts received chips funding 
          #add to political data
            cd_pres_vote <- cd_pres_vote %>% 
              mutate(chips_binary = ifelse(District %in% chips_districts2, 1, 0)) %>% 
              mutate(state = sub("-.*", "", District))
          chips <- cd_pres_vote %>%
              filter(chips_binary == 1)
          chips_states <- chips$state
          cd_pres_vote <- cd_pres_vote %>% 
              mutate(chips_state = ifelse(state %in% chips_states, 1, 0))
          save(cd_pres_vote, file = "C:/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/cd_chips.rdata")
          ##For plotting -- treated vs. rest of country
            pol_dat <- cd_pres_vote %>% 
              pivot_longer(cols = c(Dem_2020, Dem_2016), values_to = "dem_vote", names_to = "year") %>% 
              mutate(year = ifelse(year == "Dem_2020", 2020, 2016))
          pol_dat_plot <- pol_dat %>% 
              group_by(chips_binary, year) %>% 
              summarize(dem_vote = mean(dem_vote))
          `summarise()` has grouped output by 'chips_binary'. You can override using the `.groups`
          argument.
          pt <- ggplot(pol_dat_plot, aes(x = year, y = dem_vote, group = as.character(chips_binary), col = as.character(chips_binary))) 
              geom_point() geom_line() scale_x_continuous(breaks = c(2016, 2020), labels = c(2016, 2020)) 
              labs(x = "Year", y = "Democratic percent vote",  col = "CHIPS investment")
          ggsave(filename = "extension/pt.png", plot = pt, width = 10, height = 6, bg='#ffffff')
          Cannot find directory extension.
          ℹ Would you like to create a new directory?
            
            1: Yes
          2: No
          
          Selection: ##For plotting -- treated vs. rest of CDs in states
            Enter an item from the menu, or 0 to exit
          Selection: pol_dat <- cd_pres_vote %>% 
            Enter an item from the menu, or 0 to exit
          Selection:   pivot_longer(cols = c(Dem_2020, Dem_2016), values_to = "dem_vote", names_to = "year") %>% 
            Enter an item from the menu, or 0 to exit
          Selection:   mutate(year = ifelse(year == "Dem_2020", 2020, 2016)) %>% 
            Enter an item from the menu, or 0 to exit
          Selection:   filter(chips_state == 1)
          Enter an item from the menu, or 0 to exit
          Selection: pol_dat_plot <- pol_dat %>% 
            Enter an item from the menu, or 0 to exit
          Selection:   group_by(chips_binary, year) %>% 
            Enter an item from the menu, or 0 to exit
          Selection:   summarize(dem_vote = mean(dem_vote))
          Enter an item from the menu, or 0 to exit
          Selection: pt_states <- ggplot(pol_dat_plot, aes(x = year, y = dem_vote, group = as.character(chips_binary), col = as.character(chips_binary))) 
            Enter an item from the menu, or 0 to exit
          Selection:   geom_point() geom_line() scale_x_continuous(breaks = c(2016, 2020), labels = c(2016, 2020)) 
            Enter an item from the menu, or 0 to exit
          Selection:   labs(x = "Year", y = "Democratic percent vote",  col = "CHIPS investment")
          Enter an item from the menu, or 0 to exit
          Selection: ggsave(filename = ""C:\Users\joshu\OneDrive - Harvard University\Documents\JDH Replication Gov 2020 Package\Replication Package\data\Extension"", plot = pt_states, width = 10, height = 6, bg='#ffffff')
          Enter an item from the menu, or 0 to exit
          Selection: 
            Enter an item from the menu, or 0 to exit
          Selection: 
            Enter an item from the menu, or 0 to exit
          Selection: 1
          ✔ Created directory: extension.
          rm(list = ls())
          library(tidyverse)
          library(ggmap)
          ℹ Google's Terms of Service: <https://mapsplatform.google.com>
  Stadia Maps' Terms of Service: <https://stadiamaps.com/terms-of-service/>
            OpenStreetMap's Tile Usage Policy: <https://operations.osmfoundation.org/policies/tiles/>
ℹ Please cite ggmap if you use it! Use `citation("ggmap")` for details.
library(ggplot2)
library(sf)
Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.3.1; sf_use_s2() is TRUE
library(tigris)
To enable caching of data, set `options(tigris_use_cache = TRUE)`
in your R script or .Rprofile.
library(rnaturalearth)
library(rnaturalearthhires)
library(ggpattern)
chips <- read.csv("C:/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/chips_districts_votes.csv")
#create congressional district geometry
cd116 <- congressional_districts(cb = TRUE, resolution = "20m", year = 2020)
  |===============================================================================| 100%
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
          ggplot() +
              # Add state borders
              geom_sf(data = us_states, fill = NA, color = "black", size = 0.75)  # State borders
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
              coord_sf(xlim = c(-125, -65), ylim = c(25, 50), expand = FALSE)  # Limit to the U.S. bounds
              theme_void()  # Remove background, grids, and axis labels
              theme(panel.background = element_rect(fill = "white"),
                              plot.title = element_text(hjust = 0.5, face = "bold", size = 20))  # Set background to white 
              labs(title = "CHIPS Funding Allocation by Congressional District and Representative Vote")
          ggsave(""C:\Users\joshu\OneDrive - Harvard University\Documents\JDH Replication Gov 2020 Package\Replication Package\data\Congressional District and Representative Vote.png"", width = 20, height = 7, dpi = 300)