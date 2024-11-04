library(tidyverse)
library(stringr)

##load data 
chips <- read_csv("C:/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/chips.csv") %>% 
  filter(law == "CHIPS")
cd_pres_vote <- read_excel("C:/Users/joshu/OneDrive - Harvard University/Documents/JDH Replication Gov 2020 Package/Replication Package/data/cd_pres_vote.xlsx") %>% 
  select(District, Dem_2020, Dem_2016)

##Vector with CHIPS districts 

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

pt <- ggplot(pol_dat_plot, aes(x = year, y = dem_vote, group = as.character(chips_binary), col = as.character(chips_binary))) + 
  geom_point() + geom_line() + scale_x_continuous(breaks = c(2016, 2020), labels = c(2016, 2020)) + 
  labs(x = "Year", y = "Democratic percent vote",  col = "CHIPS investment")

ggsave(filename = "extension/pt.png", plot = pt, width = 10, height = 6, bg='#ffffff')


##For plotting -- treated vs. rest of CDs in states
pol_dat <- cd_pres_vote %>% 
  pivot_longer(cols = c(Dem_2020, Dem_2016), values_to = "dem_vote", names_to = "year") %>% 
  mutate(year = ifelse(year == "Dem_2020", 2020, 2016)) %>% 
  filter(chips_state == 1)

pol_dat_plot <- pol_dat %>% 
  group_by(chips_binary, year) %>% 
  summarize(dem_vote = mean(dem_vote))

pt_states <- ggplot(pol_dat_plot, aes(x = year, y = dem_vote, group = as.character(chips_binary), col = as.character(chips_binary))) + 
  geom_point() + geom_line() + scale_x_continuous(breaks = c(2016, 2020), labels = c(2016, 2020)) + 
  labs(x = "Year", y = "Democratic percent vote",  col = "CHIPS investment")

ggsave(filename = ""C:\Users\joshu\OneDrive - Harvard University\Documents\JDH Replication Gov 2020 Package\Replication Package\data\Extension"", plot = pt_states, width = 10, height = 6, bg='#ffffff')

