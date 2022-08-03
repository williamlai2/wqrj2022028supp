#install.packages("tidyverse")
library(tidyverse)
library(lubridate)

# data download from: https://donnees.ec.gc.ca/data/substances/monitor/surface-water-quality-oil-sands-region/mainstem-water-quality-oil-sands-region/
# data file name: "MainstemWaterQuality-LTWQM-M2-M7-Metals45-EPA-2011-2018-v2.csv"
# since the file name was too long for the supplementary information upload for the WQRJ, it was shortened to "S1_MainstemWaterQuality-EPA-2011-2018-v2.csv" 

# read metals data
d <- read_csv("S1_MainstemWaterQuality-EPA-2011-2018-v2.csv", 
                     skip = 19, col_names = TRUE,
                     locale = locale(encoding = "CP1252"),
                     na = c("NR", "LC", "SI"))


# data wrangling
d %>% 
  # select only those columns containing these metals as examples
  dplyr::select(1:10, matches("ALUMINUM|MOLYBDENUM|NICKEL|VANADIUM|SELENIUM")) %>%
  # rename variable names new_name = old_name (or position)
  rename(sample_id = 1,
         project_name = 2,
         station_location_name = 3,
         station_id = 4,
         lat = 5,
         long = 6,
         sample_date = 7,
         sample_type = 8,
         matrix = 9,
         water_depth = 10,
         aluminum_dissolved = 11, 
         molybdenum_dissolved = 12,
         nickel_dissolved = 13,
         selenium_dissolved = 14,
         vanadium_dissolved = 15,
         aluminum_total = 16,
         molybdenum_total = 17,
         nickel_total = 18,
         selenium_total = 19,
         vanadium_total = 20) %>% 
  # change sample_type string
  mutate(sample_type = str_to_lower(sample_type),
         sample_type = str_replace_all(sample_type, "equal volume transect composite", "volume transect"),
         sample_type = str_replace_all(sample_type, "isokinetic transect composite - ewi", "isokinetic transect"),
         # make date-time object
         sample_date_time = ymd_hm(sample_date),
         # make date object
         sample_date = as_date(sample_date_time),
         # recode stations upstream -> downstream
         station_id = fct_recode(station_id, "M2" = "AL07DD0002",
                                                "M3" = "AL07DD0008",
                                                "M4" =  "AL07DD0004",
                                                "M5" = "AL07DD0005",
                                                "M6" = "AL07DD0009",
                                                "M7" = "AL07DD0007"),
         # relevel stations upstream -> downstream
         station_id = fct_relevel(station_id, "M2", "M3", "M4", "M5", "M6", "M7"),
         # create unique id for sampling at each station and date
         unique_id = paste(station_id, sample_date, sep = "_"),) %>% 
  filter(sample_type %in% "vertical integrated") -> d2


# change data set from wide to long format
d2 %>% 
  pivot_longer(cols = matches("vana|nick|moly|alum|selen"), 
               names_to = c(".value", "type"),
               names_sep = "_") %>% 
  pivot_longer(cols = matches("vana|nick|moly|alum|selen"),
               names_to = "metals",
               values_to = "concentration") -> d3

# exploratory plots
d3 %>% 
  ggplot(aes(water_depth, concentration, col = station_id)) + 
  geom_point()

d3 %>% 
  filter(water_depth < 200) %>% 
  ggplot(aes(water_depth, concentration, col = station_id)) + 
  geom_point()

# keep water depths smaller 200m
# remove NAs
# add month quarter and year variables to highlight them with colours in plots

d3 %>%
  filter(water_depth < 200,
         concentration != is.na(concentration)) %>% 
  mutate(day = day(sample_date), 
         mon = month(sample_date),
         qrt = quarter(sample_date),
         yr = year(sample_date)) -> d4

  
# write updated data set if needed
write_csv(d4, "S1_data_cleaned.csv")
