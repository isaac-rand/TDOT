library(tidyverse)
library(sf)
library(tmap)

#----------------------------------------------------
#read in data

#trims data comes spatial
trims <- st_read("data/edls_etrims.shp")
#tntimes data read as csv, then converted to spatial. Converted to same crs as trims data (EPSG:2274)
tntimes <- read_csv("data/edls_tntimes.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(st_crs(trims)) %>%
  rename("station_id" = `Loc ID`)
#data from Chris stays as a csv
chris <- read_csv("data/edls_chris.csv")

#county names and numbers
counties <- read_csv("data/County Numbers.csv")

#----------------------------------------------------

#CREATING STATION ID IN CHRIS' DATA

#adding county numbers to Chris' data to use for id
chris <- chris %>%
  select(c(jj, `Route Feature Description`)) %>% 
  left_join(counties, by = c("jj" = "County Name"))

#parsing station number from route feature description
chris$station_number <- chris$`Route Feature Description` %>%
  #matches all digits until the first non digit after the digit
  #has the effect of matching all digits after # but before some other interrupting character (mostly ], sometimes space or )
  str_extract(pattern = '\\d+\\D') %>%
  #drops the interrupting character from the match before, leaves only the digits we want
  str_extract(pattern = '\\d+')

#creates a new field to store appropriate number of zeroes for station id
#first line creates all the zeroes, second two clean them up so they are a string without extra characters
chris$zeroes = map(8 - str_length(chris$station_number) - 2, ~rep('0', .)) %>%
  map(~toString(.)) %>%
  map(~str_remove_all(., pattern = '\\D+'))

#Creates actual station id (county number + zeroes + station number)
chris$station_id <- chris$`County Number` %>%
  paste(chris$zeroes, sep = "") %>%
  paste(chris$station_number, sep = "")

chris <- distinct(chris, station_id, .keep_all = TRUE)

#CREATING STATION ID IN TRIMS DATA (basically identical to process for Chris' data)

#adding county numbers to TRIMS' data to use for id
trims <- trims %>%
  left_join(counties, by = c("NBR_TENN_C" = "County Name"))

#parsing station number from route feature description
trims$station_number <- trims$RTE_FEAT_D %>%
  #matches all digits until the first non digit after the digit
  #has the effect of matching all digits after # but before some other interrupting character (mostly ], sometimes space or )
  str_extract(pattern = '\\d+\\D') %>%
  #drops the interrupting character from the match before, leaves only the digits we want
  str_extract(pattern = '\\d+')

#creates a new field to store appropriate number of zeroes for station id
#first line creates all the zeroes, second two clean them up so they are a string without extra characters
trims$zeroes = map(8 - str_length(trims$station_number) - 2, ~rep('0', .)) %>%
  map(~toString(.)) %>%
  map(~str_remove_all(., pattern = '\\D+'))

#Creates actual station id (county number + zeroes + station number)
trims$station_id <- trims$`County Number` %>%
  paste(trims$zeroes, sep = "") %>%
  paste(trims$station_number, sep = "")

tntimes$exists <- 1
trims$exists <- 1
chris$exists <- 1

#full_join makes a column for each id present in the tables joined
#the join is by id and the "exists" columns all get copied over
#exists will be equal to 1 for all the tables the id is found in, NA if the id is missing
#this line joins the first two tables 
results_full <- full_join(as.data.frame(tntimes), as.data.frame(trims), by = c("station_id")) %>%
  #this line joins the last table to the results from the previous two
  full_join(chris, by = c("station_id")) %>%
  #this renames the columns to more descriptive names. the join by default just tacks .x and .y onto columns
  rename(exists.tntimes = exists.x, exists.trims = exists.y, exists.chris = exists) %>%
  #sort by most recent update in E-TRIMS
  arrange(desc(UPDT_O2)) %>%
  distinct()

results_full$dist_tntimes_trims <- st_distance(results_full$geometry.x, results_full$geometry.y, by_element = TRUE)



#---------------------------------------------------
#makes a nice data frame with only relevant columns to write out
pretty_results <- results_full %>%
  select(c(station_id, exists.tntimes, exists.trims, exists.chris, dist_tntimes_trims, geometry.x, geometry.y, RTE_FEAT_D, UPDT_O2, `Region Nbr.x`)) %>%
  mutate(more_than_300_ft_apart = as.numeric(dist_tntimes_trims) > 300) %>%
  mutate(exists.chris = replace_na(exists.chris, 0)) %>%
  mutate(exists.tntimes = replace_na(exists.tntimes, 0)) %>%
  mutate(exists.trims = replace_na(exists.trims, 0)) %>%
  rename(geometry.tntimes = geometry.x) %>%
  rename(geometry.trims = geometry.y) %>%
  rename(route.feature.description.trims = `RTE_FEAT_D`) %>%
  rename(regions = `Region Nbr.x`)

#makes a nice-ish data frame with all columns included
literally_everything <- results_full %>%
  mutate(more_than_3000_ft_apart = as.numeric(dist_tntimes_trims) > 3000) %>%
  mutate(exists.chris = replace_na(exists.chris, 0)) %>%
  mutate(exists.tntimes = replace_na(exists.tntimes, 0)) %>%
  mutate(exists.trims = replace_na(exists.trims, 0)) %>%
  rename(geometry.tntimes = geometry.x) %>%
  rename(geometry.trims = geometry.y)


write_csv(pretty_results, "data/edl_location_validation.csv")
