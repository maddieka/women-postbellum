# Fixing Michigan Map

library(ggplot2)
library(viridis)
library(tidyr)
library(mapproj)
library(USAboundaries)
library(sf)
library(readxl)

# load data
source("~/git/women-postbellum/01-clean-merge-data.R")

# get shapefile from correct time period
union_sf <- us_counties(map_date = "1865-01-01", states = union_states, resolution = 'high')
union_states_sf <- us_states(map_date = "1865-01-01", states = union_states, resolution = 'high')

# clean sf names
unique(union_sf$name) 
union_sf$name <- gsub("[[:punct:]]", "", toupper(union_sf$name)) # need to remove punctuation and capitalize
union_sf$name <- gsub(" EXT", "", union_sf$name) # remove EXT from Manitou
union_sf$name <- gsub(" ", "", union_sf$name)


# clean data names
unique(data$County)
data$name <- toupper(gsub(pattern = "Mackinac/Michilim", replacement = "Mackinac", x = data$County))
data$name <- gsub(pattern = "VERNON/BAD AX", replacement = "VERNON", x = data$name)
data$name <- gsub(" ", "", data$name)

# merge with data
full_data_sf <- merge(x = union_sf, y = data, by.x = c("name", "state_terr"), by.y = c("name", "State"), all.x = TRUE)
#ggplot(full_data_sf) + geom_sf(aes(fill = mainbattlenum_discrete)) #+ geom_sf_text(aes(label = name)) 

# SUBSET TO MICHIGAN FOR WCTU MERGE
mi_data_sf <- full_data_sf %>% filter(state_terr == "Michigan")
#plot(st_geometry(mi_data_sf))


michigan_wctu <- read_excel("~/Documents/Pitt/Projects/women_civil_war/data/wctu_county_level.xlsx")
michigan_wctu$name <- gsub(" ", "", toupper(michigan_wctu$county)) # create capitalized version of county names for merge; remove spaces

mi_data_sf <- merge(x = mi_data_sf, y = michigan_wctu, by.x = c("state_terr", "name"), by.y = c("state", "name"), all = TRUE)

mi_data_sf$count_unions[is.na(mi_data_sf$county)] <- 0 # if county is NA (meaning the State WCTU convention didn't list that county's membership info, i.c. there aren't any WCTU unions in that county) then make count_unions == 0 for those counties
mi_data_sf$estimated_membership[is.na(mi_data_sf$county)] <- 0 # same as above
mi_data_sf$quantile <- ntile(x = mi_data_sf$count_unions, n = 4)
mi_data_sf$has_wctu_1890 <- ifelse(test = mi_data_sf$count_unions > 0, yes = 1, no = 0)
#data_mi$pct_pop_wctu <- data_mi$estimated_membership / data_mi$Female_count_1880


ggplot(mi_data_sf) + geom_sf(aes(fill = as.factor(has_wctu_1890))) + scale_fill_viridis_d() + theme_void() + labs(fill = "Has local WCTU by 1890")
