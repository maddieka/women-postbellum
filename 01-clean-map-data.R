# Fixing Michigan Map

library(ggplot2)
library(viridis)
library(tidyr)
library(mapproj)
library(USAboundaries)
library(sf)
library(readxl)
library(stringr)

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

# additional spatial variables to include
full_data_sf$pop_per_sqmi860 <- full_data_sf$totpop / full_data_sf$area_sqmi

# SUBSET TO STATES WITH WCTU DATA FOR WCTU MERGE
wctu_states <- c("Michigan", "Indiana")
wctu_data_sf <- full_data_sf %>% filter(state_terr %in% wctu_states)
#plot(st_geometry(wctu_data_sf))

# USE THIS CHUNK IF YOU WANT TO USE COUNT_UNIONS AS THE OUTCOME VARIABLE
# wctu_data <- read_excel("~/Documents/Pitt/Projects/women_civil_war/data/wctu_county_level.xlsx")[,1:4]
# wctu_data$name <- gsub(" ", "", toupper(wctu_data$county)) # create capitalized version of county names for merge; remove spaces
# wctu_data <- wctu_data %>% pivot_wider(names_from = year, values_from = count_unions, names_prefix = "count_unions")

# USE THIS CHUNK IF YOU WANT TO USE YES/NO WCTU UNION IN A COUNTY FOR A GIVEN YEAR
wctu_data <- read_excel("~/Documents/Pitt/Projects/women_civil_war/data/wctu_county_level.xlsx")[,1:3]
wctu_data$name <- gsub(" ", "", toupper(wctu_data$county)) # create capitalized version of county names for merge; remove spaces
wctu_data <- wctu_data %>% pivot_wider(names_from = year, values_from = county, names_prefix = "county_listed")

wctu_data_sf <- merge(x = wctu_data_sf, y = wctu_data, by.x = c("state_terr", "name"), by.y = c("state", "name"), all = TRUE)
ggplot(wctu_data_sf) + geom_sf(aes(fill = state_terr)) #+ geom_sf_text(aes(label = name)) 
wctu_data_sf <- wctu_data_sf %>% filter(state_terr %in% wctu_states)

# I THINK THESE COMMENTED LINES ARE TRASH BUT SAVING JUST IN CASE...
# wctu_data_sf$count_unions1890[is.na(wctu_data_sf$count_unions1890)] <- 0 # if county is NA (meaning the State WCTU convention didn't list that county's membership info, i.c. there aren't any WCTU unions in that county) then make count_unions == 0 for those counties
# wctu_data_sf$count_unions1883[is.na(wctu_data_sf$count_unions1883)] <- 0 # if county is NA (meaning the State WCTU convention didn't list that county's membership info, i.c. there aren't any WCTU unions in that county) then make count_unions == 0 for those counties
# wctu_data_sf$has_wctu_1890 <- ifelse(test = wctu_data_sf$count_unions1890 > 0, yes = 1, no = 0)
# wctu_data_sf$has_wctu_1883 <- ifelse(test = wctu_data_sf$count_unions1883 > 0, yes = 1, no = 0)

# if the county name IS listed in the wctu data for year 18xx (county_listed18xx is NOT NA), put a 1 for "true, has wctu union"
wctu_data_sf$has_wctu_1875 <- as.numeric(!is.na(wctu_data_sf$county_listed1875)) 
wctu_data_sf$has_wctu_1883 <- as.numeric(!is.na(wctu_data_sf$county_listed1883))
wctu_data_sf$has_wctu_1890 <- as.numeric(!is.na(wctu_data_sf$county_listed1890))
wctu_data_sf$has_wctu_1896 <- as.numeric(!is.na(wctu_data_sf$county_listed1896))
wctu_data_sf$has_wctu_1898 <- as.numeric(!is.na(wctu_data_sf$county_listed1898))

#ggplot(wctu_data_sf) + geom_sf(aes(fill = as.factor(has_wctu_1898))) + scale_fill_viridis_d() + theme_void() + labs(fill = "Has local WCTU by 1898")

