# Fixing Michigan Map

library(ggplot2)
library(viridis)
library(tidyr)
library(mapproj)
library(USAboundaries)
library(sf)
library(readxl)
library(stringr)
library(dplyr)

# load data
source("~/git/women-postbellum/01-clean-merge-data.R")

# get shapefile from correct time period
union_sf <- us_counties(map_date = "1865-01-01", states = union_states, resolution = 'high')
union_states_sf <- us_states(map_date = "1865-01-01", states = union_states, resolution = 'high')

# clean sf names
sort(unique(union_sf$name)) 
union_sf$name <- gsub("(ext)", "", union_sf$name) # remove (ext) from Manitou
union_sf$name <- gsub("[^[:alnum:]]", "", toupper(union_sf$name)) # remove punctuation and spaces, and capitalize for merge
sort(unique(union_sf$name)) 


# clean data names
sort(unique(data$County))
data$name <- toupper(gsub("[^[:alnum:]]", "", data$name)) # capitalize for merge; remove spaces and all non-alphanumeric symbols
data$name <- gsub(pattern = "MACKINACMICHILIM", replacement = "MACKINAC", x = data$name)
data$name <- gsub(pattern = "VERNONBADAX", replacement = "VERNON", x = data$name)
# data$name <- gsub(pattern = "ALGER", replacement = "SCHOOLCRAFT", x = data$name) # Alger County was split off from Schoolcraft County in 1885
# data$name <- gsub(pattern = "BARAGA", replacement = "HOUGHTON", x = data$name) # Baraga County was split off from Houghton County in 1875
# data$name <- gsub(pattern = "ARENAC", replacement = "BAY", x = data$name) # Arenac County was split off from Bay County in 1883
# data$name <- gsub(pattern = "CHARLEVOIX", replacement = "EMMET", x = data$name) # Charlevoix County was split off from Emmet County in 1869
# data$name <- gsub(pattern = "DICKINSON", replacement = "MARQUETTE", x = data$name) # Dickinson County was split off from Marquette County in 1891
# data$name <- gsub(pattern = "GOGEBIC", replacement = "ONTONAGON", x = data$name) # Gogebic County was split off from Ontonagon County in 1887
# data$name <- gsub(pattern = "IRON", replacement = "MARQUETTE", x = data$name) # Iron County was split off from Marquette County in 1890
# data$name <- gsub(pattern = "ISLEROYALE", replacement = "MARQUETTE", x = data$name) # Isle Royale County was split off from Keweenaw County in 1875 (later reincorporated in 1897)
# data$name <- gsub(pattern = "LUCE", replacement = "CHIPPEWA", x = data$name) # Luce County was split off from Chippewa County in 1887
# data$name <- gsub(pattern = "MENOMINEE", replacement = "DELTA", x = data$name) # Menominee County was split off from Delta County in 1861 (named in 1863)
sort(unique(data$name))

# merge with data
full_data_sf <- merge(x = union_sf, y = data, by.x = c("name", "state_terr"), by.y = c("name", "State"), all.x = TRUE)
#ggplot(full_data_sf) + geom_sf(aes(fill = mainbattlenum_discrete)) #+ geom_sf_text(aes(label = name)) 

# additional spatial variables to include
full_data_sf$pop_per_sqmi860 <- full_data_sf$totpop / full_data_sf$area_sqmi

# SUBSET TO STATES WITH WCTU DATA FOR WCTU MERGE
wctu_states <- c("Michigan")
wctu_data_sf <- full_data_sf %>% filter(state_terr %in% wctu_states)
#plot(st_geometry(wctu_data_sf))

# USE THIS CHUNK IF YOU WANT TO USE COUNT_UNIONS AS THE OUTCOME VARIABLE
# wctu_data <- read_excel("~/Documents/Pitt/Projects/women_civil_war/data/wctu_county_level.xlsx")[,1:4]
# wctu_data$name <- gsub(" ", "", toupper(wctu_data$county)) # create capitalized version of county names for merge; remove spaces
# wctu_data <- wctu_data %>% pivot_wider(names_from = year, values_from = count_unions, names_prefix = "count_unions")

# USE THIS CHUNK IF YOU WANT TO USE YES/NO WCTU UNION IN A COUNTY FOR A GIVEN YEAR
#wctu_data <- read_excel("~/Documents/Pitt/Projects/women_civil_war/data/wctu_county_level.xlsx")[,1:3]
wctu_data <- read_excel("~/Documents/Pitt/Projects/women_civil_war/data/michigan_wctu_attempt2.xlsx")
wctu_data$name <- gsub("[^[:alnum:]]", "", toupper(wctu_data$county)) # create capitalized version of county names for merge; remove spaces and all non-alphanumeric symbols
sort(unique(wctu_data$name))
wctu_data[is.na(wctu_data)] <- 0 # assume if the county is not listed in a particular year's report, the # unions and members == 0
wctu_data$name <- gsub(pattern = "MACKINACMICHILIM", replacement = "MACKINAC", x = wctu_data$name)
wctu_data$name <- gsub(pattern = "ALGER", replacement = "SCHOOLCRAFT", x = wctu_data$name) # Alger County was split off from Schoolcraft County in 1885
wctu_data$name <- gsub(pattern = "BARAGA", replacement = "HOUGHTON", x = wctu_data$name) # Baraga County was split off from Houghton County in 1875
wctu_data$name <- gsub(pattern = "ARENAC", replacement = "BAY", x = wctu_data$name) # Arenac County was split off from Bay County in 1883
wctu_data$name <- gsub(pattern = "CHARLEVOIX", replacement = "EMMET", x = wctu_data$name) # Charlevoix County was split off from Emmet County in 1869
wctu_data$name <- gsub(pattern = "DICKINSON", replacement = "MARQUETTE", x = wctu_data$name) # Dickinson County was split off from Marquette County in 1891
wctu_data$name <- gsub(pattern = "GOGEBIC", replacement = "ONTONAGON", x = wctu_data$name) # Gogebic County was split off from Ontonagon County in 1887
wctu_data$name <- gsub(pattern = "IRON", replacement = "MARQUETTE", x = wctu_data$name) # Iron County was split off from Marquette County in 1890
wctu_data$name <- gsub(pattern = "ISLEROYALE", replacement = "MARQUETTE", x = wctu_data$name) # Isle Royale County was split off from Keweenaw County in 1875 (later reincorporated in 1897)
wctu_data$name <- gsub(pattern = "LUCE", replacement = "CHIPPEWA", x = wctu_data$name) # Luce County was split off from Chippewa County in 1887
wctu_data$name <- gsub(pattern = "MENOMINEE", replacement = "DELTA", x = wctu_data$name) # Menominee County was split off from Delta County in 1861 (named in 1863)

# Uncomment line if you get the following error: Error in fix.by(by.y, y) : 'by' must specify uniquely valid columns
# detach(package:plyr)

test <- wctu_data %>% 
    group_by(year, state, name) %>% 
    summarise(count_unions = sum(count_unions, na.rm = TRUE),
              total_dues = sum(total_dues, na.rm = TRUE),
              estimated_membership = sum(estimated_membership, na.rm = TRUE)) %>%
    ungroup()

wctu_data_sf <- merge(x = wctu_data_sf, y = test, by.x = c("state_terr", "name"), by.y = c("state", "name"), all = TRUE)

# mi_sf <- us_counties(map_date = "2000-01-01", states = "Michigan", resolution = 'high')
# 
# ggplot() + 
#     geom_sf(data = mi_sf, color = "pink") +
#     geom_sf_text(data = mi_sf, aes(label = name), size = 2, color = "pink") +
#     geom_sf(data = union_sf %>% filter(state_name == "Michigan"), fill = NA) + 
#     geom_sf_text(data = union_sf %>% filter(state_name == "Michigan"), aes(label = name), size = 3)
#     
# wctu_data_sf$has_wctu_1882 <- as.numeric(!is.na(wctu_data_sf$county_listed1882))
# wctu_data_sf$has_wctu_1890 <- as.numeric(!is.na(wctu_data_sf$county_listed1890))
# wctu_data_sf$has_wctu_1895 <- as.numeric(!is.na(wctu_data_sf$county_listed1895))
# wctu_data_sf$has_wctu_1896 <- as.numeric(!is.na(wctu_data_sf$county_listed1896))
# wctu_data_sf$has_wctu_1898 <- as.numeric(!is.na(wctu_data_sf$county_listed1898))

