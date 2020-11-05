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

# clean union_sf names
sort(unique(union_sf$name))
union_sf$name <- gsub("(ext)", "", union_sf$name) # remove (ext) from Manitou
union_sf$name <- gsub("[^[:alnum:]]", "", toupper(union_sf$name)) # remove punctuation and spaces, and capitalize for merge
sort(unique(union_sf$name))

# prep data for merge
data$key <- toupper(data$key)

# merge with data
union_sf <- merge(x = union_sf, y = data, by.x = c("state_terr", "name"), by.y = c("State", "key"), all.x = TRUE)

ggplot(union_sf) + geom_sf(aes(fill = mainbattlenum_discrete)) #+ geom_sf_text(aes(label = name))

# additional spatial variables to include
union_sf$pop_per_sqmi860 <- union_sf$totpop / union_sf$area_sqmi


