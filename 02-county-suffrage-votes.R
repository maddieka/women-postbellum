library(ggplot2)
library(viridis)
library(tidyr)
library(gridExtra)
library(stargazer)
library(readxl)
library(mapproj)
library(USAboundaries)
library(sf)
library(pryr)
library(rgeos)

# load data
source("~/git/women-postbellum/01-clean-map-data.R")

# import Congressional District shapefile
union_congress_sf <- st_read("~/Documents/Pitt/Projects/women_civil_war/data/districtShapes/districts063.shp")
#ggplot(union_congress_sf) + geom_sf()

# change CRS projection to match full_data_sf
cd_1920 <- us_congressional(states = union_states, resolution = "low")
union_congress_sf_crs <- st_transform(union_congress_sf, crs = st_crs(cd_1920)) %>% st_intersection(., cd_1920)
# only keep union states
union_congress_sf_crs <- union_congress_sf_crs %>% filter(STATENAME %in% union_states)
object_size(union_congress_sf_crs) # 15.2 MB
# simplify shapefile to reduce its size (and time for maps to render)
simple_union_congress_sf <- rmapshaper::ms_simplify(input = as(union_congress_sf_crs, 'Spatial')) %>% st_as_sf()
object_size(simple_union_congress_sf) # 2.43 MB

ggplot(simple_union_congress_sf) + geom_sf() + theme_void()

votes <- read.csv("~/Documents/Pitt/Projects/women_civil_war/data/suffrage/congress_votes_63-3_h238.csv")
votes$district[votes$district == -1] <- NA
state_crosswalk <- read.csv("~/Documents/Pitt/Data/geography/usps_state_abrv_crosswalk.csv")

votes <- merge(x = state_crosswalk, y = votes, by.x = "Abbreviation", by.y = "state", all.x = TRUE)

vote_data_sf <- merge(x = simple_union_congress_sf, y = votes, by.x = c("STATENAME", "DISTRICT"), by.y = c("State","district"), all.x = TRUE)
vote_data_sf$vote_binary <- ifelse(test = vote_data_sf$vote == "Yea", yes = 1, no = 0)
vote_data_sf <- vote_data_sf[!is.na(vote_data_sf$vote_binary),]

plot1 <- ggplot(vote_data_sf %>% filter(STATENAME %in% wctu_states)) + geom_sf(aes(fill = vote), size = .2) + theme_void() + labs(fill = "Vote for 19th Amendment")
plot2 <- ggplot(wctu_data_sf) + geom_sf(aes(fill = as.factor(has_wctu_1898)), size = .2) + scale_fill_viridis_d() + theme_void() + labs(fill = "Has WCTU, 1898")
grid.arrange(plot1, plot2, nrow = 1)

ggplot() + 
    geom_sf(data = vote_data_sf, aes(fill = as.factor(vote_binary)), size = .5, color = "black", linetype = "solid") + 
    geom_sf(data = full_data_sf, fill = NA, color = "black", size = .3, linetype = "dotted") +
    labs(fill = "Vote", title = "Congressional Votes for the 19th Amendment", subtitle = "Votes by 1920 Congressional Districts (solid) and 1865 counties (dotted)") +
    scale_fill_brewer(palette = "Paired") +
    theme_void()
ggsave("~/Documents/Pitt/Projects/women_civil_war/meeting_notes/suffrage_map-counties_over_cd.png", width = 10, height = 8)


a1 <- st_interpolate_aw(x = st_make_valid(vote_data_sf)["vote_binary"], to = full_data_sf, extensive = FALSE)
sum(a1$vote_binary, na.rm = TRUE) / sum(simple_union_congress_sf$vote_binary, na.rm = TRUE)
summary(a1$vote_binary)
ggplot(a1) + geom_sf(aes(fill = vote_binary)) + theme_void()
ggplot() + 
    geom_sf(data = a1, aes(fill = vote_binary), color = "black", size = .2) +
    labs(fill = "Vote", title = "Congressional Votes for the 19th Amendment by County", subtitle = '1 if "Yea", 0 if "Nay" or not voting; Weighted average by proportion of split county in each congressional district') +
    #scale_fill_brewer(palette = "Paired") +
    theme_void()
ggsave("~/Documents/Pitt/Projects/women_civil_war/meeting_notes/suffrage_map-votes_by_county.png", width = 10, height = 8)
