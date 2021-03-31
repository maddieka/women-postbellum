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
library(cowplot)

# load data
source("~/git/women-postbellum/01-clean-map-data.R")

# import Congressional District shapefile
union_congress_sf <- st_read("~/Documents/Pitt/Projects/women_civil_war/data/districtShapes/districts063.shp")
#ggplot(union_congress_sf) + geom_sf()

# change CRS projection to match union_sf
cd_1920 <- us_congressional(states = union_states, resolution = "low")
union_congress_sf_crs <- st_transform(union_congress_sf, crs = st_crs(cd_1920)) %>% st_intersection(., cd_1920)
# only keep union states
union_congress_sf_crs <- union_congress_sf_crs %>% filter(STATENAME %in% union_states)
object_size(union_congress_sf_crs) # 15.2 MB
# simplify shapefile to reduce its size (and time for maps to render)
simple_union_congress_sf <- rmapshaper::ms_simplify(input = as(union_congress_sf_crs, 'Spatial')) %>% st_as_sf()
object_size(simple_union_congress_sf) # 2.43 MB

# load congressional votes for the 19th amendment
votes <- read.csv("~/Documents/Pitt/Projects/women_civil_war/data/suffrage/congress_votes_63-3_h238.csv")
votes$district[votes$district == -1] <- NA
state_crosswalk <- read.csv("~/Documents/Pitt/Data/geography/usps_state_abrv_crosswalk.csv")

votes <- merge(x = state_crosswalk, y = votes, by.x = "Abbreviation", by.y = "state", all.x = TRUE)

vote_data_sf <- merge(x = simple_union_congress_sf, y = votes, by.x = c("STATENAME", "DISTRICT"), by.y = c("State","district"), all.x = TRUE)
vote_data_sf$vote_binary <- ifelse(test = vote_data_sf$vote == "Yea", yes = 1, no = 0)
vote_data_sf <- vote_data_sf[!is.na(vote_data_sf$vote_binary),]

ggplot() +
    geom_sf(data = vote_data_sf, aes(fill = as.factor(vote_binary)), size = .5, color = "black", linetype = "solid") +
    geom_sf(data = union_sf, fill = NA, color = "black", size = .2, linetype = "dotted") +
    labs(fill = "Vote", title = "Congressional Votes for the 19th Amendment", subtitle = "Votes by 1920 Congressional Districts (solid) and 1865 counties (dotted)") +
    scale_fill_manual(values = c("#c6dbef", "#08519c"), labels = c("Nay or abstain", "Yea")) +
    theme_void()
ggsave("~/Documents/Pitt/Projects/women_civil_war/figures/suffrage_map-counties_over_cd.png", width = 8, height = 6)

# for counties straddling two districts, assign a % vote for Yea between those districts based on county area contained in each:
a1 <- st_interpolate_aw(x = st_make_valid(vote_data_sf)["vote_binary"], to = union_sf, extensive = FALSE)
# sum(a1$vote_binary, na.rm = TRUE) / sum(simple_union_congress_sf$vote_binary, na.rm = TRUE)
summary(a1$vote_binary)
ggplot(a1) + geom_sf(aes(fill = vote_binary)) + theme_void() + scale_fill_viridis()
ggplot() +
    geom_sf(data = a1, aes(fill = vote_binary), color = "black", size = .2) +
    geom_sf(data = union_states_sf, fill = NA, color = "black", size = .5) +
    #geom_sf(data = vote_data_sf, fill = NA, color = "black", size = .3, linetype = "dotted") +
    labs(fill = "Proportion", title = "Congressional Votes for the 19th Amendment by County", subtitle = "Mapped values reflect the proportion of a county's land area contained within congressional districts whose representatives voted 'Yea' in favor of the 19th Amendment.") +
    scale_fill_distiller(type = "seq", palette = "Blues", trans = "reverse") +
    theme_void()
ggsave("~/Documents/Pitt/Projects/women_civil_war/figures/suffrage_map-votes_by_county.png", width = 8, height = 6)


# again, but JUST michigan
vote_data_sf_mi <- vote_data_sf %>% filter(STATENAME == "Michigan")
union_sf_mi <- union_sf %>% filter(state_terr == "Michigan")
a1_mi <- st_interpolate_aw(x = st_make_valid(vote_data_sf_mi)["vote_binary"], to = union_sf_mi, extensive = FALSE)
ggplot(a1_mi) + geom_sf(aes(fill = vote_binary)) + theme_void() + scale_fill_viridis()

wctu_mi_1895 <- wctu_data_sf %>% filter(year == 1895)
test_sf <- st_join(a1_mi, wctu_mi_1895, largest = TRUE)
test_sf$has_union <- ifelse(test_sf$count_unions > 0, yes = 1, no = 0)
ggplot(test_sf) + geom_sf(aes(fill = vote_binary)) + theme_void() + scale_fill_viridis()

summary(lm(vote_binary ~ has_union, test_sf))
summary(lm(vote_binary ~ pct_pop_disabledx100, test_sf))

library(AER)
cor(test_sf$has_union, test_sf$pct_pop_disabled, use = "complete.obs") # relevance assumption

lm1 <- lm(vote_binary ~ has_union + log_totpop, test_sf)
lm1$residuals
cor(lm1$residuals, test_sf$pct_pop_disabled, use = "complete.obs") # exclusion restriction

