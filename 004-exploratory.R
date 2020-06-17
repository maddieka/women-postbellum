library(ggplot2)
library(viridis)
library(tidyr)
library(gridExtra)
library(stargazer)
library(readxl)

library(mapproj)
# load data
source("~/git/women-postbellum/01-clean-merge-data.R")

# map
counties <- map_data("county")
mapdata = unique(data)
mapdata$region <- tolower(mapdata$State)
mapdata$subregion <- tolower(mapdata$County)
counties <- inner_join(counties, mapdata, by = c("region","subregion"))

states <- map_data("state")
states <- states %>% filter(region %in% tolower(union_states))

summary(counties$pct_pop_kmw)
counties$kmw_discrete <- ifelse(counties$kmw > counties$wmtot, yes = "kmw > wmtot", no = "kmw < wmtot")

ggplot() + 
    geom_polygon(data = counties, aes(x = long, y = lat, group = group, fill = kmw_discrete), color = "grey", size = .2) +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
    scale_fill_viridis(discrete = TRUE, option = "cividis") + 
    coord_map()
ggsave("~/Desktop/kmw.png")

ggplot(data, aes(x = pct_pop_kmw)) + geom_histogram()

ggplot() + 
    geom_polygon(data = counties, aes(x = long, y = lat, group = group, fill = mainbattlenum), color = "grey", size = .2) +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
    scale_fill_viridis(option = "cividis") + 
    coord_map()
ggsave("~/Desktop/mainbattlenum.png")

# looking for connection bw WCTU and suffrage
wctu_state <- read.csv("~/Documents/Pitt/Projects/women_civil_war/data/nationaldues_1889.csv")
suffrage <- read.csv("~/Documents/Pitt/Projects/women_civil_war/data/suffrage_timeline.csv")
union_state <- mapdata %>% group_by(State) %>% summarise(died = sum(died, na.rm = TRUE),
                                                         disabled = sum(disabled, na.rm = TRUE),
                                                         wounded = sum(wounded, na.rm = TRUE),
                                                         deserted = sum(deserted, na.rm = TRUE),
                                                         wmtot = sum(wmtot, na.rm = TRUE))
# soldiers as pct of county male population in 1860
union_state$pct_pop_died <- union_state$died / union_state$wmtot
union_state$pct_pop_disabled <- union_state$disabled / union_state$wmtot
union_state$pct_pop_wounded <- union_state$wounded / union_state$wmtot
union_state$pct_pop_deserted <- union_state$deserted / union_state$wmtot
# soldier population percentages x100 for regressions
union_state$pct_pop_diedx100 <- union_state$pct_pop_died*100
union_state$pct_pop_disabledx100 <- union_state$pct_pop_disabled*100
union_state$pct_pop_woundedx100 <- union_state$pct_pop_wounded*100
union_state$pct_pop_desertedx100 <- union_state$pct_pop_deserted*100

crosswalk_states <- unique(crosswalk[,1:2])
test <- merge(x = crosswalk_states, y = suffrage, by.x = "State", by.y = "state", all.x = TRUE)
test <- merge(x = test, y = wctu_state, by.x = "State", by.y = "state", all.x = TRUE)
test <- merge(x = test, y = union_state, by = "State", all.x = TRUE)

test$suffrage_type[is.na(test$suffrage_type)] <- "full"
test$year[is.na(test$year)] <- 2020
test$suffrage_pre_feds <- ifelse(test$year < 2020, yes = 1, no = 0)

summary(lm(suffrage_pre_feds ~ pct_pop_disabledx100 + pct_pop_woundedx100 + , test %>% filter(suffrage_type == "full")))
