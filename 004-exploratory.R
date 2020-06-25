library(ggplot2)
library(viridis)
library(tidyr)
library(gridExtra)
library(stargazer)
library(readxl)
library(mapproj)
library(USAboundaries)
library(sf)

# load data
source("~/git/women-postbellum/01-clean-merge-data.R")

# get shapefile from correct time period
union_sf <- us_counties(map_date = "1865-01-01", states = union_states, resolution = 'high')
plot(st_geometry(union_sf))
ggplot(union_sf) + geom_sf()

union_states_sf <- us_states(map_date = "1865-01-01", states = union_states, resolution = 'high')

# merge with data
union_sf$name <- toupper(union_sf$name)
test <- merge(x = union_sf, y = data, by.x = c("name", "state_terr"), by.y = c("name", "State"), all.x = TRUE)

ggplot() + 
    geom_sf(data = test, aes(fill = mainbattlenum_discrete), color = "black", size = .2) + scale_fill_viridis(discrete = TRUE) + 
    geom_sf(data = union_states_sf, fill = NA, color = "black") +
    theme_void() 
ggsave("~/Documents/Pitt/Projects/women_civil_war/meeting_notes/map_mainbattlenum_discrete.png", width = 10, height = 6)
# "It would be perfect if we already got a clear and suggestive picture from the raw data when looking at the time series evolutions of female LFP / political activism from before to after the Civil War splitting counties/states by above/below median veteran share – as a first pass test for the hypothesis." - Andy
summary(data$pct_pop_soldiers)
data$above_median <- ifelse(data$pct_pop_soldiers >= 0.13497, yes = "Above median", no = "Below median")
data$quantile_soldiers <- ntile(data$pct_pop_soldiers, 4)

summary(data$pct_pop_disabled)
data$above_median_disabled <- ifelse(data$pct_pop_disabled >= 0.011926, yes = "Above median", no = "Below median")
data$quantile_disabled <- ntile(data$pct_pop_disabled, 6)

data_long <- gather(data, year, Female_lfp, `1860_Female_lfp`, `1870_Female_lfp`, `1880_Female_lfp`, factor_key = TRUE)
data_long$year <- gsub(pattern = "_Female_lfp", replacement = "", x = data_long$year)
# female lfp by median soldier pop
ggplot(data = data_long %>% filter(!is.na(above_median)), aes(x = year, y = Female_lfp, fill = above_median)) + 
    geom_boxplot() + 
    labs(x = "Census Year", y = "Female LFP", fill = "Counties' % soldier population") + 
    scale_fill_brewer(palette="Accent") 
ggsave("~/Documents/Pitt/Projects/women_civil_war/meeting_notes/boxplot_lfp_soldier_median.png", width = 8, height = 5)

ggplot(data = data_long %>% filter(!is.na(above_median)), aes(x = year, y = Female_lfp, fill = as.factor(quantile_soldiers))) + 
    geom_boxplot() + 
    labs(x = "Census Year", y = "Female LFP", fill = "Quantile % soldier population") + 
    scale_fill_brewer(palette="Accent") 
ggsave("~/Documents/Pitt/Projects/women_civil_war/meeting_notes/boxplot_lfp_soldier_quantile.png", width = 8, height = 5)

# female lfp by % disabled
ggplot(data = data_long %>% filter(!is.na(above_median_disabled)), aes(x = year, y = Female_lfp, fill = above_median_disabled)) + 
    geom_boxplot() + 
    labs(x = "Census Year", y = "Female LFP", fill = "Counties' % soldier disabled") + 
    scale_fill_brewer(palette="Accent") 
ggsave("~/Documents/Pitt/Projects/women_civil_war/meeting_notes/boxplot_lfp_disabled_median.png", width = 8, height = 5)

ggplot(data = data_long %>% filter(!is.na(above_median)), aes(x = year, y = Female_lfp, fill = as.factor(quantile_disabled))) + 
    geom_boxplot() + 
    labs(x = "Census Year", y = "Female LFP", fill = "Quantile % disabled population") + 
    scale_fill_brewer(palette="Accent") 
ggsave("~/Documents/Pitt/Projects/women_civil_war/meeting_notes/boxplot_lfp_disabled_quantile6.png", width = 8, height = 5)


# female lfp by mainbattlenum
ggplot(data = data_long %>% filter(!is.na(mainbattlenum_discrete)), aes(x = year, y = Female_lfp, fill = mainbattlenum_discrete)) + 
    geom_boxplot() + 
    labs(x = "Census Year", y = "Female LFP", fill = "Mean # major battles", subtitle = "379, 360, 30, and 4 observations, respectively") 


ggplot(data_long %>% filter(kmw < 10000), aes(x = kmw, y = Female_lfp, color = year)) + geom_point() + scale_color_viridis_d()
ggplot(data_long, aes(x = mainbattlenum, y = Female_lfp, color = year)) + geom_point() + scale_color_viridis_d()

# looking for connection bw WCTU and suffrage
wctu_state <- read.csv("~/Documents/Pitt/Projects/women_civil_war/data/nationaldues_1889.csv")
suffrage <- read.csv("~/Documents/Pitt/Projects/women_civil_war/data/suffrage_timeline.csv")
union_state <- data %>% group_by(State) %>% summarise(died = sum(died, na.rm = TRUE),
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
test$year[is.na(test$year)] <- 1920
test$suffrage_pre_feds <- ifelse(test$year < 1920, yes = 1, no = 0)

summary(lm(suffrage_pre_feds ~ membership, test))
