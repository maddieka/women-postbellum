# Started 06/26/2020
# Produces regression tables for SYP

# load libraries
library(ggplot2)
library(viridis)
library(tidyr)
library(gridExtra)
library(stargazer)
library(readxl)

# load data
source("~/git/women-postbellum/01-clean-merge-data.R")

# MOVE BELOW TO A DIFFERENT EXPLORATORY SCRIPT
michigan <- data %>% filter(State == "Michigan") # I only have WCTU data for Michigan so far, so exclude other states for these graphs

michigan_wctu <- read_excel("~/Documents/Pitt/Projects/women_civil_war/data/wctu_county_level.xlsx")
data_mi <- merge(x = michigan_wctu, y = michigan, by.x = c("state", "county"), by.y = c("State", "County"), all = TRUE)
data_mi$count_unions[is.na(data_mi$count_unions)] <- 0
data_mi$estimated_membership[is.na(data_mi$estimated_membership)] <- 0
data_mi$has_wctu_1890 <- ifelse(test = data_mi$count_unions > 0, yes = 1, no = 0)

# CONTINUOUS
has_wctu_1890_lm.1 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + mainbattlenum + log_totpop, data_mi)
has_wctu_1890_lm.2 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + mainbattlenum + log_totpop, data_mi)
has_wctu_1890_lm.3 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 + mainbattlenum + log_totpop, data_mi)
has_wctu_1890_lm.4 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 + pct_pop_soldiersx100 + mainbattlenum + log_totpop, data_mi)
has_wctu_1890_lm.5 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 + pct_pop_soldiersx100 + pct_pop_diedx100 + mainbattlenum + log_totpop, data_mi)
has_wctu_1890_lm.6 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 + pct_pop_soldiersx100 + pct_pop_regoutx100 + mainbattlenum + log_totpop, data_mi)
mean(data_mi$has_wctu_1890)
stargazer(has_wctu_1890_lm.1, has_wctu_1890_lm.2, has_wctu_1890_lm.3, has_wctu_1890_lm.4, has_wctu_1890_lm.5, has_wctu_1890_lm.6, df = FALSE, add.lines = list(c("Outcome Mean", rep(0.79, 6))))
