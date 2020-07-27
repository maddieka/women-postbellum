# Started 06/26/2020
# Produces regression tables for SYP

# load libraries
library(ggplot2)
library(viridis)
library(tidyr)
library(gridExtra)
library(stargazer)
library(readxl)
library(ggfortify)
options(scipen=999) # prevent scientific notation

# load data
#source("~/git/women-postbellum/01-clean-merge-data.R")
source("~/git/women-postbellum/01-clean-map-data.R")

wctu_data_sf$year1882 <- ifelse(test = wctu_data_sf$year == 1882, yes = 1, no = 0)
wctu_data_sf$year1890 <- ifelse(test = wctu_data_sf$year == 1890, yes = 1, no = 0)
wctu_data_sf$year1895 <- ifelse(test = wctu_data_sf$year == 1895, yes = 1, no = 0)
wctu_data_sf$year1896 <- ifelse(test = wctu_data_sf$year == 1896, yes = 1, no = 0)
wctu_data_sf$year1898 <- ifelse(test = wctu_data_sf$year == 1898, yes = 1, no = 0)

wctu_data_sf$disab_x_year <- wctu_data_sf$pct_pop_disabledx100*wctu_data_sf$year
wctu_data_sf$disab_x_1882 <- wctu_data_sf$pct_pop_disabledx100*wctu_data_sf$year1882
wctu_data_sf$disab_x_1890 <- wctu_data_sf$pct_pop_disabledx100*wctu_data_sf$year1890
wctu_data_sf$disab_x_1895 <- wctu_data_sf$pct_pop_disabledx100*wctu_data_sf$year1895
wctu_data_sf$disab_x_1896 <- wctu_data_sf$pct_pop_disabledx100*wctu_data_sf$year1896
wctu_data_sf$disab_x_1898 <- wctu_data_sf$pct_pop_disabledx100*wctu_data_sf$year1898

wctu_data_sf$disab_x_mbn <- wctu_data_sf$pct_pop_disabledx100*wctu_data_sf$mainbattlenum

ggplot(wctu_data_sf) + geom_sf(aes(fill = mainbattlenum_discrete))
ggplot(wctu_data_sf) + geom_sf(aes(fill = pct_pop_disabledx100))
ggplot(wctu_data_sf) + geom_sf(aes(fill = pct_pop_soldiersx100))
ggplot(wctu_data_sf) + geom_sf(aes(fill = estimated_membership)) + facet_wrap(~ year)

wctu_data_sf$has_union <- ifelse(wctu_data_sf$count_unions > 0, yes = 1, no = 0)
wctu_data_sf$log_mbshp <- log(wctu_data_sf$estimated_membership + 1)

# wctu_data_sf$mbmshp_per10k <- NA
# wctu_data_sf$denom <- NA

# OVERLEAF TABLE 1
has_union.1 <- lm(has_union ~ pct_pop_disabledx100 + as.factor(year) + disab_x_1882 + disab_x_1890 + disab_x_1895 + disab_x_1896 + disab_x_1898, wctu_data_sf)
has_union.2 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + as.factor(year) + disab_x_1882 + disab_x_1890 + disab_x_1895 + disab_x_1896 + disab_x_1898, wctu_data_sf)
has_union.3 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + as.factor(year) + disab_x_1882 + disab_x_1890 + disab_x_1895 + disab_x_1896 + disab_x_1898, wctu_data_sf)
has_union.4 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + as.factor(year) + disab_x_1882 + disab_x_1890 + disab_x_1895 + disab_x_1896 + disab_x_1898, wctu_data_sf)
has_union.5 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + as.factor(year) + disab_x_1882 + disab_x_1890 + disab_x_1895 + disab_x_1896 + disab_x_1898, wctu_data_sf)


stargazer(has_union.1, has_union.2, has_union.3, has_union.4, has_union.5, df = FALSE)
