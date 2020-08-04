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

wctu_data_sf$year <- as.factor(wctu_data_sf$year)
wctu_data_sf$has_union <- ifelse(wctu_data_sf$count_unions > 0, yes = 1, no = 0)
wctu_data_sf$log_mbshp <- log(wctu_data_sf$estimated_membership + 1)
wctu_data_sf$Total_count_1860 <- wctu_data_sf$Female_count_1860 + wctu_data_sf$Male_count_1860
wctu_data_sf$log_farmval <- log(wctu_data_sf$farmval + 1)
wctu_data_sf$log_mfgcap <- log(wctu_data_sf$mfgcap + 1)
# wctu_data_sf$mbmshp_per10k <- NA
# wctu_data_sf$denom <- NA

mean(wctu_data_sf$has_union[wctu_data_sf$year == 1882], na.rm = TRUE)
ggplot(wctu_data_sf %>% filter(!is.na(year))) + geom_sf(aes(fill = as.factor(has_union)), size = .2, color = "black") + facet_wrap(~ year) + theme_void() + scale_fill_brewer(palette = "Paired") + labs(fill = "Has WCTU Union")

# civil war vars + year FEs
lm1.1 <- lm(has_union ~ pct_pop_disabledx100 + year, wctu_data_sf)
lm1.2 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + year, wctu_data_sf)
lm1.3 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + year, wctu_data_sf)
lm1.4 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + year, wctu_data_sf)
lm1.5 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year, wctu_data_sf)
stargazer(lm1.1, lm1.2, lm1.3, lm1.4, lm1.5,
          df = FALSE,
          title = "First table",
          label = "table:uavars_yearFE",
          covariate.labels = c("Percent disabled", "Percent exited regularly","Percent wounded","Percent died","Percent fought"),
          dep.var.caption = c("County has at least one local WCTU"),
          dep.var.labels = "Outcome mean in 1882, 0.59",
          omit = c("year"),
          omit.labels = c("Year FE")
)

# civil war vars + year and county FEs
lm2.1 <- lm(has_union ~ pct_pop_disabledx100 + year + name, wctu_data_sf)
lm2.2 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + year + name, wctu_data_sf)
lm2.3 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + year + name, wctu_data_sf)
lm2.4 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + year + name, wctu_data_sf)
lm2.5 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + name, wctu_data_sf)
stargazer(lm2.1, lm2.2, lm2.3, lm2.4, lm2.5,
          df = FALSE,
          title = "I prefer the main table to this one because my dependent variable is binary and tends to only change once, so there's not much within-county variation in this data from which to gain information from County FEs.",
          label = "table:uavars_yearFE_countyFE",
          covariate.labels = c("Percent disabled", "Percent exited regularly","Percent wounded","Percent died","Percent fought"),
          dep.var.caption = c("County has at least one local WCTU"),
          dep.var.labels = "Outcome mean in 1882, 0.59",
          omit = c("year", "name"),
          omit.labels = c("Year FE", "County FE")
)

# main table plus INTERACTION effects; report year FE values
lm1.1i <- lm(has_union ~ pct_pop_disabledx100 + year + pct_pop_disabledx100*year, wctu_data_sf)
lm1.2i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + year + pct_pop_disabledx100*year, wctu_data_sf)
lm1.3i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + year + pct_pop_disabledx100*year, wctu_data_sf)
lm1.4i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + year + pct_pop_disabledx100*year, wctu_data_sf)
lm1.5i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year, wctu_data_sf)
stargazer(lm1.1i, lm1.2i, lm1.3i, lm1.4i, lm1.5i,
          df = FALSE,
          title = "First table plus interaction effects",
          label = "table:uavars_yearFE_interaction",
          covariate.labels = c("Percent disabled", "1890","1895","1896","1898", "Percent disabled X 1890", "Percent disabled X 1895", "Percent disabled X 1896", "Percent disabled X 1898"),
          dep.var.caption = c("County has at least one local WCTU"),
          dep.var.labels = "Outcome mean in 1882, 0.59",
          omit = c("pct_pop_regoutx100", "pct_pop_woundedx100", "pct_pop_diedx100", "pct_pop_soldiersx100"),
          add.lines = list(c("Union Army percentages", rep("Yes", 5)))
)

# political/economic controls table; include year FE, interaction terms; stagger log_totpop, economic stuff, and political stuff

votes <- read_dta(file = "~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_08611/DS0001/08611-0001-Data.dta")[,c(1:3,110,115)]
names(votes) <- c("STATEICP","county_name","COUNTYICP","PctVoteRepublican1860","PresVoteTurnout1860")
votes[votes == max(votes$PctVoteRepublican1860)] <- NA # 999.9 is code for NA
wctu_data_sf <- merge(x = wctu_data_sf, y = votes, by = c("STATEICP","COUNTYICP"), all.x = TRUE)

lm1.5ip <- lm1.5i # repeat last column of previous table
lm1.6ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 +
                       year + pct_pop_disabledx100*year + log_totpop, wctu_data_sf)
lm1.7ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 +
                     year + pct_pop_disabledx100*year + log_totpop + log_farmval, wctu_data_sf)
lm1.8ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 +
                     year + pct_pop_disabledx100*year + log_totpop + log_mfgcap, wctu_data_sf)
lm1.9ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 +
                     year + pct_pop_disabledx100*year + log_totpop + PresVoteTurnout1860, wctu_data_sf)
lm1.10ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 +
                     year + pct_pop_disabledx100*year + log_totpop + PctVoteRepublican1860, wctu_data_sf)
lm1.11ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 +
                      year + pct_pop_disabledx100*year + log_totpop + PctVoteRepublican1860 + PresVoteTurnout1860 + log_mfgcap + log_farmval, wctu_data_sf)
stargazer(lm1.5ip, lm1.6ip, lm1.7ip, lm1.8ip, lm1.9ip, lm1.10ip, #lm1.11ip,
          df = FALSE,
          title = "Other controls",
          label = "table:uavars_yearFE_interaction",
          covariate.labels = c("Percent disabled", "log(Total population, 1860)", "log(Cash value of farms, 1860)", "log(Manufacturing capital, 1860)","Voter turnout, 1860 presidential election", "Percent Republican votes, 1860 presidential election"),
          dep.var.caption = c("County has at least one local WCTU"),
          dep.var.labels = "Outcome mean in 1882, 0.59",
          omit = c("pct_pop_regoutx100", "pct_pop_woundedx100", "pct_pop_diedx100", "pct_pop_soldiersx100", "year", "pct_pop_disabledx100*year"),
          add.lines = list(c("Union Army percentages", rep("Yes", 6)),
                           c("Year FE", rep("Yes",6)),
                           c("Year x Percent disabled", rep("Yes",6)))
)

### HAZARD MODEL -- need to put this in a loop for more than 5 years...
# THIS JUST CORRECTS MY DATA...####
wctu_correct_sf <- wctu_data_sf

hazard_sf_1882 <- wctu_correct_sf %>% filter(year == 1882) # all of 1882
counties_1882 <- hazard_sf_1882$name[hazard_sf_1882$has_union == 1] # these counties should be 1 for all future years, too
table(wctu_correct_sf$has_union, wctu_correct_sf$year)
wctu_correct_sf$has_union[wctu_correct_sf$name %in% counties_1882] <- 1
table(wctu_correct_sf$has_union, wctu_correct_sf$year)

hazard_sf_1890 <- wctu_correct_sf %>% filter(year == 1890) # all of 1890
counties_1890 <- hazard_sf_1890$name[hazard_sf_1890$has_union == 1] # these counties should be 1 for all future years, too
wctu_correct_sf$has_union[wctu_correct_sf$year %in% c(1895, 1896, 1898) & wctu_correct_sf$name %in% counties_1890] <- 1
table(wctu_correct_sf$has_union, wctu_correct_sf$year)

hazard_sf_1895 <- wctu_correct_sf %>% filter(year == 1895) # all of 1895
counties_1895 <- hazard_sf_1895$name[hazard_sf_1895$has_union == 1] # these counties should be 1 for all future years, too
wctu_correct_sf$has_union[wctu_correct_sf$year %in% c(1896, 1898) & wctu_correct_sf$name %in% counties_1895] <- 1
table(wctu_correct_sf$has_union, wctu_correct_sf$year)

hazard_sf_1896 <- wctu_correct_sf %>% filter(year == 1896) # all of 1896
counties_1896 <- hazard_sf_1896$name[hazard_sf_1896$has_union == 1] # these counties should be 1 for all future years, too
wctu_correct_sf$has_union[wctu_correct_sf$year == 1898 & wctu_correct_sf$name %in% counties_1896] <- 1
table(wctu_correct_sf$has_union, wctu_correct_sf$year)

ggplot(wctu_correct_sf %>% filter(!is.na(year))) + geom_sf(aes(fill = as.factor(has_union)), size = .2, color = "black") + facet_wrap(~ year) + theme_void() + scale_fill_brewer(palette = "Paired") + labs(fill = "Has WCTU Union")

# HAZARD MODEL DATA... ####
hazard_sf_1882 <- wctu_data_sf %>% filter(year == 1882) # all of 1882
counties_1882 <- hazard_sf_1882$name[hazard_sf_1882$has_union == 0] # counties that haven't adopted a union yet as of 1882

# only bring counties forward that DIDN'T already have a union in 1882
hazard_sf_1890 <- wctu_data_sf %>% filter(year == 1890 & name %in% counties_1882)
hazard_sf_1895 <- wctu_data_sf %>% filter(year == 1895 & name %in% counties_1882)
hazard_sf_1896 <- wctu_data_sf %>% filter(year == 1896 & name %in% counties_1882)
hazard_sf_1898 <- wctu_data_sf %>% filter(year == 1898 & name %in% counties_1882)

# do the same for 1890
counties_1890 <- hazard_sf_1890$name[hazard_sf_1890$has_union == 0]  # counties that haven't adopted a union yet as of 1890
hazard_sf_1895 <- hazard_sf_1895 %>% filter(year == 1895 & name %in% counties_1890)
hazard_sf_1896 <- hazard_sf_1896 %>% filter(year == 1896 & name %in% counties_1890)
hazard_sf_1898 <- hazard_sf_1898 %>% filter(year == 1898 & name %in% counties_1890)

# do the same for 1895
counties_1895 <- hazard_sf_1895$name[hazard_sf_1895$has_union == 0]  # counties that haven't adopted a union yet as of 1895
hazard_sf_1896 <- hazard_sf_1896 %>% filter(year == 1896 & name %in% counties_1895)
hazard_sf_1898 <- hazard_sf_1898 %>% filter(year == 1898 & name %in% counties_1895)

# do the same for 1896
counties_1896 <- hazard_sf_1896$name[hazard_sf_1896$has_union == 0]  # counties that haven't adopted a union yet as of 1896
hazard_sf_1898 <- hazard_sf_1898 %>% filter(year == 1898 & name %in% counties_1896)

hazard_sf <- rbind(hazard_sf_1882, hazard_sf_1890, hazard_sf_1895, hazard_sf_1896, hazard_sf_1898)

ggplot(hazard_sf %>% filter(!is.na(year))) + geom_sf(aes(fill = as.factor(has_union)), size = .2, color = "black") + facet_wrap(~ year) + theme_void() + scale_fill_brewer(palette = "Paired") + labs(fill = "Has WCTU Union")

hazard_sf <- hazard_sf %>% filter(year %in% c(1882, 1890, 1895)) # no changes in 96 and 98
hazard_sf$year <- factor(hazard_sf$year, levels = c(1882, 1890, 1895))


# NEIGHBORS...####

# robustness! (include column 2 from economic/poltical controls table as baseline comparison; add (i) column 2 run without years 1896-1898, (i) "corrected" data, (ii) hazard model, and (iii) control for neighboring county)
lm1.6ir <- lm1.6ip
lm1.7ir <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 +
                year + pct_pop_disabledx100*year + log_totpop, wctu_data_sf %>% filter(year %in% c(1882,1890,1895)))
lm1.8ir <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 +
                year + pct_pop_disabledx100*year + log_totpop, wctu_correct_sf %>% filter(year %in% c(1882,1890,1895)))
lm1.9ir <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 +
                year + pct_pop_disabledx100*year + log_totpop, hazard_sf %>% filter(year %in% c(1882,1890,1895)))
stargazer(lm1.6ir, lm1.7ir, lm1.8ir, lm1.9ir,
          df = FALSE,
          title = "Other controls",
          label = "table:uavars_yearFE_interaction",
          #column.labels = c("\\multirow{2}{All years}{1882-1898}","\\multirow{2}{Restricted years}{1882-1895}", "\\multirow{2}{Corrected data}{1882-1895}", "\\multirow{2}{Hazard model}{1882-1895}"),
          column.labels = c("1882-1898", "1882-1895"), column.separate = c(1,3),
          covariate.labels = c("Percent disabled", "log(Total population, 1860)"),
          dep.var.caption = c("County has at least one local WCTU"),
          dep.var.labels = "Outcome mean in 1882, 0.59",
          omit = c("pct_pop_regoutx100", "pct_pop_woundedx100", "pct_pop_diedx100", "pct_pop_soldiersx100", "year", "pct_pop_disabledx100*year"),
          add.lines = list(c("Union Army percentages", rep("Yes", 4)),
                           c("Year FE", rep("Yes",4)),
                           c("Year x Percent disabled", rep("Yes",4)))
)

