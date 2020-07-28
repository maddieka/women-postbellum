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

# wctu_data_sf$disab_x_year <- wctu_data_sf$pct_pop_disabledx100*wctu_data_sf$year
# wctu_data_sf$disab_x_1882 <- wctu_data_sf$pct_pop_disabledx100*wctu_data_sf$year1882
# wctu_data_sf$disab_x_1890 <- wctu_data_sf$pct_pop_disabledx100*wctu_data_sf$year1890
# wctu_data_sf$disab_x_1895 <- wctu_data_sf$pct_pop_disabledx100*wctu_data_sf$year1895
# wctu_data_sf$disab_x_1896 <- wctu_data_sf$pct_pop_disabledx100*wctu_data_sf$year1896
# wctu_data_sf$disab_x_1898 <- wctu_data_sf$pct_pop_disabledx100*wctu_data_sf$year1898
# 
# wctu_data_sf$regout_x_1882 <- wctu_data_sf$pct_pop_regoutx100*wctu_data_sf$year1882
# wctu_data_sf$regout_x_1890 <- wctu_data_sf$pct_pop_regoutx100*wctu_data_sf$year1890
# wctu_data_sf$regout_x_1895 <- wctu_data_sf$pct_pop_regoutx100*wctu_data_sf$year1895
# wctu_data_sf$regout_x_1896 <- wctu_data_sf$pct_pop_regoutx100*wctu_data_sf$year1896
# wctu_data_sf$regout_x_1898 <- wctu_data_sf$pct_pop_regoutx100*wctu_data_sf$year1898
# 
# wctu_data_sf$disab_x_mbn <- wctu_data_sf$pct_pop_disabledx100*wctu_data_sf$mainbattlenum
# wctu_data_sf$regout_x_mbn <- wctu_data_sf$pct_pop_regoutx100*wctu_data_sf$mainbattlenum

# ggplot(wctu_data_sf) + geom_sf(aes(fill = mainbattlenum_discrete))
# ggplot(wctu_data_sf) + geom_sf(aes(fill = pct_pop_disabledx100))
# ggplot(wctu_data_sf) + geom_sf(aes(fill = pct_pop_soldiersx100))
# ggplot(wctu_data_sf) + geom_sf(aes(fill = estimated_membership)) + facet_wrap(~ year)

wctu_data_sf$year <- as.factor(wctu_data_sf$year)
wctu_data_sf$has_union <- ifelse(wctu_data_sf$count_unions > 0, yes = 1, no = 0)
wctu_data_sf$log_mbshp <- log(wctu_data_sf$estimated_membership + 1)
wctu_data_sf$Total_count_1860 <- wctu_data_sf$Female_count_1860 + wctu_data_sf$Male_count_1860
wctu_data_sf$log_farmval <- log(wctu_data_sf$farmval + 1)
wctu_data_sf$log_mfgcap <- log(wctu_data_sf$mfgcap + 1)
# wctu_data_sf$mbmshp_per10k <- NA
# wctu_data_sf$denom <- NA

mean(wctu_data_sf$has_union[wctu_data_sf$year == 1882], na.rm = TRUE)
ggplot(wctu_data_sf %>% filter(!is.na(year))) + geom_sf(aes(fill = as.factor(has_union))) + facet_wrap(~ year) + theme_void() + scale_fill_brewer(palette = "Paired") + labs(fill = "Has WCTU Union")

# ### MAIN TABLE - ONLY WITH LOGPOP
# has_union.1 <- lm(has_union ~ pct_pop_disabledx100 + log_totpop + year, wctu_data_sf)
# has_union.2 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + log_totpop + year, wctu_data_sf)
# has_union.3 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + log_totpop + year, wctu_data_sf)
# has_union.4 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + log_totpop + year, wctu_data_sf)
# has_union.5 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + log_totpop + year, wctu_data_sf)
# stargazer(has_union.1, has_union.2, has_union.3, has_union.4, has_union.5, 
#           df = FALSE)

### MAIN TABLE - only year FEs
has_union.1 <- lm(has_union ~ pct_pop_disabledx100 + year, wctu_data_sf)
has_union.2 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + year, wctu_data_sf)
has_union.3 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + year, wctu_data_sf)
has_union.4 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + year, wctu_data_sf)
has_union.5 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year, wctu_data_sf)
stargazer(has_union.1, has_union.2, has_union.3, has_union.4, has_union.5, 
          df = FALSE,
          title = "year FEs (outcome mean 0.586 in 1882)")

### MAIN TABLE - year FEs with interaction terms
has_union.1i <- lm(has_union ~ pct_pop_disabledx100 + year + pct_pop_disabledx100*year, wctu_data_sf)
has_union.2i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + year + pct_pop_disabledx100*year, wctu_data_sf)
has_union.3i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + year + pct_pop_disabledx100*year, wctu_data_sf)
has_union.4i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + year + pct_pop_disabledx100*year, wctu_data_sf)
has_union.5i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year, wctu_data_sf)
stargazer(has_union.1i, has_union.2i, has_union.3i, has_union.4i, has_union.5i, 
          df = FALSE,
          title = "year FEs, year x disabled interaction terms (outcome mean 0.586 in 1882)")

### MAIN TABLE - year FEs with interaction terms PLUS logtotpop
has_union.1ip <- lm(has_union ~ pct_pop_disabledx100 + year + pct_pop_disabledx100*year + log_totpop, wctu_data_sf)
has_union.2ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + year + pct_pop_disabledx100*year + log_totpop, wctu_data_sf)
has_union.3ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + year + pct_pop_disabledx100*year + log_totpop, wctu_data_sf)
has_union.4ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + year + pct_pop_disabledx100*year + log_totpop, wctu_data_sf)
has_union.5ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year + log_totpop, wctu_data_sf)
stargazer(has_union.1ip, has_union.2ip, has_union.3ip, has_union.4ip, has_union.5ip, 
          df = FALSE,
          title = "year FEs, year x disabled interaction terms (outcome mean 0.586 in 1882)",
          omit = c("year", "pct_pop_disabledx100*year"),
          omit.labels = c("Year FE", "Year x Pct Pop Disabled"))

votes <- read_dta(file = "~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_08611/DS0001/08611-0001-Data.dta")[,c(1:3,110,115)]
names(votes) <- c("STATEICP","county_name","COUNTYICP","PresVoteTurnout1860","PctVoteRepublican1860")
wctu_data_sf <- merge(x = wctu_data_sf, y = votes, by = c("STATEICP","COUNTYICP"), all.x = TRUE)


# ### WITH POLITICAL AND ECONOMIC CONTROLS
# has_union.6 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + log_totpop + year + log_farmval, wctu_data_sf)
# has_union.7 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + log_totpop + year + log_mfgcap, wctu_data_sf)
# has_union.8 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + log_totpop + year + PresVoteTurnout1860, wctu_data_sf)
# has_union.9 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + log_totpop + year + PctVoteRepublican1860, wctu_data_sf)
# # has_union.10 <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + log_totpop + year + log_farmval + log_mfgcap + PresVoteTurnout1860 + PctVoteRepublican1860, wctu_data_sf)
# stargazer(has_union.5, has_union.6, has_union.7, has_union.8, has_union.9, 
#           df = FALSE)

has_union.5.5i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year + log_totpop, wctu_data_sf)
has_union.6i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year + log_totpop + log_farmval, wctu_data_sf)
has_union.7i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year + log_totpop + log_mfgcap, wctu_data_sf)
has_union.8i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year + log_totpop + PresVoteTurnout1860, wctu_data_sf)
has_union.9i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year + log_totpop + PctVoteRepublican1860, wctu_data_sf)
has_union.10i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year + log_totpop + PctVoteRepublican1860 + PresVoteTurnout1860 + log_mfgcap + log_farmval, wctu_data_sf)
stargazer(has_union.5i, has_union.5.5i, has_union.6i, has_union.7i, has_union.8i, has_union.9i, has_union.10i,
          df = FALSE,
          title = "year FEs, year x disabled interaction terms, and political/economic controls (outcome mean 0.586 in 1882)",
          omit = c("year", "pct_pop_disabledx100*year"),
          omit.labels = c("Year FE", "Year x Pct Pop Disabled"))


### MAIN TABLE WITH INTERACTION TERMS
has_union.1i <- lm(has_union ~ pct_pop_disabledx100 + log_totpop + year + pct_pop_disabledx100*year, wctu_data_sf)
has_union.2i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + log_totpop + year + pct_pop_disabledx100*year, wctu_data_sf)
has_union.3i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + log_totpop + year + pct_pop_disabledx100*year, wctu_data_sf)
has_union.4i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + log_totpop + year + pct_pop_disabledx100*year, wctu_data_sf)
has_union.5i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + log_totpop + year + pct_pop_disabledx100*year, wctu_data_sf)
stargazer(has_union.1i, has_union.2i, has_union.3i, has_union.4i, has_union.5i, 
          df = FALSE)

### MAIN TABLE WITH INTERACTION TERMS -- county FEs instead of logpop
has_union.1i <- lm(has_union ~ pct_pop_disabledx100 + as.factor(name) + year + pct_pop_disabledx100*year, wctu_data_sf)
has_union.2i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + as.factor(name) + year + pct_pop_disabledx100*year, wctu_data_sf)
has_union.3i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + as.factor(name) + year + pct_pop_disabledx100*year, wctu_data_sf)
has_union.4i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + as.factor(name) + year + pct_pop_disabledx100*year, wctu_data_sf)
has_union.5i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + as.factor(name) + year + pct_pop_disabledx100*year, wctu_data_sf)
stargazer(has_union.1i, has_union.2i, has_union.3i, has_union.4i, has_union.5i, 
          df = FALSE)


### MAIN TABLE WITH INTERACTION TERMS -- county FEs instead of logpop
has_union.1i <- lm(has_union ~ pct_pop_disabledx100 + as.factor(name) + year + pct_pop_disabledx100*year, wctu_data_sf)
has_union.2i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + as.factor(name) + year + pct_pop_disabledx100*year, wctu_data_sf)
has_union.3i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + as.factor(name) + year + pct_pop_disabledx100*year, wctu_data_sf)
has_union.4i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + as.factor(name) + year + pct_pop_disabledx100*year, wctu_data_sf)
has_union.5i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + as.factor(name) + year + pct_pop_disabledx100*year, wctu_data_sf)
stargazer(has_union.1i, has_union.2i, has_union.3i, has_union.4i, has_union.5i, 
          df = FALSE)


# # I looked for effects between (mainbattlenum x regout x time FEs) and nothing was interesting or significant...
wctu_data_sf$mbn_x_1882 <- wctu_data_sf$mainbattlenum*wctu_data_sf$year1882
wctu_data_sf$mbn_x_1890 <- wctu_data_sf$mainbattlenum*wctu_data_sf$year1890
wctu_data_sf$mbn_x_1895 <- wctu_data_sf$mainbattlenum*wctu_data_sf$year1895
wctu_data_sf$mbn_x_1896 <- wctu_data_sf$mainbattlenum*wctu_data_sf$year1896
wctu_data_sf$mbn_x_1898 <- wctu_data_sf$mainbattlenum*wctu_data_sf$year1898
summary(lm(has_union ~ mainbattlenum + year + mbn_x_1882 + mbn_x_1890 + mbn_x_1895 + mbn_x_1896 + mbn_x_1898, wctu_data_sf))
summary(lm(has_union ~ pct_pop_regoutx100 +mainbattlenum + year + regout_x_mbn, wctu_data_sf))


