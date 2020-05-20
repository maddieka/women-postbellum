library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(vroom)
library(ggmap)
library(maps)
library(mapdata)
library(stargazer)
library(tidyr)
library(sf)

options(scipen=999)

#wctu <- read.csv("~/Documents/Pitt/Projects/women_civil_war/state_membership1895.csv")
#shp <- st_read("~/Downloads/US_AtlasHCB_Counties_Gen0001/US_HistCounties_Gen0001_Shapefile/US_HistCounties_Gen0001.shp")
#ggplot() + geom_sf(data = shp, color = "black", fill = NA) + coord_sf()

prohibition <- read.table("~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_08343/DS0001/08343-0001-Data.tsv", header = TRUE, sep = "\t")
crosswalk <- read.csv("~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_county_crosswalk.csv")[,1:5]
crosswalk$ICPSRCNTY <- substr(toupper(crosswalk$County), start = 1, stop = 4)

prohibition <- merge(x = crosswalk, y = prohibition, by.x = c("ICPSRCNTY", "STATEICP"), by.y = c("ICPSRCNTY", "ICPSRSTATE"), all.y = TRUE)

ipums <- read.csv("Documents/Pitt/Data/ipums/fullcnt18601880.csv")[-1]
ipums <- ipums %>% select(-lfp) # %>% filter(YEAR == 1880)
ipums_long <- gather(ipums, variable, value, count:workers)
ipums_long$variableSEX <- paste0(ipums_long$variable, ipums_long$SEX)
ipums_long <- ipums_long %>% select(-SEX, -variable)
ipums_wide <- spread(ipums_long, variableSEX, value)

union <- read.csv("Documents/Pitt/Data/union/UnionarmydataMaddie.csv")
union$nondead <- union$whole + union$disabwound
union_states <- c("ME", "NH", "VT", "NY", "MA", "RI", "CT", "PA", "NJ", "OH", "IN", "IL", "IA", "WI", "MN", "MI") # only core states: exluces CA, WA, KS, and boundary states

data_long <- merge(x = union, y = ipums_wide, by.x = c("county_icpsr","state_icpsr"), by.y = c("COUNTYICP","STATEICP"))
data_long <- data_long %>% filter(state_ab %in% union_states)

data_long1860 <- data_long %>% filter(YEAR == 1860)
data_long1860$pct_pop_soldiers <- data_long1860$total / data_long1860$countMale
data_long1860$pct_pop_died <- data_long1860$died / data_long1860$countMale
data_long1860$pct_pop_disabwound <- data_long1860$disabwound / data_long1860$countMale
data_long1860$pct_pop_nondead <- data_long1860$nondead / data_long1860$countMale

data_long1860 <- data_long1860 %>% select(county_icpsr, state_icpsr, pct_pop_soldiers, pct_pop_died, pct_pop_disabwound, pct_pop_nondead)

data_long <- merge(data_long, data_long1860, by = c("county_icpsr", "state_icpsr"), all.x = TRUE)
# data_state <-
#     data_long %>%
#     #select(state_ab, state_icpsr, died, disabwound, total, nondead, whole) %>%
#     group_by(state_ab) %>%
#     summarise(died = sum(died, na.rm = TRUE),
#               disabwound = sum(disabwound, na.rm = TRUE),
#               total = sum(total, na.rm = TRUE),
#               nondead = sum(nondead, na.rm = TRUE),
#               whole = sum(whole, na.rm = TRUE),
#               countFemale = sum(countFemale, na.rm = TRUE),
#               workersFemale = sum(workersFemale, na.rm = TRUE),
#               countMale = sum(countMale, na.rm = TRUE),
#               workersMale = sum(workersMale, na.rm = TRUE)
#     ) %>%
#     ungroup()

# data <- merge(x = data_state, y = wctu, by = "state_ab", all = TRUE)
data <- merge(x = data_long, y = prohibition, by.x = c("county_icpsr", "state_icpsr"), by.y = c("COUNTYICP", "STATEICP"), all.x = TRUE)

data <- data %>% filter(state_ab %in% union_states)


    
data$lfpFemale <- data$workersFemale/data$countFemale
#data$membership_as_percent_of_female_pop <- data$active_membership1895 / data$countFemale
data$total_soldiers_over_male_pop <- data$total / data$countMale
data$pct_soldierx100 <- data$total_soldiers_over_male_pop*100
data$totalpopulation <- data$countFemale + data$countMale
data$YEAR <- as.factor(data$YEAR)

data$state_ab <- as.character(data$state_ab)

# ggplot(data %>% filter(state_ab %in% union_states), aes(x = total, y = active_membership1895)) + 
#     geom_point() + 
#     geom_text(aes(label = state_ab), nudge_x = 10000) +
#     geom_smooth(method='lm') +
#     labs(x = "Total soldiers", y = "Active WCTU Membership 1885", title = "")
# 
# 
# ggplot(data %>% filter(state_ab %in% union_states), aes(x = total_soldiers_over_male_pop, y = membership_as_percent_of_female_pop)) + 
#     geom_point() + 
#     geom_text(aes(label = state_ab), nudge_x = .006) +
#     geom_smooth() +
#     labs(x = "Total soldiers / Total Male Pop", y = "Active WCTU Members 1885 / Female Pop", title = "")
# 
# ggplot(data %>% filter(state_ab %in% union_states), aes(y = active_membership1895, x = workersFemale)) + 
#     geom_point() + 
#     geom_text(aes(label = state_ab), nudge_x = 20000) +
#     geom_smooth(method='lm') +
#     labs(y = "Active WCTU Membership 1885", x = "Women Working in 1880", title = "")
# 
# ggplot(data %>% filter(state_ab %in% union_states), aes(y = membership_as_percent_of_female_pop, x = lfpFemale)) + 
#     geom_point() + 
#     geom_text(aes(label = state_ab), nudge_x = .006) +
#     geom_smooth(method='lm') +
#     labs(y = "Active WCTU Members 1885 / Female Pop", x = "Women's LFP in 1880", title = "")
# 
# 
# lm <- lm(active_membership1895 ~ total, data)
# summary(lm)
prohibition_long <- gather(data = prohibition, key = "proh_year", value = "proh_policy", PROH1801:PROH1920)
prohibition_long$proh_year <- as.numeric(gsub(pattern = "PROH", replacement = "", x = prohibition_long$proh_year))
prohibition_long$dry <- ifelse(test = prohibition_long$proh_policy > 0, yes = 1, no = 0)

test <- prohibition_long %>% filter(proh_policy > 0) %>% group_by(State, County) %>% slice(which.min(proh_year)) %>% ungroup()

data2 <- merge(x = data_long, y = test, by.x = c("county_icpsr", "state_icpsr"), by.y = c("COUNTYICP", "STATEICP"), all.x = TRUE)

data2$lfpFemale <- data2$workersFemale/data2$countFemale
#data$membership_as_percent_of_female_pop <- data$active_membership1895 / data$countFemale
data2$pct_pop_soldierx100 <- data2$pct_pop_soldiers*100
data2$pct_pop_diedx100 <- data2$pct_pop_died*100
data2$pct_pop_disabwoundx100 <- data2$pct_pop_disabwound*100
data2$pct_pop_nondeadx100 <- data2$pct_pop_nondead*100
data2$countTotal <- data2$countFemale + data2$countMale
data2$YEAR <- as.factor(data2$YEAR)
data2$state_ab <- as.character(data2$state_ab)
data2$lfpFemalex100 <- data2$lfpFemale*100

cor.test(x = data2$pct_pop_died, y = data2$proh_year, method = "pearson")
cor.test(x = data2$pct_pop_died[data2$proh_year > 1865], y = data2$proh_year[data2$proh_year > 1865], method = "pearson")

data2$proh_by_1910 <- ifelse(test = data2$proh_year < 1911, yes = 1, no = 0)
summary(lm(proh_by_1910 ~ pct_pop_nondeadx100, data2))
summary(lm(proh_by_1910 ~ pct_pop_disabwoundx100 + pct_pop_diedx100, data2))

# %1 increase in men dead after Civil War --> county becomes dry 1.2 years earlier... BUT it doesn't look like hardly any counties change their dry status until the 1900s...
ggplot(data2 %>% filter(pct_pop_soldierx100 < 100 & proh_year > 1865), aes(x = pct_pop_soldierx100, y = proh_year, size = countTotal)) + geom_point() + geom_smooth(method = "lm")
summary(lm(proh_year ~ pct_pop_soldierx100, data2))
summary(lm(proh_year ~ pct_pop_soldierx100 + pct_pop_diedx100 + pct_pop_disabwoundx100, data2))
summary(lm(proh_year ~ pct_pop_soldierx100 + pct_pop_diedx100 + pct_pop_disabwoundx100 + countTotal, data2 %>% filter(proh_year > 1865)))

data2$proh_by_1865 <- ifelse(test = data2$proh_year < 1866, yes = 1, no = 0)
data2$proh_by_1900 <- ifelse(test = data2$proh_year < 1901, yes = 1, no = 0)
data2$proh_by_1915 <- ifelse(test = data2$proh_year < 1916, yes = 1, no = 0)
data2$proh_post_war <- ifelse(test = data2$proh_year > 1865, yes = 1, no = 0)

lm65 <- lm(proh_by_1865 ~ pct_pop_soldierx100  + lfpFemalex100, data2)
lm65. <- lm(proh_by_1865 ~ pct_pop_disabwoundx100  + lfpFemalex100, data2)
lm00 <- lm(proh_by_1900 ~ pct_pop_soldierx100  + lfpFemalex100, data2 %>% filter(proh_year > 1865))
lm00. <- lm(proh_by_1900 ~ pct_pop_disabwoundx100  + lfpFemalex100, data2 %>% filter(proh_year > 1865))
lm10 <- lm(proh_by_1910 ~ pct_pop_soldierx100  + lfpFemalex100, data2 %>% filter(proh_year > 1865))
lm10. <- lm(proh_by_1910 ~ pct_pop_disabwoundx100  + lfpFemalex100, data2 %>% filter(proh_year > 1865))
lm15 <- lm(proh_by_1915 ~ pct_pop_soldierx100  + lfpFemalex100, data2 %>% filter(proh_year > 1865))
lm15. <- lm(proh_by_1915 ~ pct_pop_disabwoundx100  + lfpFemalex100, data2 %>% filter(proh_year > 1865))
stargazer(lm65, lm00, lm10, lm65., lm00., lm10.)

# data2$pct_pop_soldierx100Xproh_post_war <- data2$pct_pop_soldierx100*data2$proh_post_war
# data2$lfpFemalex100Xproh_post_war <- data2$lfpFemalex100*data2$proh_post_war
# 
# lm65.1 <- lm(proh_by_1865 ~ pct_pop_soldierx100  + pct_pop_soldierx100Xproh_post_war + lfpFemalex100 + lfpFemalex100Xproh_post_war + proh_post_war, data2)
# lm00.1 <- lm(proh_by_1900 ~ pct_pop_soldierx100  + pct_pop_soldierx100Xproh_post_war + lfpFemalex100 + lfpFemalex100Xproh_post_war + proh_post_war, data2)
# lm15.1 <- lm(proh_by_1915 ~ pct_pop_soldierx100  + pct_pop_soldierx100Xproh_post_war + lfpFemalex100 + lfpFemalex100Xproh_post_war + proh_post_war, data2)
# stargazer(lm65.1, lm00.1, lm15.1)


ggplot(data2 %>% filter(pct_pop_soldiers < 100), aes(x = pct_pop_soldiers, y = lfpFemale, size = countTotal)) + geom_point(alpha = .1) + geom_smooth(method = "lm") + facet_wrap(~YEAR)
summary(lm(lfpFemale ~ countTotal + YEAR + pct_pop_diedx100 + pct_pop_disabwoundx100, data2))

ggplot(data2 %>% filter(state_ab %in% union_states & pct_pop_soldierx100 < 100), aes(x = pct_pop_soldierx100, y = lfpFemalex100, alpha = countTotal)) + 
    geom_point() + geom_smooth(method = "lm") + facet_wrap(~YEAR) + geom_text(aes(label = name), nudge_x = 1)

ggplot(data2, aes(x = proh_year)) + geom_density() + annotate("rect", xmin = 1861, xmax = 1866, ymin = 0, ymax = .05, alpha = .2)
ggplot(data2) + 
    geom_density(aes(x = pct_pop_died), color = "blue") + 
    geom_density(aes(x = pct_pop_soldiers), color = "black") +# pct_pop_soldiers is pretty much normally distributed
    geom_density(aes(x = pct_pop_disabwound), color = "red") + 
    geom_density(aes(x = pct_pop_nondead), color = "orange")

michigan <- read.csv("~/Documents/Pitt/Projects/women_civil_war/data/local_unions_wctu.csv")[,1:5]
test <- merge(x = michigan, y = crosswalk, by.x = c("STATEICP", "county"), by.y = c("STATEICP", "County"), all.x = TRUE)
michigan <- merge(x = test, y = data2, by.x = c("STATEICP", "COUNTYICP"), by.y = c("state_icpsr", "county_icpsr"), all = TRUE)
michigan$had_local_union <- ifelse(test = is.na(michigan$chapter), yes = 0, no = 1)
cnt_unions <- michigan %>% group_by(STATEICP, COUNTYICP) %>% summarise(cnt_unions = n_distinct(chapter, na.rm = TRUE))
michigan <- merge(x = michigan, y = cnt_unions, by = c("STATEICP", "COUNTYICP"))
michigan <- michigan %>% filter(STATEICP %in% c(23,21) & YEAR == 1870) %>% select(-chapter) %>% distinct()

summary(lm(had_local_union ~ pct_pop_disabwoundx100, michigan))
summary(lm(cnt_unions ~ pct_pop_disabwoundx100, michigan))

michigan$had_2local_union <- ifelse(test = michigan$cnt_unions > 1, yes = 1, no = 0)
michigan$had_3local_union <- ifelse(test = michigan$cnt_unions > 2, yes = 1, no = 0)
michigan$had_4local_union <- ifelse(test = michigan$cnt_unions > 3, yes = 1, no = 0)

lm1 <- lm(had_local_union ~ pct_pop_disabwoundx100, michigan)
lm2 <- lm(had_2local_union ~ pct_pop_disabwoundx100, michigan)
lm3 <- lm(had_3local_union ~ pct_pop_disabwoundx100, michigan)
lm4 <- lm(had_4local_union ~ pct_pop_disabwoundx100, michigan)
stargazer(lm1, lm2, lm3, lm4)

lm1.1 <- lm1
lm1.2 <- lm(had_local_union ~ pct_pop_disabwoundx100 + lfpFemalex100, michigan)
lm1.3 <- lm(had_local_union ~ pct_pop_disabwoundx100 + lfpFemalex100 + countFemale + countMale, michigan)
lm1.4 <- lm(had_local_union ~ pct_pop_diedx100 + lfpFemalex100 + countFemale + countMale, michigan)
lm1.5 <- lm(had_local_union ~ pct_pop_soldierx100 + lfpFemalex100 + countFemale + countMale, michigan)
lm1.6 <- lm(had_local_union ~ pct_pop_nondeadx100 + lfpFemalex100 + countFemale + countMale, michigan)

stargazer(lm1.1, lm1.2, lm1.3, lm1.4, lm1.5, lm1.6)

lm2.1 <- lm1
lm2.2 <- lm(had_local_union ~ pct_pop_disabwoundx100 + lfpFemalex100, michigan)
lm2.3 <- lm(had_local_union ~ pct_pop_disabwoundx100 + lfpFemalex100 + countFemale + countMale, michigan)
lm2.4 <- lm(had_local_union ~ pct_pop_disabwoundx100 + pct_pop_diedx100 + lfpFemalex100 + countFemale + countMale, michigan)
lm2.5 <- lm(had_local_union ~ pct_pop_disabwoundx100 + pct_pop_diedx100 + pct_pop_soldierx100 + lfpFemalex100 + countFemale + countMale, michigan)
lm2.6 <- lm(had_local_union ~ pct_pop_disabwoundx100 + pct_pop_diedx100 + pct_pop_nondeadx100 + lfpFemalex100 + countFemale + countMale, michigan)

stargazer(lm2.1, lm2.2, lm2.3, lm2.4, lm2.5, lm2.6)
