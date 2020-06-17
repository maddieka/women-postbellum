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

ipums <- read.csv("Documents/Pitt/Data/ipums/fullcnt18601880.csv")[-c(1,7,8)] # confirmed... some of these counties had huge population growth over these decades. Not good enough to use female count in 1860 as denominator for percent women in wctu in 1880s
ipums_wide <- spread(ipums, SEX, count)
ipums_wider <- spread(ipums_wide[-5], YEAR, Female)
names(ipums_wider) <- c("STATEICP", "COUNTYICP",  "FemaleCount1860", "FemaleCount1870", "FemaleCount1880")
data_mi <- merge(x = data_mi, y = ipums_wider, by = c("STATEICP", "COUNTYICP"), all.x = TRUE)

ipums <- read.csv("Documents/Pitt/Data/ipums/fullcnt18601880.csv")[-1]
lfp <- ipums %>% filter(YEAR==1860 & SEX == "Female") %>% select(STATEICP, COUNTYICP, lfp)
names(lfp) <- c("STATEICP", "COUNTYICP", "lfpFemale1860")

data_mi <- merge(x = data_mi, y = lfp, by = c("STATEICP", "COUNTYICP"), all.x = TRUE)

cnt_unions_plot1 <- ggplot(data_mi, aes(x = total_soldiers, y = count_unions, size = totpop, color = mainbattlenum_discrete)) + geom_point() + scale_color_viridis(discrete=TRUE) #+ geom_abline(intercept = 0, slope = 1)
cnt_unions_lm1 <- lm(count_unions ~ disabwound + totpop + urb860 + mainbattlenum_discrete + lfpFemale1860x100, data_mi)
cnt_unions_lm2 <- lm(count_unions ~ disabwound + died + totpop + urb860 + mainbattlenum_discrete + lfpFemale1860x100, data_mi)
cnt_unions_lm3 <- lm(count_unions ~ disabwound + died + whole + totpop + urb860 + mainbattlenum_discrete + lfpFemale1860x100, data_mi)
summary(cnt_unions_lm1)

membership_plot1 <- ggplot(data_mi, aes(x = total_soldiers, y = estimated_membership, size = totpop, color = mainbattlenum_discrete)) + geom_point() + scale_color_viridis(discrete=TRUE) + geom_abline(intercept = 0, slope = 1) #+ geom_smooth(method = "lm", se = FALSE)
membership_lm1 <- lm(estimated_membership ~ disabwound + totpop + urb860 + mainbattlenum_discrete + lfpFemale1860x100, data_mi)
membership_lm2 <- lm(estimated_membership ~ disabwound + died + totpop + urb860 + mainbattlenum_discrete + lfpFemale1860x100, data_mi)
membership_lm3 <- lm(estimated_membership ~ disabwound + died + whole + totpop + urb860 + mainbattlenum_discrete + lfpFemale1860x100, data_mi)
summary(membership_lm1)

plot1 <- grid.arrange(cnt_unions_plot1, membership_plot1, ncol = 2)
ggsave("~/Documents/Pitt/Projects/women_civil_war/meeting_notes/plot1.png", plot1)
stargazer(cnt_unions_lm1, cnt_unions_lm2, cnt_unions_lm3, membership_lm1, membership_lm2, membership_lm3, df = FALSE)


fit <- lm(pct_pop_wctu ~ pct_pop_soldiersx100 + mainbattlenum , data_mi)
summary(fit)
plot4 <- 
ggplot(data_mi, aes(x = pct_pop_soldiersx100, y = pct_pop_wctu)) + 
    geom_smooth(data = fortify(fit), aes(x = pct_pop_soldiersx100, y = .fitted), method = "lm") +
    geom_point(aes(color = mainbattlenum_discrete)) + scale_color_viridis(discrete = TRUE) +
    coord_cartesian(xlim =c(0,50), ylim = c(0,0.05))
ggsave("~/Documents/Pitt/Projects/women_civil_war/meeting_notes/wctu_on_soldiers.png", plot4, width = 6, height = 4)
ggplot(data_mi, aes(x = pct_pop_soldiersx100, y = pct_pop_wctu)) + 
    geom_smooth(method = "lm") +
    geom_point(aes(color = mainbattlenum_discrete)) + scale_color_viridis(discrete = TRUE) +
    coord_cartesian(xlim =c(0,50), ylim = c(0,0.05))
#+labs(x = "% male pop in Union Army", y = "% female pop in WCTU", size = "% male pop in major", color = "Mean # major battles fought")

has_wctu_1890_lm1 <- lm(has_wctu_1890 ~ pct_pop_disabwoundx100 + totpop + urb860 + mainbattlenum_discrete + lfpFemale1860x100, data_mi)
has_wctu_1890_lm2 <- lm(has_wctu_1890 ~ pct_pop_disabwoundx100 + pct_pop_diedx100 + totpop + urb860 + mainbattlenum_discrete + lfpFemale1860x100, data_mi)
has_wctu_1890_lm3 <- lm(has_wctu_1890 ~ pct_pop_disabwoundx100 + pct_pop_diedx100 + pct_pop_wholex100 + totpop + urb860 + mainbattlenum_discrete + lfpFemale1860x100, data_mi)
summary(cnt_unions_lm1)

pct_pop_wctu_lm1 <- lm(pct_pop_wctu ~ pct_pop_disabwoundx100 + totpop + urb860 + mainbattlenum_discrete + lfpFemale1860x100, data_mi)
pct_pop_wctu_lm2 <- lm(pct_pop_wctu ~ pct_pop_disabwoundx100 + pct_pop_diedx100 + totpop + urb860 + mainbattlenum_discrete + lfpFemale1860x100, data_mi)
pct_pop_wctu_lm3 <- lm(pct_pop_wctu ~ pct_pop_disabwoundx100 + pct_pop_diedx100 + pct_pop_wholex100 + totpop + urb860 + mainbattlenum_discrete + lfpFemale1860x100, data_mi)

stargazer(has_wctu_1890_lm1, has_wctu_1890_lm2, has_wctu_1890_lm3, pct_pop_wctu_lm1, pct_pop_wctu_lm2, pct_pop_wctu_lm3, df = FALSE)

has_wctu_1890_lm.1 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + totpop, data_mi)
has_wctu_1890_lm.2 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + totpop, data_mi)
has_wctu_1890_lm.3 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 + totpop, data_mi)
has_wctu_1890_lm.4 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 + pct_pop_soldiersx100 + totpop, data_mi)
has_wctu_1890_lm.5 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 + pct_pop_soldiersx100 + pct_pop_diedx100 + totpop, data_mi)
mean(data_mi$has_wctu_1890)
stargazer(has_wctu_1890_lm.1, has_wctu_1890_lm.2, has_wctu_1890_lm.3, has_wctu_1890_lm.4, has_wctu_1890_lm.5, df = FALSE, add.lines = list(c("Outcome Mean", rep(0.83, 5))))


# POLICY OUTCOME CORRELATIONS
prop_laws <- read.csv("Documents/Pitt/Data/property_laws/dates_property_laws.csv")
prohibition <- read.table("~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_08343/DS0001/08343-0001-Data.tsv", header = TRUE, sep = "\t")
suffrage <- read.csv("~/Documents/Pitt/Projects/women_civil_war/data/suffrage_timeline.csv")

policies <- merge(x = suffrage, y = prop_laws, by.x = "state", by.y = "State", all = TRUE)

union_states <- c("Maine", "New Hampshire", "Vermont", "New York", "Massachusetts", "Rhode Island", "Connecticuit", "Pennsylvania", "New Jersey", "Ohio", "Indiana", "Illinois", "Iowa", "Wisconsin", "Minnesota", "Michigan") # only core states: exluces CA, WA, KS, and boundary states
policies$union_state <- ifelse(test = policies$state %in% union_states, yes = 1, no = 0)

prohibition_long <- gather(data = prohibition, key = "proh_year", value = "proh_policy", PROH1801:PROH1920)
prohibition_long$proh_year <- as.numeric(gsub(pattern = "PROH", replacement = "", x = prohibition_long$proh_year))
prohibition_long$dry <- ifelse(test = prohibition_long$proh_policy > 0, yes = 1, no = 0)
test <- prohibition_long %>% filter(proh_policy > 0) %>% group_by(ICPSRSTATE, ICPSRCNTY) %>% slice(which.min(proh_year)) %>% ungroup()
proh_data <- test %>% group_by(ICPSRSTATE) %>% summarise(median_proh_year = median(proh_year, na.rm = TRUE),
                                                         min_proh_year = min(proh_year, na.rm = TRUE),
                                                         mean_proh_year = mean(proh_year, na.rm = TRUE))
crosswalk <- read.csv("~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_county_crosswalk.csv")[,1:3]

test <- merge(x = crosswalk, y = policies, by.x = "State", by.y = "state", all.y = TRUE)
testx <- merge(x = test, y = proh_data, by.x = "STATEICP", by.y = "ICPSRSTATE", all = TRUE)

policies <- unique(testx)

wctu_state <- read.csv("~/Documents/Pitt/Projects/women_civil_war/data/nationaldues_1889.csv")
policies <- merge(x = policies, y = wctu_state, by.x = "State", by.y = "state", all.x = TRUE)

variables <- c("state","totpop","urb25","wmtot","wftot","level")
state <- read_dta(file = "~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_02896 1890/DS0018/02896-0018-Data.dta")[,variables]
state <- state %>% filter(level == 2)

policies <- merge(x = policies, y = state, by.x = "STATEICP", by.y = "state", all.x = TRUE)
policies$pct_pop_wctu <- policies$membership / policies$wftot

plot3 <- ggplot(policies %>% filter(State %in% union_states), aes(x = membership, y = year, label = Abbrv, color = suffrage_type))+ scale_y_continuous(breaks = seq(1840,1920, 20)) + geom_point() + geom_text(hjust = 0) + geom_smooth(method = "lm") 
ggplot(policies %>% filter(State %in% union_states), aes(x = pct_pop_wctu, y = Property_HanesWolcott2013, label = Abbrv)) + geom_point() + geom_text(hjust = 0)  + geom_smooth(method = "lm") 
ggplot(policies %>% filter(State %in% union_states), aes(x = pct_pop_wctu, y = Earnings_HanesWolcott2013, label = Abbrv)) + geom_point() + geom_text(hjust = 0)  + geom_smooth(method = "lm") 
plot2 <- ggplot(policies %>% filter(State %in% union_states), aes(x = pct_pop_wctu, y = median_proh_year, label = Abbrv), size = 2) + geom_point() + geom_text(hjust = -0.5) 
ggsave("~/Documents/Pitt/Projects/women_civil_war/meeting_notes/prohibition_on_wctu.png", plot2, width = 6, height = 4)
both <- grid.arrange(plot2, plot3)
ggsave("~/Documents/Pitt/Projects/women_civil_war/meeting_notes/policies.png", both, width = 8, height = 6)
ggplot(policies %>% filter(State %in% union_states), aes(x = pct_pop_wctu, y = mean_proh_year, label = Abbrv)) + geom_point() + geom_text(hjust = 0) 

ggplot(policies %>% filter(suffrage_type == "school"), aes(x = Property_HanesWolcott2013, y = year, label = Abbrv)) + 
    geom_point() + 
    geom_text(hjust = 0, nudge_x = 1) +
    coord_cartesian(xlim =c(1840,1920), ylim = c(1840,1920)) +
    geom_smooth(method="lm") +
    labs(x = "Year married women's property law passed", y = "Year women's suffrage for school elections passed")

# all other policy combos are flat.
# policies$year[is.na(policies$year)] <- 1919
# policies$suffrage_before_fed <- ifelse(policies$year < 1919, yes = 1, no = 0)
# union_state <- mapdata %>% group_by(State) %>% summarise(died = sum(died, na.rm = TRUE),
#                                                          disabled = sum(disabled, na.rm = TRUE),
#                                                          wounded = sum(wounded, na.rm = TRUE),
#                                                          deserted = sum(deserted, na.rm = TRUE),
#                                                          wmtot = sum(wmtot, na.rm = TRUE))
# # soldiers as pct of county male population in 1860
# union_state$pct_pop_died <- union_state$died / union_state$wmtot
# union_state$pct_pop_disabled <- union_state$disabled / union_state$wmtot
# union_state$pct_pop_wounded <- union_state$wounded / union_state$wmtot
# union_state$pct_pop_deserted <- union_state$deserted / union_state$wmtot
# # soldier population percentages x100 for regressions
# union_state$pct_pop_diedx100 <- union_state$pct_pop_died*100
# union_state$pct_pop_disabledx100 <- union_state$pct_pop_disabled*100
# union_state$pct_pop_woundedx100 <- union_state$pct_pop_wounded*100
# union_state$pct_pop_desertedx100 <- union_state$pct_pop_deserted*100
# 
# state_data <- merge(x = union_state, y = policies, by = "State", all = TRUE)
# 
# summary(lm(suffrage_before_fed ~ pct_pop_disabledx100, state_data %>% filter(suffrage_type == "full")))

# map
states <- map_data("county")

mapdata_mi = unique(data_mi)
mapdata_mi$region <- tolower(mapdata_mi$state)
mapdata_mi$subregion <- tolower(mapdata_mi$county)
counties_mi <- inner_join(states, mapdata_mi, by = c("region","subregion"))

mapdata = unique(data)
mapdata$region <- tolower(mapdata$state)
counties <- inner_join(states, mapdata, by = "region")


ggplot() + 
    geom_polygon(data = counties_mi, aes(x = long, y = lat, group = group, fill = pct_pop_wctu), color = "black") +
    scale_fill_viridis() + 
    theme_void()

ggplot() + 
    geom_polygon(data = counties, aes(x = long, y = lat, group = group, fill = pct_pop_soldiers), color = "black") +
    scale_fill_viridis() + 
    theme_void()

