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
data_mi$has_wctu_1890 <- ifelse(test = data_mi$count_unions > 0, yes = 1, no = 0)
data_mi$pct_pop_wctu <- data_mi$estimated_membership / data_mi$wftot # there's a few weird population counts... e.g. Oceana County in Michigan... est 120 wctu members, but a total of 9 white women???

ipums <- read.csv("Documents/Pitt/Data/ipums/fullcnt18601880.csv")[-c(1,7,8)] # confirmed... some of these counties had huge population growth over these decades. Not good enough to use female count in 1860 as denominator for percent women in wctu in 1880s
ipums_wide <- spread(ipums, SEX, count)
ipums_wider <- spread(ipums_wide[-5], YEAR, Female)
names(ipums_wider) <- c("STATEICP", "COUNTYICP",  "FemaleCount1860", "FemaleCount1870", "FemaleCount1880")
data_mi <- merge(x = data_mi, y = ipums_wider, by = c("STATEICP", "COUNTYICP"), all.x = TRUE)

ipums <- read.csv("Documents/Pitt/Data/ipums/fullcnt18601880.csv")[-1]
lfp <- ipums %>% filter(YEAR==1860 & SEX == "Female") %>% select(STATEICP, COUNTYICP, lfp)
names(lfp) <- c("STATEICP", "COUNTYICP", "lfpFemale1860")

data_mi <- merge(x = data_mi, y = lfp, by = c("STATEICP", "COUNTYICP"), all.x = TRUE)

data_mi$lfpFemale1860x100 <- data_mi$lfpFemale1860*100
data_mi$pct_pop_wctu <- data_mi$estimated_membership / data_mi$FemaleCount1880 # 1880 pop should be better than 1860, but i'm still 10 years off
data_mi$mainbattlenum_discrete <- NA
data_mi$mainbattlenum_discrete[data_mi$mainbattlenum < 1] <- "[0, 1)"
data_mi$mainbattlenum_discrete[data_mi$mainbattlenum >= 1 & data_mi$mainbattlenum <= 1.5] <- "[1, 1.5)"
data_mi$mainbattlenum_discrete[data_mi$mainbattlenum >= 1.5 & data_mi$mainbattlenum < 2] <- "[1.5, 2)"
data_mi$mainbattlenum_discrete[data_mi$mainbattlenum >= 2] <- "[2, 2.28]"


data_mi$pct_pop_kmw <- data_mi$kmw / data_mi$wmtot # SOMETHING WRONG WITH KMW VARIABLE... # KMW WAY EXCEEDS POPULATION FOR SEVERAL COUNTIES
data_mi$pct_soldiers_kmw <- data_mi$kmw / data_mi$total_soldiers
data_mi$whole <- data_mi$total_soldiers - data_mi$disabwound - data_mi$died
data_mi$pct_pop_mainbattle <- data_mi$mainbattle / data_mi$wmtot
data_mi$pct_soldiers_mainbattle <- data_mi$mainbattle / data_mi$total_soldiers
data_mi$pct_pop_diedx100 <- data_mi$pct_pop_died*100
data_mi$pct_pop_disabwoundx100 <- data_mi$pct_pop_disabwound*100
data_mi$pct_pop_soldiersx100 <- data_mi$pct_pop_soldiers*100
data_mi$pct_pop_whole <- data_mi$whole / data_mi$wmtot
data_mi$pct_pop_wholex100 <- data_mi$pct_pop_whole*100
data_mi$pct_pop_disabled <- data_mi$disabled / data_mi$wmtot
data_mi$pct_pop_wounded <- data_mi$wounded / data_mi$wmtot
data_mi$pct_pop_deserted <- data_mi$deserted / data_mi$wmtot

data_mi$pct_pop_disabledx100 <- data_mi$pct_pop_disabled*100
data_mi$pct_pop_woundedx100 <- data_mi$pct_pop_wounded*100
data_mi$pct_pop_desertedx100 <- data_mi$pct_pop_deserted*100

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


fit <- lm(pct_pop_wctu ~ pct_pop_soldiersx100 + pct_pop_disabwoundx100 + pct_pop_diedx100 + mainbattlenum + totpop + urb860, data_mi)
ggplot(data_mi, aes(x = pct_pop_soldiersx100, y = pct_pop_wctu)) + 
    geom_smooth(data = fortify(fit), aes(x = pct_pop_soldiersx100, y = .fitted), method = "lm") +
    geom_point(aes(color = mainbattlenum_discrete)) + scale_color_viridis(discrete = TRUE) +
    coord_cartesian(xlim =c(0,50), ylim = c(0,0.05))
ggplot(data_mi, aes(x = pct_pop_soldiersx100, y = pct_pop_wctu)) + 
    geom_smooth(method = "lm") +
    geom_point(aes(color = mainbattlenum_discrete)) + scale_color_viridis(discrete = TRUE) +
    coord_cartesian(xlim =c(0,50), ylim = c(0,0.05))
summary(fit)
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