# merging Andy's Union soldier data

# load libraries and settings
library(haven) # read_dta()
options(scipen=999) # prevent scientific notation

# import data
# andy's union army data
union <- read_dta(file = "~/Documents/Pitt/Projects/women_civil_war/data/UA_battle_and_casualty_data_2020-05-25.dta")
# county crosswalk
crosswalk <- read.csv("~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_county_crosswalk.csv")[,1:5]
# pre-aggregated ipums for 1860-1880
ipums <- read.csv("Documents/Pitt/Data/ipums/fullcnt18601880.csv")[,-c(1,8)] # -1 for row numbers, -8 for lfp
# icpsr 1860 county characteristics
variables <- c("state","county","name","totpop","urb860","urb25","wmtot","wftot","nbwmtot","nbwftot","fbwmtot","fbwftot","farmval","homemfg","mfgestab","mfgcap","mfglabm","mfglabf","level","fips","statefip")
county <- read_dta(file = "~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_02896/DS0009/02896-0009-Data.dta")[,variables]

# merge army data with county characteristics
data <- merge(x = union, y = county, by.x = c("county_icpsr", "state_icpsr"), by.y = c("county", "state"), all.x = TRUE)

# merge union data with crosswalk
data <- merge(y = data, x = crosswalk, by.y = c("county_icpsr", "state_icpsr"), by.x = c("COUNTYICP", "STATEICP"), all.y = TRUE)

# exclude non-core union states
union_states <- c("Maine", "New Hampshire", "Vermont", "New York", "Massachusetts", "Rhode Island", "Connecticuit", "Pennsylvania", "New Jersey", "Ohio", "Indiana", "Illinois", "Iowa", "Wisconsin", "Minnesota", "Michigan") # only core states: exluces CA, WA, KS, and boundary states
data <- data %>% filter(State %in% union_states & level == 1) # level == 1 for county, 2 for state, and 3 for whole country

# calculate soldier population percentages
data$disabwound <- data$disabled + data$wounded
data$total_soldiers <- data$died + data$regout + data$disabled + data$wounded + data$deserted

data$pct_pop_died <- data$died / data$wmtot
data$pct_pop_disabwound <- data$disabwound / data$wmtot
data$pct_pop_soldiers <- data$total_soldiers / data$wmtot

# MOVE BELOW TO A DIFFERENT EXPLORATORY SCRIPT
michigan <- data %>% filter(State == "Michigan")
library(readxl)
michigan_wctu <- read_excel("~/Documents/Pitt/Projects/women_civil_war/data/wctu_county_level.xlsx")
data_mi <- merge(x = michigan_wctu, y = michigan, by.x = c("state", "county"), by.y = c("State", "County"), all = TRUE)
data_mi$count_unions[is.na(data_mi$count_unions)] <- 0
data_mi$has_wctu_1890 <- ifelse(test = data_mi$count_unions > 0, yes = 1, no = 0)
data_mi$pct_pop_wctu <- data_mi$membership / data_mi$wftot

ggplot(data_mi, aes(x = mainbattle, y = count_unions, size = wmtot, color = wounded_battle)) + geom_point()
summary(lm(count_unions ~ mainbattle + wmtot + urb860 + loss, data_mi))

ggplot(data_mi, aes(x = mainbattle, y = membership, size = wmtot, color = wounded_battle)) + geom_point()
summary(lm(membership ~ mainbattle + wmtot + urb860, data_mi))

ggplot(data_mi %>% filter(pct_pop_wctu < 1), aes(x = pct_pop_soldiers, y = pct_pop_wctu)) + geom_point()




