# creating county-level data for union soldiers and county characteristics

# load libraries and settings
library(haven) # read_dta()
library(dplyr)
options(scipen=999) # prevent scientific notation

# import data
# andy's union army data
union <- read_dta(file = "~/Documents/Pitt/Projects/women_civil_war/data/UA_battle_and_casualty_data_2020-05-25.dta")
# county crosswalk
crosswalk <- read.csv("~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_county_crosswalk.csv")[,1:5]
# pre-aggregated ipums for 1860-1880
ipums <- read.csv("Documents/Pitt/Data/ipums/fullcnt18601880.csv")[,-c(1,8)] # -1 for row numbers, -8 for lfp
# icpsr 1860 county characteristics -- NEED TO ADD MORE YEARS THAN JUST 1860
variables <- c("state","county","name","totpop","urb860","urb25","wmtot","wftot","nbwmtot","nbwftot","fbwmtot","fbwftot","fcmtot","smtot","farmval","homemfg","mfgestab","mfgcap","mfglabm","mfglabf","level","fips","statefip")
county <- read_dta(file = "~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_02896/DS0009/02896-0009-Data.dta")[,variables]

# merge army data with county characteristics
data <- merge(x = union, y = county, by.x = c("county_icpsr", "state_icpsr"), by.y = c("county", "state"), all.x = TRUE)

# merge union data with crosswalk
data <- merge(y = data, x = crosswalk, by.y = c("county_icpsr", "state_icpsr"), by.x = c("COUNTYICP", "STATEICP"), all.y = TRUE)

# exclude non-core union states
union_states <- c("Maine", "New Hampshire", "Vermont", "New York", "Massachusetts", "Rhode Island", "Connecticuit", "Pennsylvania", "New Jersey", "Ohio", "Indiana", "Illinois", "Iowa", "Wisconsin", "Minnesota", "Michigan") # only core states: exluces CA, WA, KS, and boundary states
data <- data %>% filter(State %in% union_states & level == 1) # level == 1 for county, 2 for state, and 3 for whole country

# add some other county-characteristic groupings/definitions
data$mtot <- data$wmtot + data$fcmtot + data$smtot

# add some other soldier groupings/definitions
data$disabwound <- data$disabled + data$wounded
data_mi$whole <- data_mi$total_soldiers - data_mi$disabwound - data_mi$died
data$total_soldiers <- data$died + data$regout + data$disabled + data$wounded + data$deserted

# soldiers as pct of county male population in 1860
data$pct_pop_died <- data$died / data$mtot
data$pct_pop_disabwound <- data$disabwound / data$mtot
data$pct_pop_soldiers <- data$total_soldiers / data$mtot
data_mi$pct_pop_kmw <- data_mi$kmw / data_mi$wmtot # SOMETHING WRONG WITH KMW VARIABLE... # KMW WAY EXCEEDS POPULATION FOR SEVERAL COUNTIES
data_mi$pct_pop_mainbattle <- data_mi$mainbattle / data_mi$wmtot
data_mi$pct_pop_whole <- data_mi$whole / data_mi$wmtot
data_mi$pct_pop_disabled <- data_mi$disabled / data_mi$wmtot
data_mi$pct_pop_wounded <- data_mi$wounded / data_mi$wmtot
data_mi$pct_pop_deserted <- data_mi$deserted / data_mi$wmtot


# soldier population percentages x100 for regressions
data_mi$pct_pop_diedx100 <- data_mi$pct_pop_died*100
data_mi$pct_pop_disabwoundx100 <- data_mi$pct_pop_disabwound*100
data_mi$pct_pop_soldiersx100 <- data_mi$pct_pop_soldiers*100
data_mi$pct_pop_wholex100 <- data_mi$pct_pop_whole*100
data_mi$pct_pop_disabledx100 <- data_mi$pct_pop_disabled*100
data_mi$pct_pop_woundedx100 <- data_mi$pct_pop_wounded*100
data_mi$pct_pop_desertedx100 <- data_mi$pct_pop_deserted*100

# discretize mainbattlenum (average number of major battles fought by soldiers from that county)
data_mi$mainbattlenum_discrete <- NA
data_mi$mainbattlenum_discrete[data_mi$mainbattlenum < 1] <- "[0, 1)"
data_mi$mainbattlenum_discrete[data_mi$mainbattlenum >= 1 & data_mi$mainbattlenum <= 1.5] <- "[1, 1.5)"
data_mi$mainbattlenum_discrete[data_mi$mainbattlenum >= 1.5 & data_mi$mainbattlenum < 2] <- "[1.5, 2)"
data_mi$mainbattlenum_discrete[data_mi$mainbattlenum >= 2] <- "[2, 2.28]"



