# creating county-level data for union soldiers and county characteristics

# load libraries and settings
library(haven) # read_dta()
library(dplyr)
options(scipen=999) # prevent scientific notation

# import data
# andy's union army data
union <- read_dta(file = "~/Documents/Pitt/Projects/women_civil_war/data/andy_ua_data/UA_battle_and_casualty_data_2020-06-15.dta")
# county crosswalk
crosswalk <- read.csv("~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_county_crosswalk.csv")[,1:5]
# icpsr 1860 county characteristics -- NEED TO ADD MORE YEARS THAN JUST 1860
variables <- c("state","county","name","totpop","urb860","urb25","wmtot","wftot","farmval","homemfg","mfgestab","mfgcap","mfglabm","mfglabf","level","fips","statefip",
               "equipval", "homemfg","farm39","farm1019" ,"farm2049", "farm5099", "farm100","farm500","farm1000", "mfgestab" ,"mfgcap","mfgout","realest","churches", "water","rail","fbwtot","mfglabf","quaker","quakacc","germref","germracc","shaker","shakacc")
icpsr1860 <- read_dta(file = "~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_02896/DS0009/02896-0009-Data.dta")[,variables]
icpsr1860 <- icpsr1860[icpsr1860$level == 1,] # level == 1 for county, 2 for state, and 3 for whole country
# ipums - already processed
source("~/git/women-postbellum/01-clean-ipums-data.R")

# merge army data with icpsr county characteristics
data <- merge(x = union, y = icpsr1860, by.x = c("county_icpsr", "state_icpsr"), by.y = c("county", "state"), all.x = TRUE)

# merge union data with crosswalk
data <- merge(y = data, x = crosswalk, by.y = c("county_icpsr", "state_icpsr"), by.x = c("COUNTYICP", "STATEICP"), all.y = TRUE)

# merge ipums data so that county-level LFPs are included
data <- merge(x = data, y = ipums_wider, by = c("COUNTYICP", "STATEICP"), all.y = TRUE)

# exclude non-core union states
union_states <- c("Maine", "New Hampshire", "Vermont", "New York", "Massachusetts", "Rhode Island", "Connecticut", "Pennsylvania", "New Jersey", "Ohio", "Indiana", "Illinois", "Iowa", "Wisconsin", "Minnesota", "Michigan") # only core states: exluces CA, WA, KS, and boundary states
data <- data %>% filter(State %in% union_states)

# add some other county-characteristic groupings/definitions
data$log_totpop <- log(data$totpop + 1)
data$pct_urb860 <- data$urb860 / data$totpop
data$pct_urb860x100 <- (data$urb860 / data$totpop)*100
data$pct_urb25 <- data$urb25 / data$totpop
data$pct_urb25x100 <- (data$urb25 / data$totpop)*100

# add some other soldier groupings/definitions
data$disabwound <- data$disabled + data$wounded
data$total_soldiers <- data$died + data$regout + data$disabled + data$wounded + data$deserted
data$whole <- data$total_soldiers - data$disabwound - data$died

# soldiers as pct of county male population in 1860
data$pct_pop_died <- data$died / data$wmtot
data$pct_pop_disabwound <- data$disabwound / data$wmtot
data$pct_pop_soldiers <- data$total_soldiers / data$wmtot
data$pct_pop_mainbattle <- data$mainbattle / data$wmtot
data$pct_pop_whole <- data$whole / data$wmtot
data$pct_pop_disabled <- data$disabled / data$wmtot
data$pct_pop_wounded <- data$wounded / data$wmtot
data$pct_pop_deserted <- data$deserted / data$wmtot
data$pct_pop_regout <- data$regout / data$wmtot

# soldier population percentages x100 for regressions
data$pct_pop_diedx100 <- data$pct_pop_died*100
data$pct_pop_disabwoundx100 <- data$pct_pop_disabwound*100
data$pct_pop_soldiersx100 <- data$pct_pop_soldiers*100
data$pct_pop_wholex100 <- data$pct_pop_whole*100
data$pct_pop_disabledx100 <- data$pct_pop_disabled*100
data$pct_pop_woundedx100 <- data$pct_pop_wounded*100
data$pct_pop_desertedx100 <- data$pct_pop_deserted*100
data$pct_pop_regoutx100 <- data$pct_pop_regout*100

# discretize mainbattlenum (average number of major battles fought by soldiers from that county)
#plot(density(data$mainbattlenum))
data$mainbattlenum_discrete <- NA
data$mainbattlenum_discrete[data$mainbattlenum < 0.5] <- "[0, 0.5)"
data$mainbattlenum_discrete[data$mainbattlenum >= 0.5 & data$mainbattlenum < 1] <- "[0.5, 1.0)"
data$mainbattlenum_discrete[data$mainbattlenum >= 1 & data$mainbattlenum < 1.5] <- "[1.0, 1.5)"
data$mainbattlenum_discrete[data$mainbattlenum >= 1.5] <- "[1.5, 3.25]"



