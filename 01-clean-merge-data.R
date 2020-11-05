# creating county-level data for union soldiers and county characteristics

# load libraries and settings
library(haven) # read_dta()
library(dplyr)
options(scipen=999) # prevent scientific notation

#### import data ####
# andy's union army data
union <- read_dta(file = "~/Documents/Pitt/Projects/women_civil_war/data/andy_ua_data/UA_battle_and_casualty_data_2020-06-15.dta")
union <- union[!is.na(union$county_icpsr), ] # remove NA county icpsr (i think state aggregates)

# county crosswalk
crosswalk <- read.csv("~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_county_crosswalk.csv")[,1:5]
# icpsr 1860 county characteristics -- NEED TO ADD MORE YEARS THAN JUST 1860
variables <- c("state","county","name","totpop","urb860","urb25","wmtot","wftot","farmval","homemfg","mfgestab","mfgcap","mfglabm","mfglabf","level","fips","statefip",
               "equipval", "homemfg","farm39","farm1019" ,"farm2049", "farm5099", "farm100","farm500","farm1000", "mfgestab" ,"mfgcap","mfgout","realest","churches", "water","rail","fbwtot","mfglabf","quaker","quakacc","germref","germracc","shaker","shakacc")
icpsr1860 <- read_dta(file = "~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_02896/DS0009/02896-0009-Data.dta")[,variables]
icpsr1860 <- icpsr1860[icpsr1860$level == 1,] # level == 1 for county, 2 for state, and 3 for whole country
# ipums - already processed
source("~/git/women-postbellum/01-clean-ipums-data.R")

# wctu
wctu_data <- read_excel("~/Documents/Pitt/Projects/women_civil_war/data/wctu_county_level.xlsx")[,1:4]

table(wctu_data$state, wctu_data$year)
wctu_data <- wctu_data %>% filter(year %in% c(1875, 1880, 1882)) # SELECT WHICH CROSS-SECTIONS YOU WANT (earliest year available for MI, PA, IN, and OH)

wctu_data$key <- gsub("[^[:alnum:]]", "", wctu_data$county) # remove all spaces and non-alphanumeric symbols (mainly targeting "Mackinac/Michilim")
wctu_data$key <- gsub(pattern = "Alger", replacement = "Schoolcraft", x = wctu_data$key) # Alger County was split off from Schoolcraft County in 1885
wctu_data$key <- gsub(pattern = "Baraga", replacement = "Houghton", x = wctu_data$key) # Baraga County was split off from Houghton County in 1875
wctu_data$key <- gsub(pattern = "Arenac", replacement = "Bay", x = wctu_data$key) # Arenac County was split off from Bay County in 1883
wctu_data$key <- gsub(pattern = "Charlevoix", replacement = "Emmet", x = wctu_data$key) # Charlevoix County was split off from Emmet County in 1869
wctu_data$key <- gsub(pattern = "Dickinson", replacement = "Marquette", x = wctu_data$key) # Dickinson County was split off from Marquette County in 1891
wctu_data$key <- gsub(pattern = "Gogebic", replacement = "Ontonagon", x = wctu_data$key) # Gogebic County was split off from Ontonagon County in 1887
wctu_data$key <- gsub(pattern = "Iron", replacement = "Marquette", x = wctu_data$key) # Iron County was split off from Marquette County in 1890
wctu_data$key <- gsub(pattern = "IsleRoyale", replacement = "Marquette", x = wctu_data$key) # Isle Royale County was split off from Keweenaw County in 1875 (later reincorporated in 1897)
wctu_data$key <- gsub(pattern = "Luce", replacement = "Chippewa", x = wctu_data$key) # Luce County was split off from Chippewa County in 1887
wctu_data$key <- gsub(pattern = "Menominee", replacement = "Delta", x = wctu_data$key) # Menominee County was split off from Delta County in 1861 (named in 1863)

wctu_data$count_unions[is.na(wctu_data$count_unions)] <- 0 # if no membership data, assume zero unions in this county

wctu_data <- wctu_data %>%
  group_by(year, state, key) %>%
  summarise(count_unions = sum(count_unions, na.rm = TRUE)) %>% # e.g. Alger and Schoolcraft both have WCTU data, but during the Civil War, both of these were 1 county. Sum up chapters by Civil War geography
  ungroup()

wctu_data$has_union <- ifelse(wctu_data$count_unions > 0, yes = 1, no = 0)

# presidential election 1860 returns
votes <- read_dta(file = "~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_08611/DS0001/08611-0001-Data.dta")[,c(1,3,110,115)]
names(votes) <- c("STATEICP","COUNTYICP","PctVoteRepublican1860","PresVoteTurnout1860")
votes[votes == max(votes$PctVoteRepublican1860)] <- NA # 999.9 is code for NA
# votes$PresVoteTurnout1860[votes$PresVoteTurnout1860 > 100] <- # percentages over 100% -- i will leave these in for now because the denominator is an estimate, so percentages > 100 don't necessarily mean the # votes was wrong, rather the estimated voter-eligible population was wrong... still means there was high turnout. and i'm supposed to remove this variable from the analysis as of 11/4/2020 anyways

#### merge ####
# merge army data with icpsr county characteristics
data <- merge(x = union, y = icpsr1860, by.x = c("county_icpsr", "state_icpsr"), by.y = c("county", "state"), all.x = TRUE)

# merge union data with crosswalk
data <- merge(x = crosswalk, y = data, by.y = c("county_icpsr", "state_icpsr"), by.x = c("COUNTYICP", "STATEICP"), all.y = TRUE)

# merge ipums data so that county-level LFPs are included
data <- merge(x = data, y = ipums_wider, by = c("COUNTYICP", "STATEICP"), all.x = TRUE)

# merge wctu data
data$key <- gsub("[^[:alnum:]]", "", data$County)
data <- merge(x = data, y = wctu_data, by.x = c("State","key"), by.y = c("state","key"), all = TRUE)

# merge 1860 election returns data
data <- merge(x = data, y = votes, by = c("COUNTYICP","STATEICP"), all.x = TRUE)

#### subset (exclude non-core union states) and create new variables ####
union_states <- c("Maine", "New Hampshire", "Vermont", "New York", "Massachusetts", "Rhode Island", "Connecticut", "Pennsylvania", "New Jersey", "Ohio", "Indiana", "Illinois", "Iowa", "Wisconsin", "Minnesota", "Michigan") # only core states: exluces CA, WA, KS, and boundary states
data <- data %>% filter(State %in% union_states)

table(data$State) # pretty much the correct number of county-level observations per state

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
data$mainbattlenum_discrete <- NA
data$mainbattlenum_discrete[data$mainbattlenum < 0.5] <- "[0, 0.5)"
data$mainbattlenum_discrete[data$mainbattlenum >= 0.5 & data$mainbattlenum < 1] <- "[0.5, 1.0)"
data$mainbattlenum_discrete[data$mainbattlenum >= 1 & data$mainbattlenum < 1.5] <- "[1.0, 1.5)"
data$mainbattlenum_discrete[data$mainbattlenum >= 1.5] <- "[1.5, 3.25]"



