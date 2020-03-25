
library(dplyr)

ipums <- read.csv("Documents/Pitt/Data/ipums/usa_00005.csv")
prop_laws <- read.csv("Documents/Pitt/Data/property_laws/dates_property_laws.csv")
union <- read.csv("Documents/Pitt/Data/union/UnionarmydataMaddie.csv")

# ipums
ipums$SEX[ipums$SEX == 1] <- "Male"
ipums$SEX[ipums$SEX == 2] <- "Female"
#women <- ipums %>% filter(SEX == 2)
length(unique(ipums$COUNTYICP))

total_pop <- data.frame(table(ipums$YEAR, ipums$COUNTYICP, ipums$SEX))
table(ipums$SEX, ipums$YEAR, ipums$LABFORCE)

lfp <- ipums %>% group_by(COUNTYICP, STATEICP, YEAR, SEX, LABFORCE) %>% summarise(count = n()) %>% mutate(total = sum(count), percent = count/total)

plot_data <- merge(x = lfp, y = union, by.x = c("COUNTYICP", "STATEICP"), by.y = c("county_icpsr", "state_icpsr"), all = TRUE)
plot_data <- merge(x = plot_data, y = prop_laws, by.x = "state_ab", by.y = "Abbrv")