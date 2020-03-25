library(ggplot2)
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
plot_data <- merge(x = plot_data, y = prop_laws, by.x = "state_ab", by.y = "Abbrv", all = TRUE)

###### LFP of women on # soldiers by year ######
ggplot(data = plot_data %>% filter(LABFORCE == 2 & SEX == "Female" & YEAR > 1850 & total.y < 25000), aes(x = total.y, y = percent, color = as.factor(YEAR))) + 
    geom_point() + geom_smooth(method = lm) +
    labs(x = "Total Soldiers", y = "LFP - Women", color = "Year")


ggplot(data = plot_data %>% filter(LABFORCE == 2 & SEX == "Female"), aes(x = died, y = percent, color = as.factor(YEAR))) + 
    geom_point() + geom_smooth(method = lm) +
    labs(x = "Dead Soldiers", y = "LFP - Women", color = "Year")
    
ggplot(data = plot_data %>% filter(LABFORCE == 2 & SEX == "Female" & disabwound < 2500), aes(x = disabwound, y = percent, color = as.factor(YEAR))) + 
    geom_point() + geom_smooth(method = lm)+
    labs(x = "Disabled/Wounded Veterans", y = "LFP - Women", color = "Year")

ggplot(data = plot_data %>% filter(LABFORCE == 2 & SEX == "Female" & whole < 20000), aes(x = whole, y = percent, color = as.factor(YEAR))) + 
    geom_point() + geom_smooth(method = lm)+
    labs(x = "Healthy Veterans", y = "LFP - Women", color = "Year")

###### timing of earnings/property laws on # soldiers #####
ggplot(data = plot_data %>% filter(total.y < 25000), aes(x = total.y, y = as.numeric(Earnings_HanesWolcott2013))) + 
    geom_point(alpha = 0.2) + geom_smooth() +
    labs(x = "Total Soldiers", y = "Year Passed Married Women's Property Law (Earnings)", subtitle = "Smoothed with generalized additive mode smoothing (GAM)")
ggplot(data = plot_data %>% filter(total.y < 5000), aes(x = total.y, y = as.numeric(Earnings_HanesWolcott2013))) + 
    geom_point(alpha = 0.2) + geom_smooth() +
    labs(x = "Total Soldiers", y = "LFP - Women", color = "Year")

