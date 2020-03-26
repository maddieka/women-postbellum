# preliminary data work for women LFP, political activism as function of broken civil war soldiers
library(dplyr)
library(ggplot2)
library(gridExtra)

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
#male_pop <- lfp %>% select(COUNTYICP, STATEICP, YEAR, SEX, total) %>% filter(SEX == "Male" & YEAR == 1860) %>% distinct() 
#male_pop <- male_pop %>% summarise(county_icpsr = COUNTYICP, state_icpsr = STATEICP, male_pop_1860 = total) %>% ungroup() %>% select(county_icpsr, state_icpsr, male_pop_1860)

#union2 <- merge(x = union, y = male_pop, by = c("county_icpsr", "state_icpsr"), all.x = TRUE)
plot_data <- merge(x = lfp, y = union, by.x = c("COUNTYICP", "STATEICP"), by.y = c("county_icpsr", "state_icpsr"), all = TRUE)
plot_data <- merge(x = plot_data, y = prop_laws, by.x = "state_ab", by.y = "Abbrv", all = TRUE)

#plot_data$percent_soldiers <- plot_data$total.y / plot_data$male_pop_1860
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

##### differntial LFP by sex #####

women <- 
ggplot(data = plot_data %>% filter(LABFORCE == 2 & SEX == "Female" & YEAR > 1850 & total.y < 25000), aes(x = whole, y = percent, color = as.factor(YEAR))) + 
    geom_point() + geom_smooth(method = lm) +
    labs(x = "Total Soldiers", y = "LFP - Women", color = "Year")

men <- 
    ggplot(data = plot_data %>% filter(LABFORCE == 2 & SEX == "Male" & YEAR > 1850 & total.y < 25000), aes(x = whole, y = percent, color = as.factor(YEAR))) + 
    geom_point() + geom_smooth(method = lm) +
    labs(x = "Total Soldiers", y = "LFP - Men", color = "Year")

grid.arrange(women, men, ncol = 2)

###### timing of earnings/property laws on # soldiers #####
plot <-
ggplot(data = plot_data %>% filter(total.y < 25000), aes(x = total.y, y = as.numeric(Earnings_HanesWolcott2013))) + 
    geom_point(alpha = 0.2) + geom_smooth() +
    labs(x = "Total Soldiers", y = "Year Married Women's Property Law Passed (Earnings)", subtitle = "Smoothed with generalized additive mode smoothing (GAM)")

plot + coord_cartesian(xlim = c(0,10000), ylim = c(1860, 1900)) + labs(x = "Total Soldiers", y = "Year Married Women's Property Law Passed (Earnings)", subtitle = "Smoothed with generalized additive mode smoothing (GAM); cropped xlim = c(0,10000), ylim = c(1860, 1900)")


plot <-
    ggplot(data = plot_data %>% filter(total.y < 25000), aes(x = total.y, y = as.numeric(Property_HanesWolcott2013))) + 
    geom_point(alpha = 0.2) + geom_smooth() +
    labs(x = "Total Soldiers", y = "Year Passed Married Women's Property Law (Property)", subtitle = "Smoothed with generalized additive mode smoothing (GAM)")

plot + coord_cartesian(xlim = c(0,10000), ylim = c(1860, 1900)) + labs(x = "Total Soldiers", y = "Year Passed Married Women's Property Law (Earnings)", subtitle = "Smoothed with generalized additive mode smoothing (GAM); cropped xlim = c(0,10000), ylim = c(1860, 1900)")


###### LFP of women on timing of MWPL ######
lfp_state <- ipums %>% group_by(STATEICP, STATEFIP, YEAR, SEX, LABFORCE) %>% summarise(count = n()) %>% mutate(total = sum(count), percent = count/total)
union_state <- union %>% group_by(state_ab) %>% summarise(died_state = sum(died, na.rm = TRUE),
                                                             disabwound_state = sum(disabwound, na.rm = TRUE),
                                                             total_state = sum(total, na.rm = TRUE),
                                                             whole_state = sum(whole, na.rm = TRUE)
                                                             )
fips <- unique(read.csv("Documents/Pitt/Data/geography/ssa_fips_state_county2017.csv")[,c("state","fipsstate")])
union_state <- merge(x = fips, y= union_state, by.x = "state", by.y = "state_ab", all.y = TRUE)

plot_data <- merge(x = lfp_state, y = union_state, by.x = "STATEFIP", by.y = "fipsstate", all = TRUE)
plot_data <- merge(x = plot_data, y = prop_laws, by.x = "state", by.y = "Abbrv", all = TRUE)


### EARNINGS
ggplot(data = plot_data %>% filter(LABFORCE == 2 & SEX == "Female" & YEAR > 1850), aes(x = Earnings_HanesWolcott2013, y = percent)) + 
    geom_point() + geom_smooth(method=lm) +
    labs(x = "Year Passed Married Women's Property Law (Earnings)", y = "LFP - Women", subtitle = "All years; lm")
# interpretation: lots of laws pass 2-3 years after the civil war... a peak in women's LFP follows and then levels off over time...
ggplot(data = plot_data %>% filter(LABFORCE == 2 & SEX == "Female" & YEAR > 1850), aes(x = Earnings_HanesWolcott2013, y = percent, color = as.factor(YEAR))) + 
    geom_point() + geom_smooth(method=loess) +
    labs(x = "Year Passed Married Women's Property Law (Earnings)", y = "LFP - Women", subtitle = "loess", color = "Year")

ggplot(data = plot_data %>% filter(LABFORCE == 2 & SEX == "Female" & YEAR > 1850), aes(x = Earnings_HanesWolcott2013, y = percent, color = as.factor(YEAR))) + 
    geom_point() + geom_smooth() +
    labs(x = "Year Passed Married Women's Property Law (Earnings)", y = "LFP - Women", subtitle = "Smoothed with generalized additive mode smoothing (GAM)", color = "Year")

### PROPERTY - good news that the property laws don't show as big of a pattern!
ggplot(data = plot_data %>% filter(LABFORCE == 2 & SEX == "Female" & YEAR > 1850), aes(x = Property_HanesWolcott2013, y = percent)) + 
    geom_point() + geom_smooth(method=lm) +
    labs(x = "Year Passed Married Women's Property Law (Property)", y = "LFP - Women", subtitle = "All years; lm")
# interpretation: lots of laws pass 2-3 years after the civil war... a peak in women's LFP follows and then levels off over time...
ggplot(data = plot_data %>% filter(LABFORCE == 2 & SEX == "Female" & YEAR > 1850), aes(x = Property_HanesWolcott2013, y = percent, color = as.factor(YEAR))) + 
    geom_point() + geom_smooth(method=loess) +
    labs(x = "Year Passed Married Women's Property Law (Property)", y = "LFP - Women", subtitle = "loess", color = "Year")

ggplot(data = plot_data %>% filter(LABFORCE == 2 & SEX == "Female" & YEAR > 1850), aes(x = Property_HanesWolcott2013, y = percent, color = as.factor(YEAR))) + 
    geom_point() + geom_smooth() +
    labs(x = "Year Passed Married Women's Property Law (Property)", y = "LFP - Women", subtitle = "Smoothed with generalized additive mode smoothing (GAM)", color = "Year")

hist(prop_laws$Earnings_HanesWolcott2013, breaks = 10)
ggplot(prop_laws, aes(x = Earnings_HanesWolcott2013)) + geom_histogram()

##### NEW DATA #####