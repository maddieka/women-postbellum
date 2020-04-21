# preliminary data work for women LFP, political activism as function of broken civil war soldiers
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

options(scipen=999)

#ipums <- read.csv("Documents/Pitt/Data/ipums/usa_00005.csv")
ipums1 <- vroom::vroom(file = "Documents/Pitt/Data/ipums/usa_00006.csv", delim = ",") # 1860, 1870

ipums60.70 <- ipums1 %>% 
    filter(AGE > 10 & AGE < 70) %>%
    group_by(STATEICP, COUNTYICP, YEAR, SEX) %>% summarise(count = n(),
                                                           workers = sum(LABFORCE == 2, na.rm = TRUE),
                                                           lfp = workers/count) %>% 
    ungroup()
rm(ipums1)

# write.csv(ipums60.70, "Documents/Pitt/Data/ipums/temp_ipums60-70.10age70.csv")
# ipums <- read.csv("Documents/Pitt/Data/ipums/temp_ipums60-70.10age70.csv")[-1]

ipums2 <- vroom::vroom(file = "Documents/Pitt/Data/ipums/usa_00007.csv", delim = ",") # 1880, 1900


# test <- ipums2 %>% group_by(STATEICP, COUNTYICP, YEAR, SEX, LABFORCE) %>% summarise(count = n()) %>% ungroup()

ipums80.00 <- ipums2 %>% 
    filter(AGE > 10 & AGE < 70) %>%
    group_by(STATEICP, COUNTYICP, YEAR, SEX) %>% summarise(count = n(),
                                                           workers = sum(LABFORCE == 2, na.rm = TRUE),
                                                           lfp = workers/count) %>% 
    ungroup()
rm(ipums2)

ipums80.00 <- ipums80.00 %>% filter(YEAR == 1880)

prop_laws <- read.csv("Documents/Pitt/Data/property_laws/dates_property_laws.csv")
union <- read.csv("Documents/Pitt/Data/union/UnionarmydataMaddie.csv")
union$nondead <- union$whole + union$disabwound
union_states <- c("ME", "NH", "VT", "NY", "MA", "RI", "CT", "PA", "NJ", "OH", "IN", "IL", "IA", "WI", "MN", "KS", "CA", "OR", "MI")
union <- union %>% filter(state_ab %in% union_states)

ipums <- rbind(ipums60.70, ipums80.00)
rm(list = c("ipums60.70", "ipums80.00"))

# ipums
ipums$SEX[ipums$SEX == 1] <- "Male"
ipums$SEX[ipums$SEX == 2] <- "Female"
#women <- ipums %>% filter(SEX == 2)
length(unique(ipums$COUNTYICP))

write.csv(ipums, "Documents/Pitt/Data/ipums/fullcnt18601880.csv")
ipums <- read.csv("Documents/Pitt/Data/ipums/fullcnt18601880.csv")[-1]
# total_pop <- data.frame(table(ipums$YEAR, ipums$COUNTYICP, ipums$SEX))
# table(ipums$SEX, ipums$YEAR, ipums$LABFORCE)
# 
# lfp <- ipums %>% group_by(COUNTYICP, STATEICP, YEAR, SEX, LABFORCE) %>% summarise(count = n()) %>% mutate(total = sum(count), percent = count/total)
#male_pop <- lfp %>% select(COUNTYICP, STATEICP, YEAR, SEX, total) %>% filter(SEX == "Male" & YEAR == 1860) %>% distinct() 
#male_pop <- male_pop %>% summarise(county_icpsr = COUNTYICP, state_icpsr = STATEICP, male_pop_1860 = total) %>% ungroup() %>% select(county_icpsr, state_icpsr, male_pop_1860)

#union2 <- merge(x = union, y = male_pop, by = c("county_icpsr", "state_icpsr"), all.x = TRUE)

men <- ipums %>% filter(SEX == "Male" & YEAR == 1860)
men <- merge(x = men, y = union, by.x = c("COUNTYICP", "STATEICP"), by.y = c("county_icpsr", "state_icpsr"))
men <- men %>% 
    mutate(percent_enlisted = total / count,
           percent_killed = died / count,
           percent_veterans = nondead / count
           ) %>% 
    select(COUNTYICP,STATEICP,percent_enlisted,percent_killed,percent_veterans)

union2 <- merge(x = union, y = men,  by.y = c("COUNTYICP", "STATEICP"), by.x = c("county_icpsr", "state_icpsr"), all.x = TRUE)

plot_data <- merge(x = ipums, y = union2, by.x = c("COUNTYICP", "STATEICP"), by.y = c("county_icpsr", "state_icpsr"))
plot_data <- merge(x = plot_data, y = prop_laws, by.x = "state_ab", by.y = "Abbrv")

# ggplot(data = plot_data %>% filter(SEX == "Female"), aes(x = percent_men_enlisted, y = lfp, color = as.factor(YEAR))) + 
#     geom_point() + geom_smooth(method = lm) +
#     labs(x = "Total Soldiers", y = "LFP - Women", color = "Year")
# 
# ggplot(data = plot_data %>% filter(LABFORCE == 2 & SEX == "Female"), aes(x = percent_men_enlisted, y = percent, color = as.factor(YEAR))) + 
#     geom_point() + geom_smooth(method = lm) +
#     labs(x = "Dead Soldiers", y = "LFP - Women", color = "Year")
#     
# ggplot(data = plot_data %>% filter(LABFORCE == 2 & SEX == "Female" & disabwound < 2500), aes(x = disabwound, y = percent, color = as.factor(YEAR))) + 
#     geom_point() + geom_smooth(method = lm)+
#     labs(x = "Disabled/Wounded Veterans", y = "LFP - Women", color = "Year")
# 
# ggplot(data = plot_data %>% filter(LABFORCE == 2 & SEX == "Female" & whole < 20000), aes(x = whole, y = percent, color = as.factor(YEAR))) + 
#     geom_point() + geom_smooth(method = lm)+
#     labs(x = "Healthy Veterans", y = "LFP - Women", color = "Year")

##### long version; facet above into one plot ####
plot_data_long <- gather(plot_data, soldier_type, soldier_pct, percent_enlisted, percent_killed, percent_veterans, factor_key = TRUE)

ggplot(data = plot_data_long %>% filter(!is.na(SEX) & soldier_pct <=1),
       aes(x = soldier_pct, y = lfp, color = as.factor(YEAR))) + 
    geom_point(alpha = .2) +
    geom_smooth(method=lm) +
    facet_wrap(~ SEX + soldier_type, ncol = 3) +
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(limits = c(0,1)) + 
    labs(x = "Soldier %", y = "Labor Force Participation", color = "Year", subtitle = "Union states only")

ggplot(data = plot_data_long %>% filter(!is.na(SEX) & soldier_pct <=1 & SEX == "Female" & soldier_type == "percent_enlisted"),
       aes(x = soldier_pct, y = lfp, color = as.factor(YEAR))) + 
    geom_point(alpha = .2) +
    geom_smooth(method=lm) +
    #facet_wrap(~ SEX + soldier_type, ncol = 3) +
    scale_color_brewer(palette = "PuRd") +
    #scale_y_continuous(limits = c(0,1)) + 
    labs(x = "Number of Veterans / 1860 Male Population", y = "Women's Labor Force Participation", color = "Year", subtitle = "Union states only")


##### differntial LFP by sex #####

# women <- 
# ggplot(data = plot_data %>% filter(LABFORCE == 2 & SEX == "Female" & YEAR > 1850 & total.y < 25000), aes(x = whole, y = percent, color = as.factor(YEAR))) + 
#     geom_point() + geom_smooth(method = lm) +
#     labs(x = "Total Soldiers", y = "LFP - Women", color = "Year")
# 
# men <- 
#     ggplot(data = plot_data %>% filter(LABFORCE == 2 & SEX == "Male" & YEAR > 1850 & total.y < 25000), aes(x = whole, y = percent, color = as.factor(YEAR))) + 
#     geom_point() + geom_smooth(method = lm) +
#     labs(x = "Total Soldiers", y = "LFP - Men", color = "Year")
# 
# grid.arrange(women, men, ncol = 2)

###### timing of earnings/property laws on # soldiers #####
plot_data_long <- gather(plot_data_long, law_type, law_yr, Property_HanesWolcott2013, Earnings_HanesWolcott2013, factor_key = TRUE)

ggplot(data = plot_data_long %>% filter(SEX == "Female"), aes(x = soldier_pct, y = law_yr, size = count, color = lfp, weight = count)) + 
    geom_point(alpha = 0.2) + 
    geom_smooth(method=lm) +
    facet_wrap(~ law_type + soldier_type, ncol = 3) +
    scale_x_continuous(limits = c(0,1)) + 
    scale_y_continuous(limits = c(1840,1950)) +
    scale_color_viridis_c(option = "magma", direction = -1) +
    labs(x = " Soldier %", y = "Year Married Women's Property Law Passed", subtitle = "County level", size = "Women's Population", color = "Women's LFP")

union_state <- 
    union2 %>% 
    select(state_ab, died, disabwound, total, nondead, whole) %>%
    group_by(state_ab) %>%
    summarise(died = sum(died, na.rm = TRUE),
          disabwound = sum(disabwound, na.rm = TRUE),
          total = sum(total, na.rm = TRUE),
          nondead = sum(nondead, na.rm = TRUE),
          whole = sum(whole, na.rm = TRUE)
          ) %>%
    ungroup()
plot_data_state <- plot_data %>% ungroup() %>%
    select(state_ab, YEAR, SEX, count, workers, State) %>%
    group_by(state_ab, YEAR, SEX, State) %>%
    summarise(count = sum(count, na.rm = TRUE),
              workers = sum(workers, na.rm = TRUE))
male_pop_1860 <- plot_data_state %>% filter(YEAR == 1860, SEX == "Male")
union_state <- merge(x = union_state, y = male_pop_1860, by = "state_ab")
union_state <- union_state %>% mutate(percent_enlisted = total / count,
                                      percent_killed = died / count,
                                      percent_veterans = nondead / count
                                      ) %>% 
    select(state_ab, percent_enlisted,percent_killed,percent_veterans)
plot_data_state <- merge(x = plot_data_state, y = union_state, by = "state_ab", all.x = TRUE)
plot_data_state$lfp <- plot_data_state$workers / plot_data_state$count
plot_data_state <- merge(x = plot_data_state, y = prop_laws, by.x = "state_ab", by.y = "Abbrv", all.x = TRUE)
plot_data_state_long <- gather(plot_data_state, law_type, law_yr, Property_HanesWolcott2013, Earnings_HanesWolcott2013, factor_key = TRUE)
plot_data_state_long <- gather(plot_data_state_long, soldier_type, soldier_pct, percent_enlisted, percent_killed, percent_veterans, factor_key = TRUE)

ggplot(data = plot_data_state_long %>% filter(SEX == "Female"), aes(x = soldier_pct, y = law_yr, size = count, color = lfp)) + 
    geom_point(alpha = 0.2) + 
    geom_smooth(method=lm) +
    facet_wrap(~ law_type + soldier_type, ncol = 3) +
    scale_x_continuous(limits = c(0,1)) + 
    scale_y_continuous(limits = c(1840,1950)) +
    scale_color_viridis_c(option = "magma", direction = -1) +
    labs(x = " Soldier %", y = "Year Married Women's Property Law Passed", subtitle = "State level", size = "Women's Population", color = "Women's LFP")





# plot + coord_cartesian(xlim = c(0,10000), ylim = c(1860, 1900)) + labs(x = "Total Soldiers", y = "Year Married Women's Property Law Passed (Earnings)", subtitle = "Smoothed with generalized additive mode smoothing (GAM); cropped xlim = c(0,10000), ylim = c(1860, 1900)")
# 
# 
# ggplot(data = plot_data_long, aes(x = soldier_pct, y = as.numeric(Property_HanesWolcott2013))) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth(method=lm) +
#     facet_wrap(~soldier_type) +
#     scale_x_continuous(limits = c(0,1)) + 
#     scale_y_continuous(limits = c(1825,1950)) 
#     labs(x = "Total Soldiers", y = "Year Married Women's Property Law Passed (Earnings)", subtitle = "Smoothed with generalized additive mode smoothing (GAM)")
#     
# plot + coord_cartesian(xlim = c(0,10000), ylim = c(1860, 1900)) + labs(x = "Total Soldiers", y = "Year Passed Married Women's Property Law (Earnings)", subtitle = "Smoothed with generalized additive mode smoothing (GAM); cropped xlim = c(0,10000), ylim = c(1860, 1900)")
# 

# ggplot(data = prop_data_long %>% filter(soldier_cnt < 25000 & soldier_type == "total.y"), aes(x = soldier_cnt, y = as.numeric(law_yr))) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth() +
#     facet_wrap(~ law_type) +
#     labs(x = "Total Soldiers", y = "Year Married Women's Property Law Passed", subtitle = "Smoothed with generalized additive mode smoothing (GAM)")
# 
# ggplot(data = prop_data_long %>% filter(soldier_cnt < 25000 & soldier_type == "total.y"), aes(x = soldier_cnt, y = as.numeric(law_yr))) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth(method = glm) +
#     facet_wrap(~ law_type) +
#     labs(x = "Total Soldiers", y = "Year Married Women's Property Law Passed", subtitle = "glm")
# 
# ggplot(data = prop_data_long %>% filter(soldier_cnt < 25000 & soldier_type == "total.y"), aes(x = soldier_cnt, y = as.numeric(law_yr))) + 
#     geom_point(alpha = 0.2) + 
#     geom_smooth() +
#     facet_wrap(~ law_type) +
#     coord_cartesian(xlim = c(0,10000), ylim = c(1855, 1900)) + 
#     labs(x = "Total Soldiers", y = "Year Married Women's Property Law Passed (Earnings)", subtitle = "Smoothed with generalized additive mode smoothing (GAM); cropped xlim = c(0,10000), ylim = c(1855, 1900)")
# 
###### LFP of women on timing of MWPL ######
# lfp_state <- ipums %>% group_by(STATEICP, YEAR, SEX) %>% summarise(count = n()) %>% mutate(total = sum(count), percent = count/total)
# union_state <- union %>% group_by(state_ab) %>% summarise(died_state = sum(died, na.rm = TRUE),
#                                                              disabwound_state = sum(disabwound, na.rm = TRUE),
#                                                              total_state = sum(total, na.rm = TRUE),
#                                                              whole_state = sum(whole, na.rm = TRUE)
#                                                              )
# fips <- unique(read.csv("Documents/Pitt/Data/geography/ssa_fips_state_county2017.csv")[,c("state","fipsstate")])
# union_state <- merge(x = fips, y= union_state, by.x = "state", by.y = "state_ab", all.y = TRUE)
# 
# plot_data_state <- merge(x = lfp_state, y = union_state, by.x = "STATEFIP", by.y = "fipsstate", all = TRUE)
# plot_data_state <- merge(x = plot_data, y = prop_laws, by.x = "state", by.y = "Abbrv", all = TRUE)


### EARNINGS
ggplot(data = plot_data_long %>% filter(YEAR %in% c(1860, 1870,1880) & law_yr > 1850 & SEX == "Female"), aes(y = lfp, x = law_yr, color = as.factor(YEAR), size = count)) + 
        geom_point(alpha = .2) + 
        geom_smooth(method = lm) +
        facet_wrap(~ law_type) +
    scale_color_brewer(palette = "Dark2") +
    #annotate("rect", xmin = 1861, xmax = 1866, ymin = 0, ymax = .51, alpha = .2) +
    #scale_y_continuous(limits = c(1840, 1945)) +
    #scale_x_continuous(limits = c(0,1)) +
    labs(x = "Year Passed Married Women's Property Law (Earnings)", y = "Women's LFP", subtitle = "County level")


ggplot(data = plot_data_state_long %>% filter(YEAR %in% c(1860, 1870,1880) & SEX == "Female"), aes(y = lfp, x = law_yr, color = as.factor(YEAR), size = count)) + 
    geom_point(alpha = .2) + 
    geom_smooth(method = lm) +
    facet_wrap(~ law_type) +
    scale_color_brewer(palette = "Dark2") +
    #annotate("rect", xmin = 1861, xmax = 1866, ymin = 0, ymax = .29, alpha = .2) +
    #scale_y_continuous(limits = c(1840, 1945)) +
    #scale_x_continuous(limits = c(0,1)) +
    labs(x = "Year Passed Married Women's Property Law (Earnings)", y = "Women's LFP", subtitle = "State level")




# interpretation: lots of laws pass 2-3 years after the civil war... a peak in women's LFP follows and then levels off over time...
ggplot(data = plot_data_state %>% filter(LABFORCE == 2 & SEX == "Female" & YEAR > 1850), aes(x = Earnings_HanesWolcott2013, y = percent, color = as.factor(YEAR))) + 
    geom_point() + geom_smooth(method=loess) +
    labs(x = "Year Passed Married Women's Property Law (Earnings)", y = "LFP - Women", subtitle = "loess", color = "Year")

ggplot(data = plot_data_state %>% filter(LABFORCE == 2 & SEX == "Female" & YEAR > 1850), aes(x = Earnings_HanesWolcott2013, y = percent, color = as.factor(YEAR))) + 
    geom_point() + geom_smooth() +
    labs(x = "Year Passed Married Women's Property Law (Earnings)", y = "LFP - Women", subtitle = "Smoothed with generalized additive mode smoothing (GAM)", color = "Year")

### PROPERTY - good news that the property laws don't show as big of a pattern!
ggplot(data = plot_data_state %>% filter(LABFORCE == 2 & SEX == "Female" & YEAR > 1850), aes(x = Property_HanesWolcott2013, y = percent)) + 
    geom_point() + geom_smooth(method=lm) +
    labs(x = "Year Passed Married Women's Property Law (Property)", y = "LFP - Women", subtitle = "All years; lm")

ggplot(data = plot_data_state %>% filter(LABFORCE == 2 & SEX == "Female" & YEAR > 1850), aes(x = Property_HanesWolcott2013, y = percent, color = as.factor(YEAR))) + 
    geom_point() + geom_smooth(method=loess) +
    labs(x = "Year Passed Married Women's Property Law (Property)", y = "LFP - Women", subtitle = "loess", color = "Year")

ggplot(data = plot_data_state %>% filter(LABFORCE == 2 & SEX == "Female" & YEAR > 1850), aes(x = Property_HanesWolcott2013, y = percent, color = as.factor(YEAR))) + 
    geom_point() + geom_smooth() +
    labs(x = "Year Passed Married Women's Property Law (Property)", y = "LFP - Women", subtitle = "Smoothed with generalized additive mode smoothing (GAM)", color = "Year")

prop_data_long_state <- gather(plot_data_state, law_type, law_yr, Property_HanesWolcott2013, Earnings_HanesWolcott2013, factor_key = TRUE)
ggplot(data = prop_data_long_state %>% filter(LABFORCE == 2 & YEAR > 1850), aes(x = law_yr, y = percent, color = as.factor(YEAR))) + 
    geom_point(alpha = .2) + 
    geom_smooth() +
    facet_wrap(~ law_type + SEX, ) +
    scale_x_continuous(breaks = seq(1840,1930,10)) +
    scale_color_brewer(palette = "RdPu") +
    labs(x = "Year Passed Married Women's Property Law", y = "Labor Force Participation", subtitle = "Smoothed with generalized additive mode smoothing (GAM)", color = "Year", title = "State level")

ggplot(data = prop_data_long %>% filter(LABFORCE == 2 & YEAR > 1850), aes(x = law_yr, y = percent, color = as.factor(YEAR))) + 
    geom_point(alpha = .2) + 
    geom_smooth() +
    facet_wrap(~ law_type + SEX, ) +
    scale_x_continuous(breaks = seq(1840,1930,10)) +
    scale_color_brewer(palette = "RdPu") +
    labs(x = "Year Married Women's Property Law Passed", y = "Labor Force Participation", subtitle = "Smoothed with generalized additive mode smoothing (GAM)", color = "Year", title = "County level")


hist(prop_laws$Earnings_HanesWolcott2013, breaks = 10)
plot_data_long_state <- plot_data_long %>% select(state_ab, law_type, law_yr) %>% distinct()
plot_data_long_state$law_type <- ifelse(test = plot_data_long_state$law_type == "Property_HanesWolcott2013", yes = "Property", "Earnings")
ggplot(plot_data_long_state, aes(x = law_yr, fill = law_type)) + 
    geom_histogram(binwidth = 1) + 
    annotate("rect", xmin = 1861, xmax = 1866, ymin = 0, ymax = 7, alpha = .2) + 
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(breaks = seq(1840,1950,5)) +
    theme(legend.position="bottom",
          legend.title=element_text(size=20), 
          legend.text=element_text(size=20),
          legend.key.width=unit(3,"cm"),
          axis.text = element_text(size=20),
          axis.title = element_text(size=20)) +
    labs(x = "Year", y = "Count", fill = "")

#### MAPS ####
states <- map_data("state")

data = unique(plot_data_long %>% filter(law_type == "Earnings_HanesWolcott2013") %>% select(State, law_yr))
data$region <- tolower(data$State)
earnings <- inner_join(states, data, by = "region")
ggplot(data = earnings) + 
    geom_polygon(aes(x = long, y = lat, group = group, fill = law_yr), color = "black") +
    scale_fill_gradient2(midpoint = 1865, low = "#e41a1c", high = "#377eb8") +
    labs(fill = "Year Married Women's Earnings Law Passed")
 
ggplot() + 
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
    geom_polygon(data = earnings, aes(x = long, y = lat, group = group, fill = law_yr), color = "black") +
    scale_fill_viridis_c(option = "magma") 
    labs(fill = "Year Earnings Law Passed") 
    
earnings$decade <- ifelse(test = earnings$law_yr <= 1859, yes = "Before 1860", no = NA)
earnings$decade[earnings$law_yr %in% 1860:1869] <- "1870s" 
earnings$decade[earnings$law_yr %in% 1870:1879] <- "1870s"  
earnings$decade[earnings$law_yr %in% 1880:1889] <- "1880s"     
earnings$decade[earnings$law_yr >= 1890] <- "1890 or later"

ggplot() + 
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
    geom_polygon(data = earnings, aes(x = long, y = lat, group = group, fill = decade), color = "black") 
labs(fill = "Year Earnings Law Passed") 

plot_union_state <- plot_data_state_long %>% select(state_ab,State.x, soldier_type, soldier_pct) %>% distinct()
plot_union_state$region <- tolower(plot_union_state$State.x)
plot_union <- inner_join(states, plot_union_state, by.x = "region")    
    
ggplot() + 
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = NA, color = "black") +
    geom_polygon(data = plot_union %>% filter(soldier_type == "percent_veterans"), aes(x = long, y = lat, group = group, fill = soldier_pct), color = "black") +
    scale_fill_viridis_c() +
    theme_minimal() +
    theme(legend.position="bottom",
          legend.title=element_text(size=25), 
          legend.text=element_text(size=25),
          legend.key.width=unit(5,"cm"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()) +
    labs(fill = "% of 1860 Male Population Returning as Veterans") 



lfp1 <- lm(formula = lfp  ~ factor(YEAR) + percent_enlisted + percent_veterans + count + state_ab, data = plot_data %>% filter(SEX == "Female"))
stargazer(lfp1)
mwpl1 <- lm(Earnings_HanesWolcott2013 ~ lfp + percent_enlisted + percent_veterans + count, data = plot_data %>% filter(SEX == "Female"))
stargazer(mwpl1)

mwpl1state <- lm(Earnings_HanesWolcott2013 ~ lfp + percent_enlisted + percent_veterans + count, data = plot_data_state %>% filter(SEX == "Female"))
stargazer(mwpl1state)

lfp2 <- lm(formula = lfp  ~ factor(YEAR) + Earnings_HanesWolcott2013, data = plot_data %>% filter(SEX == "Female"))
stargazer(lfp2)

lfp2state <- lm(formula = lfp  ~ factor(YEAR) + Earnings_HanesWolcott2013, data = plot_data_state %>% filter(SEX == "Female"))
stargazer(lfp2state)

plot_data$percent_veteransX100 <- plot_data$percent_veterans*100
plot_data$EarningsBefore1870 <- ifelse(test = plot_data$Earnings_HanesWolcott2013 < 1870, yes = 1, no = 0)
plot_data$interaction <- plot_data$EarningsBefore1870*plot_data$percent_veteransX100

lfp1basic <- lm(formula = lfp  ~ factor(YEAR) + percent_veteransX100  + factor(COUNTYICP), data = plot_data %>% filter(SEX == "Female"))
summary(lfp1basic)
stargazer(lfp1basic)


stargazer(whole_population, working_age_pop)

alcohol <- read.delim("Documents/Pitt/History/alcohol.txt")
alcohol <- separate(data = alcohol,col =  Year.Spirits.Liquors.Wine, sep = " ", into = c("year", "spirits", "liquor", "wine"))

alcohol$year <- as.numeric(alcohol$year)
alcohol$spirits <- as.numeric(gsub('[[:punct:] ]+','',alcohol$spirits))
alcohol$liquor <- as.numeric(gsub('[[:punct:] ]+','',alcohol$liquor))
alcohol$wine <- as.numeric(gsub('[[:punct:] ]+','',alcohol$wine))

alcohol$total <- rowSums(alcohol[,2:4], na.rm = TRUE)
alcohol <- gather(alcohol, key = type, value = gallons, - year)

ggplot(data = alcohol, aes(x = as.numeric(year), y = (gallons), color = type)) + 
    geom_point() + 
    geom_line() +
    annotate("rect", xmin = 1861, xmax = 1866, ymin = 0, ymax = 1000000000, alpha = .2)

dealers <- read.delim("Documents/Pitt/History/dealers.txt")
dealers <- separate(data = dealers,col =  Year.liquorDealers.maltliquordealers.total.Dealerspercapita, sep = " ", into = c("year", "retail_liquor_dealers", "retail_malt_liquor_dealers", "total","total_dealers_per_capita"))

dealers$year <- as.numeric(dealers$year)
dealers$retail_liquor_dealers <- as.numeric(gsub('[[:punct:] ]+','',dealers$retail_liquor_dealers))
dealers$retail_malt_liquor_dealers <- as.numeric(gsub('[[:punct:] ]+','',dealers$retail_malt_liquor_dealers))
dealers$total <- as.numeric(gsub('[[:punct:] ]+','',dealers$total))
dealers$total_dealers_per_capita <- as.numeric(dealers$total_dealers_per_capita)

#dealers <- gather(dealers, key = type, value = gallons, - year)

ggplot(data = dealers, aes(x = year, y = total_dealers_per_capita)) + 
    geom_point() + 
    geom_line() +
    annotate("rect", xmin = 1861, xmax = 1866, ymin = 0, ymax = 4.5, alpha = .2) +
    geom_hline(yintercept = 2.3, linetype="dashed") + 
    labs(x = "Year", y = "Retail liquor dealers per capita") 


gallons <- read.csv("Documents/Pitt/History/blocker_table4.csv")
list <- strsplit(x = as.character(gallons$YEAR), split = " ")

df <- data.frame(YEAR=character(),
                 NEW_DISTILLED=character(),
                 RORABAUGH_DISTILLED=character(),
                 NEW_FERMENTED=character(),
                 RORABAUGH_FERMENTED=character(),
                 NEW_WINE=character(),
                 RORABAUGH_WINE=character(),
                 NEW_TOTAL=character(),
                 RORABAUGH_TOTAL=character(),
                 stringsAsFactors = FALSE
                 )
df[1,] <- c(list[[4]][1], list[[4]][2], list[[4]][3], list[[4]][4], list[[4]][5], NA, list[[4]][6], list[[4]][7],list[[4]][8])

for (i in 1:59) {
    if (length(list[[i]]) == 4){
        df[nrow(df)+1, ] <- c(list[[i]][1], list[[i]][2], NA, list[[i]][3], NA, NA, NA, list[[i]][4], NA)
        }
    else if (length(list[[i]]) == 5){
        df[nrow(df)+1, ] <- c(list[[i]][1], list[[i]][2], NA, list[[i]][3], NA, list[[i]][4], NA, list[[i]][5], NA)
        }
    else if (length(list[[i]]) == 7){
        df[nrow(df)+1, ] <- c(list[[i]][1], list[[i]][2], list[[i]][3], list[[i]][4], NA, list[[i]][5], NA, list[[i]][6], list[[i]][7])
        }
    else if (length(list[[i]]) == 9){
        df[nrow(df)+1, ] <- c(list[[i]][1], list[[i]][2], list[[i]][3], list[[i]][4], list[[i]][5], list[[i]][6], list[[i]][7], list[[i]][8], list[[i]][9])
    }
}

df_long <- gather(df, condition, value, NEW_DISTILLED, NEW_FERMENTED, NEW_WINE, NEW_TOTAL)

ggplot(data = df_long, aes(x = as.numeric(YEAR), y = as.numeric(value), color = condition)) + geom_point() + geom_line()
