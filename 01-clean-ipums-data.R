# temperance project
# cleaning ipums data
# created 6/19/2020

library(dplyr) # data manipulation
library(tidyr) # wide to long
library(vroom) # import large csv

options(scipen=999) # prevent R from storing small numbers in scientific notation (for small LFPs, etc.)

### LINES 13-65 TAKE A LONG TIME TO RUN: THE OUTPUT IS SAVED AND LOADED IN ON LINE 44 TO SAVE TIME. TO MAKE CHANGES TO THE ORIGINAL STRUCTURE OF ipums_long, UNCOMMENT AND RUN AGAIN.
# 1850
# ipums <- vroom::vroom(file = "~/Documents/Pitt/Data/ipums/usa_00010.csv", delim = ",") # 1850
#
# ipums50 <-
#     ipums %>% # begin with raw ipums data for 1860 and 1870
#     filter(AGE > 10) %>%
#     group_by(STATEICP, COUNTYICP, YEAR, SEX) %>% summarise(count = n(),
#                                                            workers = sum(LABFORCE == 2, na.rm = TRUE),
#                                                            lfp = workers/count) %>%
#     ungroup()
#
# rm(ipums)
#
# write.csv(ipums50, "Documents/Pitt/Data/ipums/temp.csv") # store ipums50 in case R crashes, again...
#
# # 1860-1870
# ipums1 <- vroom::vroom(file = "~/Documents/Pitt/Data/ipums/usa_00006.csv", delim = ",") # 1860, 1870
#
# ipums60.70 <-
#     ipums1 %>% # begin with raw ipums data for 1860 and 1870
#     filter(AGE > 10) %>%
#     group_by(STATEICP, COUNTYICP, YEAR, SEX) %>% summarise(count = n(),
#                                                            workers = sum(LABFORCE == 2, na.rm = TRUE),
#                                                            lfp = workers/count) %>%
#     ungroup()
#
# write.csv(rbind(ipums50, ipums60.70), "Documents/Pitt/Data/ipums/temp.csv") # store ipums50, 60, and 70 in case R crashes, again...
#
# rm(ipums1,ipums60.70)
#
# # 1880
# ipums2 <- vroom::vroom(file = "~/Documents/Pitt/Data/ipums/usa_00011.csv", delim = ",") # 1880
#
# ipums80 <- ipums2 %>%
#     filter(AGE > 10) %>%
#     group_by(STATEICP, COUNTYICP, YEAR, SEX) %>% summarise(count = n(),
#                                                            workers = sum(LABFORCE == 2, na.rm = TRUE),
#                                                            lfp = workers/count) %>%
#     ungroup()
#
# rm(ipums2)
# temp <- read.csv("~/Documents/Pitt/Data/ipums/temp.csv")[-1]
# write.csv(rbind(temp, ipums80), "Documents/Pitt/Data/ipums/temp.csv") # store ipums50-80 in case R crashes, again...
# rm(ipums80, temp)

# 1900 (1890 unavailable)
# ipums3 <- vroom::vroom(file = "~/Documents/Pitt/Data/ipums/usa_00013.csv", delim = ",") # 1900
# ipums3$LABFORCE <- ifelse(test = ipums3$MOUNEMP > 0, yes = 2, no = 1)
# ipums00 <- ipums3 %>%
#     filter(AGE > 10) %>%
#     group_by(STATEICP, COUNTYICP, YEAR, SEX) %>% summarise(count = n(),
#                                                            workers = sum(LABFORCE == 2, na.rm = TRUE),
#                                                            lfp = workers/count) %>%
#     ungroup()
#
# # combine 1850-1900
# temp <- read.csv("~/Documents/Pitt/Data/ipums/temp.csv")[-1]
# ipums_long <- rbind(temp, ipums00)
#
# rm(ipums00, ipums3, temp)
# write.csv(ipums_long, "~/Documents/Pitt/Data/ipums/fullcnt1850-1900.csv", row.names = FALSE)

# create wide format
ipums_long <- read.csv("~/Documents/Pitt/Data/ipums/fullcnt1850-1900.csv")
ipums_long$SEX[ipums_long$SEX == 1] <- "Male"
ipums_long$SEX[ipums_long$SEX == 2] <- "Female"

# create wide format data where LFP is wide, and YEAR is still long
ipums_wide <-
    ipums_long %>%
    gather(variable, value, -(STATEICP:SEX)) %>%
    unite(temp, SEX, variable) %>%
    spread(temp, value)

# create wide format data where both LFP and YEAR are long
# then change variable names to prevent leading numbers (Female_count_1850 instead of `1850_Female_count``)
ipums_wider <-
    ipums_wide %>%
    gather(variable, value, -(STATEICP:YEAR)) %>%
    unite(temp, YEAR, variable) %>%
    spread(temp, value)
names(ipums_wider) <- c("STATEICP","COUNTYICP","Female_count_1850","Female_lfp_1850","Female_workers_1850","Male_count_1850","Male_lfp_1850","Male_workers_1850","Female_count_1860","Female_lfp_1860","Female_workers_1860","Male_count_1860","Male_lfp_1860","Male_workers_1860","Female_count_1870","Female_lfp_1870","Female_workers_1870","Male_count_1870","Male_lfp_1870","Male_workers_1870","Female_count_1880","Female_lfp_1880","Female_workers_1880","Male_count_1880","Male_lfp_1880","Male_workers_1880","Female_count_1900","Female_lfp_1900","Female_workers_1900","Male_count_1900","Male_lfp_1900","Male_workers_1900")



