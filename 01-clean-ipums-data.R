# temperance project
# cleaning ipums data
# created 6/19/2020

library(dplyr) # data manipulation
library(tidyr) # wide to long
library(vroom) # import large csv

options(scipen=999) # prevent R from storing small numbers in scientific notation (for small LFPs, etc.)

### LINES 13-43 TAKE A LONG TIME TO RUN: THE OUTPUT IS SAVED AND LOADED IN ON LINE 44 TO SAVE TIME. TO MAKE CHANGES TO THE ORIGINAL STRUCTURE OF ipums_long, UNCOMMENT AND RUN AGAIN.

# ipums1 <- vroom::vroom(file = "Documents/Pitt/Data/ipums/usa_00006.csv", delim = ",") # 1860, 1870
# 
# ipums60.70 <- 
#     ipums1 %>% # begin with raw ipums data for 1860 and 1870
#     filter(AGE > 10 & AGE < 70) %>% # EXPLAIN WHY OR CHANGE
#     group_by(STATEICP, COUNTYICP, YEAR, SEX) %>% summarise(count = n(),
#                                                            workers = sum(LABFORCE == 2, na.rm = TRUE),
#                                                            lfp = workers/count) %>% 
#     ungroup()
# 
# rm(ipums1)
# 
# ipums2 <- vroom::vroom(file = "Documents/Pitt/Data/ipums/usa_00007.csv", delim = ",") # 1880, 1900
# 
# ipums80.00 <- ipums2 %>% 
#     filter(AGE > 10 & AGE < 70) %>% # EXPLAIN WHY OR CHANGE
#     group_by(STATEICP, COUNTYICP, YEAR, SEX) %>% summarise(count = n(),
#                                                            workers = sum(LABFORCE == 2, na.rm = TRUE),
#                                                            lfp = workers/count) %>% 
#     ungroup()
# rm(ipums2)
# 
# ipums80.00 <- ipums80.00 %>% filter(YEAR == 1880) # EXPLAIN WHY OR CHANGE
# ipums_long <- rbind(ipums60.70, ipums80.00)
# rm(list = c("ipums60.70", "ipums80.00"))
# 
# # create wide format
# ipums_long$SEX[ipums$SEX == 1] <- "Male"
# ipums_long$SEX[ipums$SEX == 2] <- "Female"
# 
# write.csv(ipums_long, "Documents/Pitt/Data/ipums/fullcnt18601880.csv")
ipums_long <- read.csv("Documents/Pitt/Data/ipums/fullcnt18601880.csv")[-1]


ipums_wide <- 
    ipums_long %>%
    gather(variable, value, -(STATEICP:SEX)) %>%
    unite(temp, SEX, variable) %>%
    spread(temp, value)

ipums_wide <- 
    ipums_wide %>%
    gather(variable, value, -(STATEICP:YEAR)) %>%
    unite(temp, YEAR, variable) %>%
    spread(temp, value)



