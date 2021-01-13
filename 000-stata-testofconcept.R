library(haven)
library(dplyr)
library(tidyr)
library(stargazer)
options(scipen=999) # prevent scientific notation
union_states <- c("Maine", "New Hampshire", "Vermont", "New York", "Massachusetts", "Rhode Island", "Connecticut", "Pennsylvania", "New Jersey", "Ohio", "Indiana", "Illinois", "Iowa", "Wisconsin", "Minnesota", "Michigan") # only core states: exluces CA, WA, KS, and boundary states

##### women's lfp graphic ####
library(ggplot2)
library(dplyr)
source("~/git/women-postbellum/01-clean-ipums-data.R")
ipums_long$region <- ifelse(ipums_long$STATEICP %in% c(23, 14, 22, 24, 2, 4, 6, 13, 5, 1, 12, 21, 31, 25, 33), yes = "North", no = "South")
data <- ipums_long %>% group_by(region, YEAR, SEX) %>% summarise(totpop = sum(count, na.rm = TRUE), workers = sum(workers, na.rm = TRUE))
data$lfp <- data$workers / data$totpop

ggplot(data %>% filter (SEX == 2), aes(x = YEAR, y = lfp, color = as.factor(region), group = region)) + geom_point() + geom_line()


#### state test of concept ####

census_ua <- read_dta("~/Documents/Pitt/Projects/women_civil_war/data/matched_1860-80_soldierhouseholds_union_clean.dta")

census_ua$solider <- as.numeric(!is.na(census_ua$pidsoldier))

latex1 <- lm(disabled ~ works + wounded + deserted, census_ua)
latex2 <- lm(works ~ disabled + wounded + deserted, census_ua)
latex3 <- lm(works ~ disabled + wounded + deserted, census_ua)
latex4 <- lm(works ~ disabled + wounded + deserted, census_ua)

stargazer(latex4, latex2, latex3, latex4)
table(census_ua$general_relation, census_ua$solider) # the vast majority of soldiers (82,182) are household heads; also 5k are sons, hundreds in other categories

table(census_ua$self_empty_info_maritalstatus, census_ua$solider) # 81k soldiers are married

# if the soldier is a widower... ARE DISABLED GUYS MORE LIKELY TO BECOME WIDOWERS by 1880? COULD THIS BE A DOMESTIC VIOLENCE SIGNAL???
test <- census_ua %>% filter(solider == 1)
test$widower <- ifelse(test$self_empty_info_maritalstatus == "Widower", yes = 1, no = 0)
t.test(test$widower~test$disabled)
summary(lm(widower ~ disabled + age_i + as.factor(edid), test))

# so I should subset for soldiers who are married and head of household; regress wives' work status on disability.
# I can do a separate regression for the 5k sons to see if ANY woman household member is likely to join the workforce.

data <- census_ua %>%
    filter(disabled == 1 | regout == 1) %>%
    gather(exit_status, value, disabled, regout) %>%
    filter(value == 1)

data$general_relation <- as.factor(data$general_relation)
levels(data$general_relation)

data$exit_status <- factor(data$exit_status, levels = c("regout", "disabled"))
levels(data$exit_status)

summary(lm(works ~ general_relation, data %>% filter(veteran_hoh == 1)))

summary(lm(works ~ exit_status + as.factor(countyid), data %>% filter(veteran_hoh == 1 & general_relation %in% c("Wife", "Dau", "Mother", "Niece", "Sister")))) # relative to female family members of regout soldiers, wives, daughters, mothers, nieces, and sisters of diabled soldiers are 0.8pp more likely to work.

summary(lm(works ~ exit_status + as.factor(countyid), data %>% filter(veteran_hoh == 1 & general_relation %in% c("Dau")))) # relative to female family members of regout soldiers, wives, daughters, mothers, nieces, and sisters of diabled soldiers are 0.8pp more likely to work.

census_ua$white <- ifelse(census_ua$self_empty_info_race == "White", yes = 1, no = 0)
census_ua$enlisted <- ifelse(census_ua$inmethod == "Enlisted", yes = 1, no = 0)
census_ua$commissioned <- ifelse(census_ua$inmethod == "Commission", yes = 1, no = 0)
census_ua$drafted <- ifelse(census_ua$inmethod == "Drafted", yes = 1, no = 0)
census_ua$private <- ifelse(census_ua$enlrank == "Priv", yes = 1, no = 0)
census_ua$corporal <- as.numeric(grepl("Corpl", census_ua$enlrank, fixed = TRUE))
census_ua$sergeant <- as.numeric(grepl("Sergt", census_ua$enlrank, fixed = TRUE))
census_ua$sergeant[census_ua$enlrank == "Sergeant"] <- 1
census_ua$lieutenant <- as.numeric(grepl("Lieut", census_ua$enlrank, fixed = TRUE))
census_ua$captain <- ifelse(census_ua$enlrank == "Captain", 1, 0)
census_ua$indate2 <- as.Date(census_ua$indate2, "%Y-%m-%d")
census_ua$married <- ifelse(census_ua$self_empty_info_maritalstatus == "Married", 1, 0)


census_ua %>%
  filter(!is.na(pidsoldier)) %>%
  # filter(veteran_hoh == 1 & wife == 1 & age < 41 & age > 15 & exit_status_d>0 & inmethod %in% c("Commission","Enlisted","Drafted")) %>%
  select(enlage, veteran_hoh, hhsize, occscore, white, married, wife_works, enlisted, commissioned, drafted, private, corporal, sergeant, lieutenant, captain, disabled, wounded, regout, deserted, fought, foughtsum) %>%
  as.data.frame() %>%
  stargazer(#type = "text",
    summary.stat = c("n", "mean", "sd", "min", "max"),
    label = "tab:ua_summary_stats",
    title = "Summary Statistics",
    digits = 2,
    covariate.labels = c("Age at enlistment",
                         "Head of household",
                         "Household size",
                         "Occupational score",
                         "White",
                         "Married",
                         "Wife works in 1880",
                         "\\hspace{.5cm}Enlisted",
                         "\\hspace{.5cm}Commissioned",
                         "\\hspace{.5cm}Drafted",
                         "\\hspace{.5cm}Private",
                         "\\hspace{.5cm}Corporal",
                         "\\hspace{.5cm}Sergeant",
                         "\\hspace{.5cm}Lieutenant",
                         "\\hspace{.5cm}Captain",
                         "\\hspace{.5cm}Disabled",
                         "\\hspace{.5cm}Wounded",
                         "\\hspace{.5cm}Regular exit",
                         "\\hspace{.5cm}Deserted"
    )
  )

star <-
  census_ua %>%
  select(enlage,  indate2, enlrank) %>%
  group_by(State) %>%
  mutate(id = 1:n()) %>%
  ungroup() %>%
  gather(temp, val, has_union) %>% # pct_pop_disabled, pct_pop_wounded, pct_pop_regout, pct_pop_soldiers,pct_pop_died,
  unite(temp1, temp, State, sep = '_') %>%
  spread(temp1, val) %>%
  select(-id) %>%
  as.data.frame() %>%
  stargazer(#type = "text",
    summary.stat = c("n", "mean", "min", "p25", "median", "p75", "max"),
    label = "tab:wctu_summary_stats",
    title = "Summary Statistics",
    digits = 2,
    covariate.labels = c("Percent wounded",
                         "Percent exited regularly",
                         "Percent enlisted",
                         "Percent died",
                         "Percent disabled",
                         "\\hspace{.5cm}Indiana, 1875",
                         "\\hspace{.5cm}Michigan, 1882",
                         "\\hspace{.5cm}Ohio, 1875",
                         "\\hspace{.5cm}Pennsylvania, 1880"
    )
  )
# star <- star[c(1:5,15:18,10,7,6,9,8)]
cat(star, sep = "\n")
