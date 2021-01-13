# Started 06/26/2020
# Produces regression tables for SYP

# load libraries
library(ggplot2)
library(viridis)
library(tidyr)
library(ggeffects)
library(gridExtra)
library(stargazer)
library(readxl)
library(ggfortify)
library(sandwich) # for robust standard errors
library(AER)

library(plm)
library(lmtest)
library(multiwayvcov)
options(scipen=999) # prevent scientific notation

# load data
source("~/git/women-postbellum/01-clean-merge-data.R")
# source("~/git/women-postbellum/01-clean-map-data.R")

#### Summary Statistics ####
star <-
  data %>%
  filter(State %in% c("Indiana","Ohio","Pennsylvania","Michigan")) %>%
  select(State,  pct_pop_woundedx100, pct_pop_regoutx100, pct_pop_soldiersx100,pct_pop_diedx100,pct_pop_disabledx100, has_union) %>%
  group_by(State) %>%
  mutate(id = 1:n()) %>%
  ungroup() %>%
  gather(temp, val, has_union) %>% # pct_pop_disabled, pct_pop_wounded, pct_pop_regout, pct_pop_soldiers,pct_pop_died,
  unite(temp1, temp, State, sep = '_') %>%
  spread(temp1, val) %>%
  select(-id) %>%
  as.data.frame() %>%
  stargazer(#type = "text",
            summary.stat = c("n", "mean", "sd", "min", "max"),
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



##### Main table - percentages #####
lm1.1i <- lm(has_union ~ pct_pop_disabledx100 + State, data) # baseline model
robust_se1i <- sqrt(diag(vcovHC(lm1.1i, type = "HC1"))) # calculate robust se

lm1.2i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + State , data)
robust_se2i <- sqrt(diag(vcovHC(lm1.2i, type = "HC1")))

lm1.3i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + State , data)
robust_se3i <- sqrt(diag(vcovHC(lm1.3i, type = "HC1")))

lm1.4i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + State , data)
robust_se4i <- sqrt(diag(vcovHC(lm1.4i, type = "HC1")))

lm1.5i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + State , data)
robust_se5i <- sqrt(diag(vcovHC(lm1.5i, type = "HC1")))

# lm1.6i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + log_totpop + State , data)
# robust_se6i <- sqrt(diag(vcovHC(lm1.6i, type = "HC1")))

stargazer(type = "text",
          lm1.1i, lm1.2i, lm1.3i, lm1.4i, lm1.5i,#lm1.6i,
          df = FALSE,
          se = list(robust_se1i, robust_se2i, robust_se3i, robust_se4i, robust_se5i),
          title = "Union Army population percentages and the probability a county has a WCTU.",
          label = "table:uavars",
          covariate.labels = c("Percent disabled", "Percent exited regularly","Percent wounded","Percent died","Percent enlisted"),
          # dep.var.caption = c("County has at least one local WCTU"),
          dep.var.labels = "County has at least one local WCTU",
          omit = c("State"),
          omit.stat = c("f", "ser", "rsq"),
          add.lines = list(#c("Union Army percentages", rep("Yes", 5)),
            c("State F.E.", rep("Yes", 6))            )

)

# SYP Table 3: County characteristics


# socioeconmic/poltical controls
lm1.5ip <- lm1.5i # repeat last column of previous table
robust_se5ip <- robust_se5i

lm1.6ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + State + log_totpop, data)
robust_se6ip <- sqrt(diag(vcovHC(lm1.6ip, type = "HC1")))

lm1.7ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + State  + log_totpop + Female_lfp_1860, data)
robust_se7ip <- sqrt(diag(vcovHC(lm1.7ip, type = "HC1")))

lm1.8ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + State  + log_totpop + Female_lfp_1860 + log(mfgestab+1), data)
robust_se8ip <- sqrt(diag(vcovHC(lm1.8ip, type = "HC1")))

lm1.9ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + State  + log_totpop + Female_lfp_1860  + log(mfgestab+1) + log(farmval+1), data)
robust_se9ip <- sqrt(diag(vcovHC(lm1.9ip, type = "HC1")))

lm1.10ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + State + log_totpop + Female_lfp_1860  + log(mfgestab+1) + log(farmval+1) + log(urb860+1), data)
robust_se10ip <- sqrt(diag(vcovHC(lm1.10ip, type = "HC1")))

# lm1.11ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + State + log_totpop + log(urb25 +1) + log(mfgestab+1) + log(mfglabf +1) + log(persest+1) + log(realest+1) + log(acimp+1) + PresVoteTurnout1860 + PctVoteRepublican1860, data)
lm1.11ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + State + log_totpop + Female_lfp_1860  + log(mfgestab+1) + log(farmval+1) + log(urb860+1) + PresVoteTurnout1860, data)
robust_se11ip <- sqrt(diag(vcovHC(lm1.11ip, type = "HC1")))

lm1.12ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + State + log_totpop + Female_lfp_1860  + log(mfgestab+1) + log(farmval+1) + log(urb860+1) + PresVoteTurnout1860 + PctVoteRepublican1860, data)
robust_se12ip <- sqrt(diag(vcovHC(lm1.12ip, type = "HC1")))


stargazer(type = "text",
  lm1.5ip, lm1.6ip, lm1.7ip, lm1.8ip, lm1.9ip, lm1.10ip, lm1.11ip,lm1.12ip,
  df = FALSE,
  se = list(robust_se5ip, robust_se6ip, robust_se7ip, robust_se8ip, robust_se9ip, robust_se10ip, robust_se11ip,robust_se12ip),
  title = "SYP Table 3",
  # label = "table:econ_political_controls",
  # covariate.labels = c("Percent disabled", "log(Total population, 1860)", "log(Cash value of farms, 1860)", "log(Manufacturing capital, 1860)","Voter turnout$^\\dagger$", "Percent Republican votes$^\\dagger$"),
  dep.var.caption = c("County has at least one local WCTU"),
  # dep.var.labels = "Outcome mean in 1882, 0.59",
  # omit = c("pct_pop_regoutx100", "pct_pop_woundedx100", "pct_pop_diedx100", "pct_pop_soldiersx100", "State", "pct_pop_disabledx100*year"),
  omit.stat = c("f", "ser", "rsq"),
  add.lines = list(c("Union Army percentages", rep("Yes", 8)),
                   c("State F.E.", rep("Yes",8))
                   ),
  notes = "$^\\dagger$1860 Presidential Election"
)

ggplot(data = data %>% filter(State %in% c("Indiana","Ohio","Pennsylvania","Michigan")), aes(x = PctVoteRepublican1860, y = pct_pop_disabledx100, color = as.factor(has_union))) + geom_point() + geom_smooth(method = "lm")
ggplot(data = data, aes(x = PctVoteRepublican1860, y = pct_pop_disabledx100)) + geom_point() + geom_smooth(method = "loess")
ggplot(data = data %>% filter(State %in% c("Indiana","Ohio","Pennsylvania","Michigan")), aes(x = PctVoteRepublican1860, color = as.factor(mainbattlenum_discrete))) + geom_density()

# ##### pct disabled on year organized - PA only  #####
# timeline <- read_excel("~/Documents/Pitt/Projects/women_civil_war/data/wctu_records/date_wctu_organized_countylevel.xlsx")
# timeline$name <- toupper(timeline$county)
# pa <- data %>% filter(State == "Pennsylvania" & year == 1880)
# timeline_sf <- merge(x = pa, y = timeline, by.x = c("State", "name"), by.y = c("state", "name"), all = TRUE)
#
# timeline_sf$year_organized_round <- as.factor(round(timeline_sf$year_organized/5)*5)
#
# ggplot(timeline_sf) + geom_sf(aes(fill = year_organized_round), size = .2, color = "black") +
#   theme_void() +
#   scale_fill_viridis_d(option = "magma") +
#   labs(fill = "Year Organized") +
#   theme(text = element_text(size = 12))
#
# ggplot(timeline_sf) + geom_sf(aes(fill = year_organized), size = .2, color = "black") +
#   theme_void() +
#   scale_fill_viridis(option = "magma") +
#   labs(fill = "Year Organized") +
#   theme(text = element_text(size = 12))
#
# summary(lm(year_organized ~ pct_pop_disabledx100 , timeline_sf))



