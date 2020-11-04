# Started 06/26/2020
# Produces regression tables for SYP

# load libraries
library(ggplot2)
library(viridis)
library(tidyr)
library(gridExtra)
library(stargazer)
library(readxl)
library(ggfortify)
library(sandwich) # for robust standard errors
options(scipen=999) # prevent scientific notation

# load data
#source("~/git/women-postbellum/01-clean-merge-data.R")
source("~/git/women-postbellum/01-clean-map-data.R")

wctu_data_sf$year <- as.factor(wctu_data_sf$year)

### SELECT CROSS-SECTIONS
# clear data; uncomment line 69 of 01-clean-map-data.R
summary(lm(has_union ~ pct_pop_disabledx100 + state_terr, wctu_data_sf))

ggplot(wctu_data_sf) + geom_sf(aes(fill = as.factor(has_union)), size = .2, color = "black") +
  theme_void() +
  scale_fill_brewer(palette = "Paired") +
  labs(fill = "Has WCTU Union") +
  theme(legend.position = "bottom", text = element_text(size = 12))
ggsave("~/Documents/Pitt/Projects/women_civil_war/figures/original_data_4states1875.png", width = 6, height = 6)


##### Main table - percentages #####
lm1.1i <- lm(has_union ~ pct_pop_disabledx100 + state_terr, wctu_data_sf) # baseline model
robust_se1i <- sqrt(diag(vcovHC(lm1.1i, type = "HC1"))) # calculate robust se

lm1.2i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + state_terr , wctu_data_sf)
robust_se2i <- sqrt(diag(vcovHC(lm1.2i, type = "HC1")))

lm1.3i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + state_terr , wctu_data_sf)
robust_se3i <- sqrt(diag(vcovHC(lm1.3i, type = "HC1")))

lm1.4i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + state_terr , wctu_data_sf)
robust_se4i <- sqrt(diag(vcovHC(lm1.4i, type = "HC1")))

lm1.5i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + state_terr , wctu_data_sf)
robust_se5i <- sqrt(diag(vcovHC(lm1.5i, type = "HC1")))

# lm1.6i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + log_totpop + state_terr , wctu_data_sf)
# robust_se6i <- sqrt(diag(vcovHC(lm1.6i, type = "HC1")))


stargazer(type = "text",
          lm1.1i, lm1.2i, lm1.3i, lm1.4i, lm1.5i,#lm1.6i,
          df = FALSE,
          se = list(robust_se1i, robust_se2i, robust_se3i, robust_se4i, robust_se5i,robust_se6i),
          title = "SYP Table 2",
          label = "table:uavars",
          # covariate.labels = c("Percent disabled", "Percent exited regularly","Percent wounded","Percent died","Percent enlisted","log(Total Pop, 1860)"),
          # dep.var.caption = c("County has at least one local WCTU"),
          dep.var.labels = "County has at least one local WCTU",
          omit = c("state_terr"),
          omit.stat = c("f", "ser", "rsq"),
          add.lines = list(#c("Union Army percentages", rep("Yes", 5)),
            c("State F.E.", rep("Yes", 6))            )
)

# SYP Table 3: County characteristics
votes <- read_dta(file = "~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_08611/DS0001/08611-0001-Data.dta")[,c(1:3,110,115)]
names(votes) <- c("STATEICP","county_name","COUNTYICP","PctVoteRepublican1860","PresVoteTurnout1860")
votes[votes == max(votes$PctVoteRepublican1860)] <- NA # 999.9 is code for NA
votes$PresVoteTurnout1860[votes$PresVoteTurnout1860 > 100] <- NA # percentages over 100% are obviously invalid, so make NA
wctu_data_sf <- merge(x = wctu_data_sf, y = votes, by = c("STATEICP","COUNTYICP"), all.x = TRUE)

# socioeconmic/poltical controls
lm1.5ip <- lm1.5i # repeat last column of previous table
robust_se5ip <- robust_se5i

lm1.6ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + state_terr + log_totpop, wctu_data_sf)
robust_se6ip <- sqrt(diag(vcovHC(lm1.6ip, type = "HC1")))

lm1.7ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + state_terr  + log_totpop + Female_lfp_1860, wctu_data_sf)
robust_se7ip <- sqrt(diag(vcovHC(lm1.7ip, type = "HC1")))

lm1.8ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + state_terr  + log_totpop + Female_lfp_1860 + log(mfgestab + 1), wctu_data_sf)
robust_se8ip <- sqrt(diag(vcovHC(lm1.8ip, type = "HC1")))

lm1.9ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + state_terr  + log_totpop + Female_lfp_1860 + log(mfgestab + 1) + log(farmval +1), wctu_data_sf)
robust_se9ip <- sqrt(diag(vcovHC(lm1.9ip, type = "HC1")))

lm1.10ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + state_terr + log_totpop + Female_lfp_1860+ log(mfgestab + 1) + log(farmval +1) + log(urb860+1), wctu_data_sf)
robust_se10ip <- sqrt(diag(vcovHC(lm1.10ip, type = "HC1")))

lm1.11ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + state_terr + log_totpop + Female_lfp_1860+ log(mfgestab + 1)  + log(farmval +1) + log(urb860+1) + PresVoteTurnout1860, wctu_data_sf)
robust_se11ip <- sqrt(diag(vcovHC(lm1.11ip, type = "HC1")))

lm1.12ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + state_terr + log_totpop + Female_lfp_1860+ log(mfgestab + 1)  + log(farmval +1) + log(urb860+1) + PresVoteTurnout1860 + PctVoteRepublican1860, wctu_data_sf)
robust_se12ip <- sqrt(diag(vcovHC(lm1.12ip, type = "HC1")))


stargazer(#type = "text",
  lm1.5ip, lm1.6ip, lm1.7ip, lm1.8ip, lm1.9ip, lm1.10ip, lm1.11ip,lm1.12ip,
  df = FALSE,
  se = list(robust_se5ip, robust_se6ip, robust_se7ip, robust_se8ip, robust_se9ip, robust_se10ip,robust_se11ip,robust_se12ip),
  title = "SYP Table 3",
  # label = "table:econ_political_controls",
  # covariate.labels = c("Percent disabled", "log(Total population, 1860)", "log(Cash value of farms, 1860)", "log(Manufacturing capital, 1860)","Voter turnout$^\\dagger$", "Percent Republican votes$^\\dagger$"),
  dep.var.caption = c("County has at least one local WCTU"),
  # dep.var.labels = "Outcome mean in 1882, 0.59",
  omit = c("pct_pop_regoutx100", "pct_pop_woundedx100", "pct_pop_diedx100", "pct_pop_soldiersx100", "state_terr", "pct_pop_disabledx100*year"),
  omit.stat = c("f", "ser", "rsq"),
  add.lines = list(c("Union Army percentages", rep("Yes", 7)),
                   c("State F.E.", rep("Yes",7))
                   ),
  notes = "$^\\dagger$1860 Presidential Election"
)


# ##### Main table - standardized counts instead of percent disabled #####
# lm1.1i <- lm(has_union ~ scale(disabled) + state_terr, wctu_data_sf) # baseline model
# robust_se1i <- sqrt(diag(vcovHC(lm1.1i, type = "HC1"))) # calculate robust se
#
# lm1.2i <- lm(has_union ~ scale(disabled) + scale(regout) + state_terr , wctu_data_sf)
# robust_se2i <- sqrt(diag(vcovHC(lm1.2i, type = "HC1")))
#
# lm1.3i <- lm(has_union ~ scale(disabled) + scale(regout) + scale(wounded) + state_terr , wctu_data_sf)
# robust_se3i <- sqrt(diag(vcovHC(lm1.3i, type = "HC1")))
#
# lm1.4i <- lm(has_union ~ scale(disabled) + scale(regout) + scale(wounded) + scale(died) + state_terr , wctu_data_sf)
# robust_se4i <- sqrt(diag(vcovHC(lm1.4i, type = "HC1")))
#
# lm1.5i <- lm(has_union ~ scale(disabled) + scale(regout) + scale(wounded) + scale(died) + scale(total_soldiers) + state_terr , wctu_data_sf)
# robust_se5i <- sqrt(diag(vcovHC(lm1.5i, type = "HC1")))
#
# lm1.6i <- lm(has_union ~ scale(disabled) + scale(regout) + scale(wounded) + scale(died) + scale(total_soldiers) + scale(totpop) + state_terr , wctu_data_sf)
# robust_se6i <- sqrt(diag(vcovHC(lm1.6i, type = "HC1")))
#
#
# stargazer(type = "text",
#   lm1.1i, lm1.2i, lm1.3i, lm1.4i, lm1.5i,lm1.6i,
#   df = FALSE,
#   se = list(robust_se1i, robust_se2i, robust_se3i, robust_se4i, robust_se5i,robust_se6i),
#   title = "Main table",
#   label = "table:uavars",
#   # covariate.labels = c("Percent disabled", "Percent exited regularly","Percent wounded","Percent died","Percent enlisted","log(Total Pop, 1860)"),
#   # dep.var.caption = c("County has at least one local WCTU"),
#   dep.var.labels = "County has at least one local WCTU",
#   omit = c("state_terr"),
#   omit.stat = c("f", "ser", "rsq"),
#   add.lines = list(#c("Union Army percentages", rep("Yes", 5)),
#     c("State F.E.", rep("Yes", 6))            )
# )

##### pct disabled on year organized - PA only  #####
timeline <- read_excel("~/Documents/Pitt/Projects/women_civil_war/data/wctu_records/date_wctu_organized_countylevel.xlsx")
timeline$name <- toupper(timeline$county)
pa <- wctu_data_sf %>% filter(state_terr == "Pennsylvania" & year == 1880)
timeline_sf <- merge(x = pa, y = timeline, by.x = c("state_terr", "name"), by.y = c("state", "name"), all = TRUE)

timeline_sf$year_organized_round <- as.factor(round(timeline_sf$year_organized/5)*5)

ggplot(timeline_sf) + geom_sf(aes(fill = year_organized_round), size = .2, color = "black") +
  theme_void() +
  scale_fill_viridis_d(option = "magma") +
  labs(fill = "Year Organized") +
  theme(text = element_text(size = 12))

ggplot(timeline_sf) + geom_sf(aes(fill = year_organized), size = .2, color = "black") +
  theme_void() +
  scale_fill_viridis(option = "magma") +
  labs(fill = "Year Organized") +
  theme(text = element_text(size = 12))

summary(lm(year_organized ~ pct_pop_disabledx100 , timeline_sf))


##### IV #####
# "IV
# "balance" or kitchen sink reg table
wctu_data_sf$churches_per_capita <- wctu_data_sf$churches / wctu_data_sf$totpop
wctu_data_sf$pct_urb860x100 <- wctu_data_sf$pct_urb860*100

## exclusion restriciton
all <- lm(pct_pop_disabled ~ water + rail + scale(totpop) + scale(pop_per_sqmi860) + scale(urb860) + scale(urb25) +scale(homemfg) + scale(mfgestab) +scale(realest)+scale(mfgout)+scale(mfgcap) + scale(farmval) +scale(churches_per_capita) + scale(PresVoteTurnout1860) + scale(PctVoteRepublican1860)+scale(Female_lfp_1860) + scale(Male_lfp_1860) +scale(fbwtot) + scale(total_soldiers), wctu_data_sf)
all2 <- lm(has_union ~ water + rail + scale(totpop) + scale(pop_per_sqmi860) + scale(urb860) + scale(urb25) +scale(homemfg) + scale(mfgestab) +scale(realest)+scale(mfgout)+scale(mfgcap) + scale(farmval) +scale(churches_per_capita) + scale(PresVoteTurnout1860) + scale(PctVoteRepublican1860)+scale(Female_lfp_1860) + scale(Male_lfp_1860) +scale(fbwtot) + scale(total_soldiers), wctu_data_sf)
all3 <- lm(log(kmw + 1) ~ water + rail + scale(totpop) + scale(pop_per_sqmi860) + scale(urb860) + scale(urb25) +scale(homemfg) + scale(mfgestab) +scale(realest)+scale(mfgout)+scale(mfgcap) + scale(farmval) +scale(churches_per_capita) + scale(PresVoteTurnout1860) + scale(PctVoteRepublican1860)+scale(Female_lfp_1860) + scale(Male_lfp_1860) +scale(fbwtot) + scale(total_soldiers), wctu_data_sf)
all4 <- lm(mainbattlenum ~ water + rail + scale(totpop) + scale(pop_per_sqmi860) + scale(urb860) + scale(urb25) +scale(homemfg) + scale(mfgestab) +scale(realest)+scale(mfgout)+scale(mfgcap) + scale(farmval) +scale(churches_per_capita) + scale(PresVoteTurnout1860) + scale(PctVoteRepublican1860)+scale(Female_lfp_1860) + scale(Male_lfp_1860) +scale(fbwtot) + scale(total_soldiers), wctu_data_sf)
all5 <- lm(log(died_battle + 1) ~ water + rail + scale(totpop) + scale(pop_per_sqmi860) + scale(urb860) + scale(urb25) +scale(homemfg) + scale(mfgestab) +scale(realest)+scale(mfgout)+scale(mfgcap) + scale(farmval) +scale(churches_per_capita) + scale(PresVoteTurnout1860) + scale(PctVoteRepublican1860)+scale(Female_lfp_1860) + scale(Male_lfp_1860) +scale(fbwtot) + scale(total_soldiers), wctu_data_sf)

stargazer(all, all2, all3, all4, all5, #type = "text",
          df = FALSE, font.size = "small", report = "vc*", omit.stat = c("f", "ser", "rsq"),
          label = "county_chars",
          # # dep.var.labels = c("Percent disabled", "Has WCTU", "KMW","MBN"),
          # # covariate.labels = c("Churches per capita",
          # #                      "Voter turnout$^\\dagger$",
          # #                      "Percent Republican votes$^\\dagger$",
          # #                      "Female LFP",
          # #                      "Male LFP",
          # #                      "Foreign born white population",
          # #                      "Water transport access$^\\ddagger$",
          # #                      "Rail access$^\\ddagger$",
          # #                      "Total population",
          # #                      "Population per sq mile",
          # #                      "Population in places 2,500 +",
          # #                      "Population in places 25,000 +",
          # #                      "Value of domestic manufacurers",
          # #                      "Manufacturing establishments",
          # #                      "Value of real estate",
          # #                      "Value of manufacturing output",
          # #                      "Manufacturing capital",
          # #                      "Cash value of farms"),
          notes = c("$^\\dagger$1860 Presidential Election", "$^\\ddagger$Dummy variable; not scaled for interpretation by standard deviation.")
)

# Instrumental relevance and 1st stage
wctu_data_complete_cases <- wctu_data_sf %>%
  select(kmw, mainbattlenum, died_battle, pct_pop_disabledx100, pct_pop_regoutx100, pct_pop_woundedx100, pct_pop_diedx100, pct_pop_soldiersx100, PctVoteRepublican1860, PresVoteTurnout1860, log_totpop, farmval, mfgestab, urb860, Female_lfp_1860, state_terr, has_union) %>%
  na.omit

test1 <- lm(pct_pop_disabledx100 ~ log(kmw + 1) + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + state_terr + log_totpop + Female_lfp_1860+ log(mfgestab + 1)  + log(farmval +1) + log(urb860+1) + PresVoteTurnout1860 + PctVoteRepublican1860, data = wctu_data_complete_cases)
test1.1 <- lm(pct_pop_disabledx100 ~ log(kmw + 1) + state_terr, data = wctu_data_complete_cases)
test2 <- lm(pct_pop_disabledx100 ~ mainbattlenum + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + state_terr + log_totpop + Female_lfp_1860+ log(mfgestab + 1)  + log(farmval +1) + log(urb860+1) + PresVoteTurnout1860 + PctVoteRepublican1860, wctu_data_complete_cases)
test2.1 <- lm(pct_pop_disabledx100 ~ mainbattlenum + state_terr, wctu_data_complete_cases)
test3 <- lm(pct_pop_disabledx100 ~ log(died_battle + 1) + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + state_terr + log_totpop + Female_lfp_1860+ log(mfgestab + 1)  + log(farmval +1) + log(urb860+1) + PresVoteTurnout1860 + PctVoteRepublican1860, wctu_data_complete_cases)
test3.1 <- lm(pct_pop_disabledx100 ~ log(died_battle + 1) + state_terr, wctu_data_complete_cases)

stargazer(test1,test1.1, test2,test2.1, test3,test3.1, #type = "text",
          omit = c("pct_pop_regoutx100", "pct_pop_woundedx100", "pct_pop_diedx100", "pct_pop_soldiersx100", "state_terr", "log_totpop","mfgestab", "farmval","Female_lfp_1860","urb860","PresVoteTurnout1860","PctVoteRepublican1860"),
          omit.stat = c("ser", "rsq"),
          df = FALSE,
          add.lines = list(c("Union Army percentages", "Yes","No", "Yes","No", "Yes","No"),
                           c("County sociopolitical controls", "Yes","No", "Yes","No", "Yes","No"),
                           c("State F.E.", rep("Yes",6))
          )

)

# 2nd stage
d.hat.kmw <- test1$fitted.values
d.hat.mbn <- test2$fitted.values
# d.hat.died <- test3$fitted.values

tsls2.kmw <- lm(has_union ~ d.hat.kmw + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + state_terr + log_totpop + Female_lfp_1860+ log(mfgestab + 1)  + log(farmval +1) + log(urb860+1) + PresVoteTurnout1860 + PctVoteRepublican1860, data = wctu_data_complete_cases)
tsls2.mbn <- lm(has_union ~ d.hat.mbn + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + state_terr + log_totpop + Female_lfp_1860+ log(mfgestab + 1)  + log(farmval +1) + log(urb860+1) + PresVoteTurnout1860 + PctVoteRepublican1860, data = wctu_data_complete_cases)
# tsls2.died <- lm(has_union ~ d.hat.died + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + state_terr + log_totpop + Female_lfp_1860+ log(mfgestab + 1)  + log(farmval +1) + log(urb860+1) + PresVoteTurnout1860 + PctVoteRepublican1860, data = wctu_data_complete_cases)

stargazer(tsls2.kmw, tsls2.mbn, #type = "text",
          omit = c("pct_pop_regoutx100", "pct_pop_woundedx100", "pct_pop_diedx100", "pct_pop_soldiersx100", "state_terr", "log_totpop","mfgestab", "farmval","Female_lfp_1860","urb860","PresVoteTurnout1860","PctVoteRepublican1860"),
          omit.stat = c("ser", "rsq"),
          add.lines = list(c("Union Army percentages", rep("Yes", 7)),
                           c("County sociopolitical controls", rep("Yes", 3)),
                           c("State F.E.", rep("Yes",7))))


