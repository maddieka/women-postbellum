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

wctu_data_sf$year1882 <- ifelse(test = wctu_data_sf$year == 1882, yes = 1, no = 0)
wctu_data_sf$year1890 <- ifelse(test = wctu_data_sf$year == 1890, yes = 1, no = 0)
wctu_data_sf$year1895 <- ifelse(test = wctu_data_sf$year == 1895, yes = 1, no = 0)
wctu_data_sf$year1896 <- ifelse(test = wctu_data_sf$year == 1896, yes = 1, no = 0)
wctu_data_sf$year1898 <- ifelse(test = wctu_data_sf$year == 1898, yes = 1, no = 0)

wctu_data_sf$year <- as.factor(wctu_data_sf$year)
wctu_data_sf$log_mbshp <- log(wctu_data_sf$estimated_membership + 1)
wctu_data_sf$Total_count_1860 <- wctu_data_sf$Female_count_1860 + wctu_data_sf$Male_count_1860
wctu_data_sf$log_farmval <- log(wctu_data_sf$farmval + 1)
wctu_data_sf$log_mfgcap <- log(wctu_data_sf$mfgcap + 1)
# wctu_data_sf$mbmshp_per10k <- NA
# wctu_data_sf$denom <- NA

# summary stats table
wctu_data$has_union <- ifelse(wctu_data$count_unions > 0, yes = 1, no = 0)

star <-
  wctu_data_sf %>%
  select(year, count_unions, estimated_membership,  pct_pop_wounded, pct_pop_regout, pct_pop_soldiers,pct_pop_died,pct_pop_disabled, has_union) %>%
  group_by(year) %>%
  mutate(id = 1:n()) %>%
  ungroup() %>%
  gather(temp, val, count_unions, pct_pop_disabled, pct_pop_wounded, pct_pop_regout, pct_pop_soldiers,pct_pop_died, estimated_membership, has_union) %>%
  unite(temp1, temp, year, sep = '_') %>%
  spread(temp1, val) %>%
  select(-id) %>%
  as.data.frame() %>%
  stargazer(summary.stat = c("n", "mean", "sd", "min", "median", "max"),
            label = "tab:wctu_summary_stats",
            title = "Summary Statistics",
            #digits = 2,
            covariate.labels = c("Count unions, 1882","\\hspace{.5cm}1890","\\hspace{.5cm}1895","\\hspace{.5cm}1896","\\hspace{.5cm}1898",
                                 "Estimated membership, 1882","\\hspace{.5cm}1890","\\hspace{.5cm}1895","\\hspace{.5cm}1896","\\hspace{.5cm}1898",
                                 "Has a union, 1882","\\hspace{.5cm}1890","\\hspace{.5cm}1895","\\hspace{.5cm}1896","\\hspace{.5cm}1898",
                                 "Percent died","\\hspace{.5cm}1890","\\hspace{.5cm}1895","\\hspace{.5cm}1896","\\hspace{.5cm}1898",
                                 "Percent disabled","\\hspace{.5cm}1890","\\hspace{.5cm}1895","\\hspace{.5cm}1896","\\hspace{.5cm}1898",
                                 "Percent exited regularly","\\hspace{.5cm}1890","\\hspace{.5cm}1895","\\hspace{.5cm}1896","\\hspace{.5cm}1898",
                                 "Percent enlisted","\\hspace{.5cm}1890","\\hspace{.5cm}1895","\\hspace{.5cm}1896","\\hspace{.5cm}1898",
                                 "Percent wounded","\\hspace{.5cm}1890","\\hspace{.5cm}1895","\\hspace{.5cm}1896","\\hspace{.5cm}1898")
  )
star <- gsub(x = star, pattern = ".000", replacement = "")
star <- star[c(1:26,32,27,47,42,37,52:54)]
cat(star, sep = "\n")

# wctu_data %>%
#   select(year, count_unions, estimated_membership, total_dues, has_union) %>%
#   group_by(year) %>%
#   mutate(id = 1:n()) %>%
#   ungroup() %>%
#   gather(temp, val, count_unions, total_dues, estimated_membership, has_union) %>%
#   unite(temp1, temp, year, sep = '_') %>%
#   spread(temp1, val) %>%
#   select(-id) %>%
#   as.data.frame() %>%
#   stargazer(type = "text",
#             summary.stat = c("n", "mean", "sd", "min", "median", "max"),
#             label = "tab:wctu_summary_stats",
#             title = "Summary Statistics for the Annual Treasurer's Reports of the Michigan WCTU",
#             covariate.labels = c("Count unions, 1882","\\hspace{.5cm}1890","\\hspace{.5cm}1895","\\hspace{.5cm}1896","\\hspace{.5cm}1898",
#                                  "Estimated membership, 1882","\\hspace{.5cm}1890","\\hspace{.5cm}1895","\\hspace{.5cm}1896","\\hspace{.5cm}1898",
#                                  "Has a union, 1882","\\hspace{.5cm}1890","\\hspace{.5cm}1895","\\hspace{.5cm}1896","\\hspace{.5cm}1898",
#                                  "Dues collected, 1882","\\hspace{.5cm}1890","\\hspace{.5cm}1895","\\hspace{.5cm}1896","\\hspace{.5cm}1898")
#   )

mean(wctu_data_sf$has_union[wctu_data_sf$year == 1882], na.rm = TRUE)
ggplot(wctu_data_sf %>% filter(!is.na(year))) + geom_sf(aes(fill = as.factor(has_union)), size = .2, color = "black") + facet_wrap(~ year, nrow = 1) + theme_void() + scale_fill_brewer(palette = "Paired") + labs(fill = "Has WCTU Union",subtitle = "") + theme(legend.position = "bottom", text = element_text(size = 12))
ggsave("~/Documents/Pitt/Projects/women_civil_war/figures/original_data.png", width = 8, height = 3)

# Main table ####
lm1.1i <- lm(has_union ~ pct_pop_disabledx100 + year , wctu_data_sf) # baseline model
robust_se1i <- sqrt(diag(vcovHC(lm1.1i, type = "HC1"))) # calculate robust se

lm1.2i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + year , wctu_data_sf)
robust_se2i <- sqrt(diag(vcovHC(lm1.2i, type = "HC1")))

lm1.3i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + year , wctu_data_sf)
robust_se3i <- sqrt(diag(vcovHC(lm1.3i, type = "HC1")))

lm1.4i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + year , wctu_data_sf)
robust_se4i <- sqrt(diag(vcovHC(lm1.4i, type = "HC1")))

lm1.5i <- lm(has_union ~ pct_pop_disabledx100 + year , wctu_data_sf)
robust_se5i <- sqrt(diag(vcovHC(lm1.5i, type = "HC1")))

stargazer(type = "text",
          lm1.1i, lm1.2i, lm1.3i, lm1.4i, lm1.5i,
          df = FALSE,
          se = list(robust_se1i, robust_se2i, robust_se3i, robust_se4i, robust_se5i),
          title = "Main table",
          label = "table:uavars",
          covariate.labels = c("Percent disabled", "Percent exited regularly","Percent wounded","Percent died","Percent enlisted"),
          dep.var.caption = c("County has at least one local WCTU"),
          dep.var.labels = "Outcome mean in 1882, 0.59",
          #omit = c("year"),
          omit.stat = c("f", "ser", "rsq"),
          add.lines = list(#c("Union Army percentages", rep("Yes", 5)),
            c("Year F.E.", rep("Yes", 5))            )
)

# main table --  playing ####
lm1.1i <- lm(has_union ~ pct_pop_disabledx100 + year, wctu_data_sf) # baseline model
robust_se1i <- sqrt(diag(vcovHC(lm1.1i, type = "HC1"))) # calculate robust se

lm1.2i <- lm(has_union ~  pct_pop_regoutx100 + year , wctu_data_sf)
robust_se2i <- sqrt(diag(vcovHC(lm1.2i, type = "HC1")))

lm1.3i <- lm(has_union ~ pct_pop_woundedx100 + year , wctu_data_sf)
robust_se3i <- sqrt(diag(vcovHC(lm1.3i, type = "HC1")))

lm1.4i <- lm(has_union ~ pct_pop_diedx100 + year , wctu_data_sf)
robust_se4i <- sqrt(diag(vcovHC(lm1.4i, type = "HC1")))

lm1.5i <- lm(has_union ~ pct_pop_soldiersx100 + year , wctu_data_sf)
robust_se5i <- sqrt(diag(vcovHC(lm1.5i, type = "HC1")))

lm1.6i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_woundedx100 + year , wctu_data_sf)
robust_se5i <- sqrt(diag(vcovHC(lm1.5i, type = "HC1")))

stargazer(#type = "text",
          lm1.1i, lm1.2i, lm1.3i, lm1.4i, lm1.5i,lm1.6i,
          df = FALSE,
          se = list(robust_se1i, robust_se2i, robust_se3i, robust_se4i, robust_se5i),
          title = "Main table",
          label = "table:uavars",
          covariate.labels = c("Percent disabled", "Percent exited regularly","Percent wounded","Percent died","Percent enlisted"),
          # dep.var.caption = c("County has at least one local WCTU"),
          dep.var.labels = "County has at least one local WCTU",
          omit = c("year"),
          omit.stat = c("f", "ser", "rsq"),
          add.lines = list(
            # c("Union Army percentages", rep("Yes", 5)),
            c("Year F.E.", rep("Yes", 6))
            )
)

par(mfrow = c(2, 2))
plot(lm1.1i)
# library(sjPlot)
# library(sjlabelled)
# library(sjmisc)
# plot_model(lm1.5ic,terms =  c("year [1890,1895,1896,1898]", "pct_pop_disabledx100:year [1890,1895,1896,1898]"))
# lm1.5ic$coefficients
#
library(coefplot2)
coefplot2(lm1.5i, vertical = FALSE)
labels <- c("Percent disabled", "Percent exited regularly","Percent wounded","Percent died","Percent enlisted","1890","1895","1896","1898","Disabled x 1890","Disabled x 1895","Disabled x 1896","Disabled x 1898")
coefplot2(lm1.5i, varnames = labels, main = "")

library(broom)
coef <- tidy(lm1.5i, conf.int = TRUE)
# coef <- coef %>% filter(term %in% c("year1890","year1895","year1896","year1898","pct_pop_disabledx100:year1890", "pct_pop_disabledx100:year1895", "pct_pop_disabledx100:year1896", "pct_pop_disabledx100:year1898"))
# coef$year <- stringr::str_sub(gsub("[^0-9.-]", "", coef$term), start = -4, end = -1)
# coef$type <- ifelse(test = str_detect(coef$term, pattern = ":"), yes = "year x disabled", no = "year FE")
coef <- coef[-1, ] # get rid of the intercept
coef$labels <- labels
coef$type <- factor(c(rep("Union Army main effect", 5), rep("Year main effect",4), rep("Interaction effect",4)), levels = c("Year main effect", "Interaction effect", "Union Army main effect"))

ggplot(data = coef, aes(x = labels, y =  estimate))+
  geom_point(aes(color = type))+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, color = type))+
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = "", x = "", y = "", color = "") +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90), legend.position="top", text = element_text(size=10))
# scale_color_brewer(palette = "Paired") +
ggsave("~/Documents/Pitt/Projects/women_civil_war/figures/main_effect_plot.png", width = 7, height = 5)

# political/economic controls table; include year F.E., interaction terms; stagger log_totpop, economic stuff, and political stuff
votes <- read_dta(file = "~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_08611/DS0001/08611-0001-Data.dta")[,c(1:3,110,115)]
names(votes) <- c("STATEICP","county_name","COUNTYICP","PctVoteRepublican1860","PresVoteTurnout1860")
votes[votes == max(votes$PctVoteRepublican1860)] <- NA # 999.9 is code for NA
votes$PresVoteTurnout1860[votes$PresVoteTurnout1860 > 100] <- NA # percentages over 100% are obviously invalid, so make NA
wctu_data_sf <- merge(x = wctu_data_sf, y = votes, by = c("STATEICP","COUNTYICP"), all.x = TRUE)

wctu_data_sf$Male_lfp_1860x100 <- wctu_data_sf$Male_lfp_1860*100
wctu_data_sf$Female_lfp_1860x100 <- wctu_data_sf$Female_lfp_1860*100

# socioeconmic/poltical controls
lm1.5ip <- lm1.1i # repeat last column of previous table
robust_se5ip <- robust_se1i

lm1.6ip <- lm(has_union ~ pct_pop_disabledx100 + year  + log_totpop, wctu_data_sf)
robust_se6ip <- sqrt(diag(vcovHC(lm1.6ip, type = "HC1")))

lm1.7ip <- lm(has_union ~ pct_pop_disabledx100  + year + log_totpop + Male_lfp_1860x100, wctu_data_sf)
robust_se7ip <- sqrt(diag(vcovHC(lm1.7ip, type = "HC1")))

lm1.75ip <- lm(has_union ~ pct_pop_disabledx100  + year + log_totpop + Male_lfp_1860x100 + Female_lfp_1860x100, wctu_data_sf)
robust_se7ip <- sqrt(diag(vcovHC(lm1.75ip, type = "HC1")))

lm1.8ip <- lm(has_union ~ pct_pop_disabledx100  + year + log_totpop + Male_lfp_1860x100 + Female_lfp_1860x100+ log(mfgestab+1), wctu_data_sf)
robust_se8ip <- sqrt(diag(vcovHC(lm1.8ip, type = "HC1")))

lm1.9ip <- lm(has_union ~ pct_pop_disabledx100 +year  + log_totpop + Male_lfp_1860x100 + Female_lfp_1860x100+ log(mfgestab+1) + PresVoteTurnout1860, wctu_data_sf)
robust_se9ip <- sqrt(diag(vcovHC(lm1.9ip, type = "HC1")))

lm1.10ip <- lm(has_union ~ pct_pop_disabledx100 +year  + log_totpop + Male_lfp_1860x100 + Female_lfp_1860x100+ log(mfgestab+1) + PresVoteTurnout1860 + PctVoteRepublican1860, wctu_data_sf)
robust_se10ip <- sqrt(diag(vcovHC(lm1.10ip, type = "HC1")))

stargazer(#type = "text",
  lm1.5ip, lm1.6ip, lm1.7ip, lm1.75ip, lm1.8ip, lm1.9ip, lm1.10ip, #lm1.11ip,
  df = FALSE,
  ci = TRUE,
  se = list(robust_se5ip, robust_se6ip, robust_se7ip, robust_se8ip, robust_se9ip, robust_se10ip),
  title = "Socioeconomic/political controls",
  label = "table:econ_political_controls",
  covariate.labels = c("Percent disabled", "log(Total population, 1860)", "Male LFP, 1860", "Female LFP, 1860","log(Count manufacturers, 1860)","Voter turnout$^\\dagger$", "Percent Republican votes$^\\dagger$"),
  # covariate.labels = c("Percent disabled","1890","1895","1896","1898", "log(Total population, 1860)", "Male LFP, 1860","log(Count manufacturers, 1860)","Voter turnout$^\\dagger$", "Percent Republican votes$^\\dagger$"),
  dep.var.labels = "County has at least one local WCTU",
  omit = "year",
  omit.stat = c("f", "ser", "rsq"),
  add.lines = list(c("Union Army percentages", rep("No", 6)),
                   c("Year F.E.", rep("Yes",6))
  ),
  notes = "$^\\dagger$1860 Presidential Election"
)


# 1] "STATEICP"               "COUNTYICP"              "state_terr"             "name"                   "id_num"
# [6] "id"                     "fips.x"                 "version"                "start_date"             "end_date"
# [11] "change"                 "citation"               "start_n"                "end_n"                  "area_sqmi"
# [16] "cnty_type"              "full_name"              "cross_ref"              "name_start"             "state_abbr"
# [21] "state_name"             "state_code"             "STATEFIPS"              "County"                 "kmw"
# [26] "mainbattlenum"          "loss"                   "disabled_battle"        "wounded_battle"         "mainbattle"
# [31] "died_battle"            "died_wounds"            "died_disease"           "died"                   "regout"
# [36] "disabled"               "wounded"                "deserted"               "totpop"                 "urb860"
# [41] "urb25"                  "wmtot"                  "wftot"                  "farmval"                "homemfg"
# [46] "mfgestab"               "mfgcap"                 "mfglabm"                "mfglabf"                "level"
# [51] "fips.y"                 "statefip"               "equipval"               "homemfg.1"              "farm39"
# [56] "farm1019"               "farm2049"               "farm5099"               "farm100"                "farm500"
# [61] "farm1000"               "mfgestab.1"             "mfgcap.1"               "mfgout"                 "realest"
# [66] "churches"               "water"                  "rail"                   "fbwtot"                 "mfglabf.1"
# [71] "quaker"                 "quakacc"                "germref"                "germracc"               "shaker"
# [76] "shakacc"                "1850_Female_count"      "Female_lfp_1850"        "Female_workers_1850"    "Male_count_1850"
# [81] "Male_lfp_1850"          "Male_workers_1850"      "Female_count_1860"      "Female_lfp_1860"        "Female_workers_1860"
# [86] "Male_count_1860"        "Male_lfp_1860"          "Male_workers_1860"      "Female_count_1870"      "Female_lfp_1870"
# [91] "Female_workers_1870"    "Male_count_1870"        "Male_lfp_1870"          "Male_workers_1870"      "Female_count_1880"
# [96] "Female_lfp_1880"        "Female_workers_1880"    "Male_count_1880"        "Male_lfp_1880"          "Male_workers_1880"
# [101] "Female_count_1900"      "Female_lfp_1900"        "Female_workers_1900"    "Male_count_1900"        "Male_lfp_1900"
# [106] "Male_workers_1900"      "log_totpop"             "pct_urb860"             "pct_urb860x100"         "pct_urb25"
# [111] "pct_urb25x100"          "disabwound"             "total_soldiers"         "whole"                  "pct_pop_died"
# [116] "pct_pop_disabwound"     "pct_pop_soldiers"       "pct_pop_mainbattle"     "pct_pop_whole"          "pct_pop_disabled"
# [121] "pct_pop_wounded"        "pct_pop_deserted"       "pct_pop_regout"         "pct_pop_diedx100"       "pct_pop_disabwoundx100"
# [126] "pct_pop_soldiersx100"   "pct_pop_wholex100"      "pct_pop_disabledx100"   "pct_pop_woundedx100"    "pct_pop_desertedx100"
# [131] "pct_pop_regoutx100"     "mainbattlenum_discrete" "pop_per_sqmi860"        "year"                   "count_unions"
# [136] "total_dues"             "estimated_membership"   "year1882"               "year1890"               "year1895"
# [141] "year1896"               "year1898"               "has_union"              "log_mbshp"              "Total_count_1860"
# [146] "log_farmval"            "log_mfgcap"             "county_name"            "PctVoteRepublican1860"  "PresVoteTurnout1860"
# [151] "geometry"

df <- as.data.frame(st_set_geometry(x = wctu_data_sf, value = NULL))
names(df) <- gsub("[^[:alnum:] ]", "", names(df))
df$logmfgestab <- log(df$mfgestab + 1)
df <- df %>% select(hasunion, pctpopdisabledx100, year, logtotpop, Femalelfp1860, Malelfp1860, logmfgestab, PresVoteTurnout1860, PctVoteRepublican1860, pctpopdied, pctpopsoldiers, pctpopregout ,pctpopwounded)
write_dta(df, "~/Desktop/df.dta")

library(broom)
coef1 <- tidy(lm1.5ip, conf.int = TRUE)
coef1$type <- "None (1)"
coef2 <- tidy(lm1.6ip, conf.int = TRUE)
coef2$type <- "Total population (2)"
coef3 <- tidy(lm1.7ip, conf.int = TRUE)
coef3$type <- "Male LFP (3)"
coef4 <- tidy(lm1.8ip, conf.int = TRUE)
coef4$type <- "Count manufacturers (4)"
coef5 <- tidy(lm1.9ip, conf.int = TRUE)
coef5$type <- "Voter turnout (5)"
coef6 <- tidy(lm1.10ip, conf.int = TRUE)
coef6$type <- "Percent Republican voters (6)"

coef <- rbind(coef1, coef2, coef3, coef4, coef5, coef6)
# coef <- coef %>% filter(term %in% c("year1890","year1895","year1896","year1898","pct_pop_disabledx100:year1890", "pct_pop_disabledx100:year1895", "pct_pop_disabledx100:year1896", "pct_pop_disabledx100:year1898"))
# coef$year <- stringr::str_sub(gsub("[^0-9.-]", "", coef$term), start = -4, end = -1)
# coef$type <- ifelse(test = str_detect(coef$term, pattern = ":"), yes = "year x disabled", no = "year FE")
coef <- coef %>% filter(term == "pct_pop_disabledx100")
coef$type <- factor(coef$type, levels = c("Percent Republican voters (6)","Voter turnout (5)","Count manufacturers (4)","Male LFP (3)","Total population (2)","None (1)"))
#coef$labels <- labels
#coef$type <- factor(c(rep("Union Army main effect", 5), rep("Year main effect",4), rep("Interaction effect",4)), levels = c("Year main effect", "Interaction effect", "Union Army main effect"))
coef$adjusted_rsq <- c(0.210 , 0.245 , 0.250 , 0.195 , 0.184 , 0.168)

ggplot(data = coef, aes(x = type, y =  estimate))+
  geom_point(aes(color=adjusted_rsq))+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high,color=adjusted_rsq))+
  labs(title = "", x = "Control (Column)", y = "Coefficient on Percent Disabled", color = "Adjusted R2") +
  scale_color_viridis() +
  theme_minimal() +
  coord_flip() + theme(axis.title.x = element_text(vjust=-0.7))

ggsave("~/Documents/Pitt/Projects/women_civil_war/figures/main_effect_plot.png", width = 7, height = 3)

library(effects)
test <- lm(has_union ~ pct_pop_disabledx100 + year , wctu_data_sf) # baseline model

eff_cf <- effect(term = "year", mod = test)
print(plot(eff_cf, multiline=TRUE))

effect_plot()







### HAZARD MODEL -- need to put this in a loop for more than 5 years...
# THIS JUST CORRECTS MY DATA...####
wctu_correct_sf <- wctu_data_sf

hazard_sf_1882 <- wctu_correct_sf %>% filter(year == 1882) # all of 1882
counties_1882 <- hazard_sf_1882$name[hazard_sf_1882$has_union == 1] # these counties should be 1 for all future years, too
table(wctu_correct_sf$has_union, wctu_correct_sf$year)
wctu_correct_sf$has_union[wctu_correct_sf$name %in% counties_1882] <- 1
table(wctu_correct_sf$has_union, wctu_correct_sf$year)

hazard_sf_1890 <- wctu_correct_sf %>% filter(year == 1890) # all of 1890
counties_1890 <- hazard_sf_1890$name[hazard_sf_1890$has_union == 1] # these counties should be 1 for all future years, too
wctu_correct_sf$has_union[wctu_correct_sf$year %in% c(1895, 1896, 1898) & wctu_correct_sf$name %in% counties_1890] <- 1
table(wctu_correct_sf$has_union, wctu_correct_sf$year)

hazard_sf_1895 <- wctu_correct_sf %>% filter(year == 1895) # all of 1895
counties_1895 <- hazard_sf_1895$name[hazard_sf_1895$has_union == 1] # these counties should be 1 for all future years, too
wctu_correct_sf$has_union[wctu_correct_sf$year %in% c(1896, 1898) & wctu_correct_sf$name %in% counties_1895] <- 1
table(wctu_correct_sf$has_union, wctu_correct_sf$year)

hazard_sf_1896 <- wctu_correct_sf %>% filter(year == 1896) # all of 1896
counties_1896 <- hazard_sf_1896$name[hazard_sf_1896$has_union == 1] # these counties should be 1 for all future years, too
wctu_correct_sf$has_union[wctu_correct_sf$year == 1898 & wctu_correct_sf$name %in% counties_1896] <- 1
table(wctu_correct_sf$has_union, wctu_correct_sf$year)

ggplot(wctu_correct_sf %>% filter(!is.na(year))) + geom_sf(aes(fill = as.factor(has_union)), size = .2, color = "black") + facet_wrap(~ year) + theme_void() + scale_fill_brewer(palette = "Paired") + labs(fill = "Has WCTU Union")
ggsave("~/Documents/Pitt/Projects/women_civil_war/figures/corrected_data.png", width = 8, height = 6)

hazard_sf_1882 <- wctu_data_sf %>% filter(year == 1882) # all of 1882
counties_1882 <- hazard_sf_1882$name[hazard_sf_1882$has_union == 0] # counties that haven't adopted a union yet as of 1882

# only bring counties forward that DIDN'T already have a union in 1882
hazard_sf_1890 <- wctu_data_sf %>% filter(year == 1890 & name %in% counties_1882)
hazard_sf_1895 <- wctu_data_sf %>% filter(year == 1895 & name %in% counties_1882)
hazard_sf_1896 <- wctu_data_sf %>% filter(year == 1896 & name %in% counties_1882)
hazard_sf_1898 <- wctu_data_sf %>% filter(year == 1898 & name %in% counties_1882)

# do the same for 1890
counties_1890 <- hazard_sf_1890$name[hazard_sf_1890$has_union == 0]  # counties that haven't adopted a union yet as of 1890
hazard_sf_1895 <- hazard_sf_1895 %>% filter(year == 1895 & name %in% counties_1890)
hazard_sf_1896 <- hazard_sf_1896 %>% filter(year == 1896 & name %in% counties_1890)
hazard_sf_1898 <- hazard_sf_1898 %>% filter(year == 1898 & name %in% counties_1890)

# do the same for 1895
counties_1895 <- hazard_sf_1895$name[hazard_sf_1895$has_union == 0]  # counties that haven't adopted a union yet as of 1895
hazard_sf_1896 <- hazard_sf_1896 %>% filter(year == 1896 & name %in% counties_1895)
hazard_sf_1898 <- hazard_sf_1898 %>% filter(year == 1898 & name %in% counties_1895)

# do the same for 1896
counties_1896 <- hazard_sf_1896$name[hazard_sf_1896$has_union == 0]  # counties that haven't adopted a union yet as of 1896
hazard_sf_1898 <- hazard_sf_1898 %>% filter(year == 1898 & name %in% counties_1896)

hazard_sf <- rbind(hazard_sf_1882, hazard_sf_1890, hazard_sf_1895, hazard_sf_1896, hazard_sf_1898)

ggplot(hazard_sf %>% filter(!is.na(year))) + geom_sf(aes(fill = as.factor(has_union)), size = .2, color = "black") + facet_wrap(~ year) + theme_void() + scale_fill_brewer(palette = "Paired") + labs(fill = "Has WCTU Union")
ggsave("~/Documents/Pitt/Projects/women_civil_war/figures/hazard_data.png", width = 8, height = 6)
hazard_data <- ggplot(hazard_sf %>% filter(!is.na(year))) + geom_sf(aes(fill = as.factor(has_union)), size = .2, color = "black") + facet_wrap(~ year, nrow = 1) + theme_void() + scale_fill_brewer(palette = "Paired") +
  theme(text = element_text(size=10), legend.position = "bottom")+
  labs(fill = "Has WCTU Union", title = "Hazard data", subtitle = "")
hazard_sf <- hazard_sf %>% filter(year %in% c(1882, 1890, 1895)) # no changes in 96 and 98, so remove completely
hazard_sf$year <- factor(hazard_sf$year, levels = c(1882, 1890, 1895))

corrected_data <- ggplot(wctu_correct_sf %>% filter(!is.na(year))) + geom_sf(aes(fill = as.factor(has_union)), size = .2, color = "black") + facet_wrap(~ year, nrow = 1) + theme_void() + scale_fill_brewer(palette = "Paired") +
  theme(text = element_text(size=10), legend.position = "none")+
  labs(fill = "Has WCTU Union", title = "Adjusted data", subtitle = "")
# HAZARD MODEL DATA... ####

original_data <- ggplot(wctu_data_sf %>% filter(!is.na(year))) +
  geom_sf(aes(fill = as.factor(has_union)), size = .2, color = "black") +
  facet_wrap(~ year, nrow = 1) + theme_void() + scale_fill_brewer(palette = "Paired") +
  theme(text = element_text(size=10), legend.position = "none")+
  labs(fill = "Has WCTU Union", title = "Raw data", subtitle = "")
grid.arrange(original_data, corrected_data, hazard_data, nrow = 3)
# NEIGHBORS...####


# robustness! (include column 2 from economic/poltical controls table as baseline comparison; add (i) column 2 run without years 1896-1898, (i) "corrected" data, (ii) hazard model, and (iii) control for neighboring county)
lm1.6ir <- lm(has_union ~ pct_pop_disabledx100  + log_totpop + year, wctu_data_sf)
robust_se6ir <- sqrt(diag(vcovHC(lm1.6ir, type = "HC1")))

lm1.7ir <- lm(has_union ~ pct_pop_disabledx100   + log_totpop + year, wctu_data_sf %>% filter(year %in% c(1882,1890,1895)))
robust_se7ir <- sqrt(diag(vcovHC(lm1.7ir, type = "HC1")))

lm1.8ir <- lm(has_union ~ pct_pop_disabledx100   + log_totpop + year, wctu_correct_sf %>% filter(year %in% c(1882,1890,1895)))
robust_se8ir <- sqrt(diag(vcovHC(lm1.8ir, type = "HC1")))

lm1.9ir <- lm(has_union ~ pct_pop_disabledx100   + log_totpop + year, hazard_sf %>% filter(year %in% c(1882,1890,1895)))
robust_se9ir <- sqrt(diag(vcovHC(lm1.9ir, type = "HC1")))

stargazer(#type = "text",
          lm1.6ir, lm1.7ir, lm1.8ir, #lm1.9ir,
          df = FALSE,
          se = list(robust_se6ir, robust_se7ir, robust_se8ir, robust_se9ir),
          title = "Robustness",
          label = "table:robust",
          column.labels = c("All years", "Restricted years", "Adjusted data"),
          covariate.labels = c("Percent disabled", "log(Total population, 1860)", "1890","1895","1896","1898"),
          dep.var.labels = c("County has at least one local WCTU"),
          # omit = "year",
          # omit = c("pct_pop_regoutx100", "pct_pop_woundedx100", "pct_pop_diedx100", "pct_pop_soldiersx100", "year", "pct_pop_disabledx100*year"),
          omit.stat = c("f", "ser", "rsq"),
          add.lines = list(c("Union Army percentages", rep("No", 4)),
                           c("Year F.E.", rep("Yes",4)))
)

# \\[-1.8ex] & \multicolumn{4}{c}{Outcome mean in 1882, 0.59} \\ & 1882-1898 & \multicolumn{3}{c}{1882-1895} \\ \cline{2-2}\cline{3-5} \\[-1.8ex] & All years & Restricted years & Corrected data & Hazard model \\ \\[-1.8ex] & (1) & (2) & (3) & (4)\\ \hline \\[-1.8ex]

# SURVIVAL ####
library(survival)
library(survminer)
wctu_data_sf$time <- as.numeric(as.character(wctu_data_sf$year)) - 1874 # Survival time in years (time a county will go without a WCTU)
wctu_data_sf$quintile <- ntile(wctu_data_sf$pct_pop_disabledx100, 4)
# wctu_data_sf$round_pct <- round(wctu_data_sf$pct_pop_disabledx100, 0)
# wctu_data_sf$round_pct[wctu_data_sf$round_pct == 4] <- 3
wctu_data_sf$median <- ifelse(test = wctu_data_sf$pct_pop_disabledx100 > median(wctu_data_sf$pct_pop_disabledx100, na.rm = TRUE), yes = "Above", no = "Below")

fit <- survfit(Surv(time, has_union) ~ median, data = wctu_data_sf)
summary(fit)$table
surv_summary(fit)
attr(fit, "table")
ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           # risk.table = TRUE, # Add risk table
           # risk.table.col = "strata", # Change risk table color by groups
           # linetype = "strata", # Change line type by groups
           # surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(),
           )
ggsave("~/Documents/Pitt/Projects/women_civil_war/figures/surv_plot_median.png", width = 6, height = 4)

summary(fit)

fit2 <- survfit(Surv(time, has_union) ~ quintile, data = wctu_data_sf)
ggsurvplot(fit2,
           pval = FALSE, conf.int = TRUE,
           # risk.table = TRUE, # Add risk table
           # risk.table.col = "strata", # Change risk table color by groups
           # linetype = "strata", # Change line type by groups
           # surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(),
)
ggsave("~/Documents/Pitt/Projects/women_civil_war/figures/quintile14.png", width = 6, height = 4)


fit2 <- survfit(Surv(time, has_union) ~ quintile, data = wctu_data_sf)
ggsurvplot(fit2,
           pval = FALSE, conf.int = FALSE,
           # risk.table = TRUE, # Add risk table
           # risk.table.col = "strata", # Change risk table color by groups
           # linetype = "strata", # Change line type by groups
           # surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(),
)
ggsave("~/Documents/Pitt/Projects/women_civil_war/figures/quintile.png", width = 6, height = 4)

dt <- data.frame(x=c(1:200),y=rnorm(200))
dens <- density(wctu_data_sf$pct_pop_disabledx100, na.rm = TRUE, kernel = "rectangular")
df <- data.frame(x=dens$x, y=dens$y)
probs <- c(0, 0.25, 0.5, 0.75, 1)
quantiles <- quantile(dt$y, prob=probs)
df$quant <- factor(findInterval(df$x,quantiles))
ggplot(df, aes(x,y)) +
  geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) +
  #scale_x_continuous(breaks=quantiles) +
  scale_fill_brewer(guide="none")

library(ggridges)

ggplot(data = wctu_data_sf, aes(x = pct_pop_disabled, y = 0, fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = 4, quantile_lines = TRUE) +
  scale_fill_viridis_d(name = "Quantiles") +
  theme_minimal()

# fit2 <- survfit(Surv(time, has_union) ~ round_pct, data = wctu_data_sf %>% filter(round_pct %in% c(0,3)))
# ggsurvplot(fit2,
#            pval = TRUE, conf.int = TRUE,
#            # risk.table = TRUE, # Add risk table
#            # risk.table.col = "strata", # Change risk table color by groups
#            # linetype = "strata", # Change line type by groups
#            # surv.median.line = "hv", # Specify median survival
#            ggtheme = theme_bw(),
# )
# ggsave("~/Documents/Pitt/Projects/women_civil_war/figures/surv_plot_pct.png", width = 6, height = 4)

# Assumptions of Cox proportional hazard model is that effect of x on y is linear over time. I can't validate this yet (more assumptions found here: http://www.sthda.com/english/wiki/cox-model-assumptions)
# res.cox.d <- coxph(Surv(time, has_union) ~ pct_pop_disabledx100, data = wctu_data_sf)
# res.cox.p <- coxph(Surv(time, has_union) ~ log_totpop, data = wctu_data_sf)
# res.cox.b <- coxph(Surv(time, has_union) ~ pct_pop_disabledx100 + log_totpop, data = wctu_data_sf)
# summary(res.cox.b)
# stargazer(res.cox.d, res.cox.p, res.cox.b, df = FALSE)
#
# ggsurvplot(survfit(res.cox), color = "#2E9FDF",
#            ggtheme = theme_minimal())
