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
wctu_data_sf$has_union <- ifelse(wctu_data_sf$count_unions > 0, yes = 1, no = 0)
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


# Main table
lm1.1i <- lm(has_union ~ pct_pop_disabledx100 + year + pct_pop_disabledx100*year, wctu_data_sf) # baseline model
robust_se1i <- sqrt(diag(vcovHC(lm1.1i, type = "HC1"))) # calculate robust se

lm1.2i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + year + pct_pop_disabledx100*year, wctu_data_sf)
robust_se2i <- sqrt(diag(vcovHC(lm1.2i, type = "HC1")))

lm1.3i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + year + pct_pop_disabledx100*year, wctu_data_sf)
robust_se3i <- sqrt(diag(vcovHC(lm1.3i, type = "HC1")))

lm1.4i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + year + pct_pop_disabledx100*year, wctu_data_sf)
robust_se4i <- sqrt(diag(vcovHC(lm1.4i, type = "HC1")))

lm1.5i <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year, wctu_data_sf)
robust_se5i <- sqrt(diag(vcovHC(lm1.5i, type = "HC1")))

stargazer(type = "text",
          ci = TRUE,
          lm1.1i, lm1.2i, lm1.3i, lm1.4i, lm1.5i,
          df = FALSE,
          se = list(robust_se1i, robust_se2i, robust_se3i, robust_se4i, robust_se5i),
          title = "Main table",
          label = "table:uavars",
          covariate.labels = c("Percent disabled", "Percent exited regularly","Percent wounded","Percent died","Percent enlisted"),
          dep.var.caption = c("County has at least one local WCTU"),
          dep.var.labels = "Outcome mean in 1882, 0.59",
          omit = c("year", "pct_pop_disabledx100*year"),
          omit.stat = c("f", "ser", "rsq"),
          add.lines = list(#c("Union Army percentages", rep("Yes", 5)),
            c("Year F.E.", rep("Yes", 5)),
            c("Year x Percent disabled", rep("Yes", 5)))
)


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

# socioeconmic/poltical controls
lm1.5ip <- lm1.5i # repeat last column of previous table
robust_se5ip <- robust_se5i

lm1.6ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year + log_totpop, wctu_data_sf)
robust_se6ip <- sqrt(diag(vcovHC(lm1.6ip, type = "HC1")))

lm1.7ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year + log_totpop + log_farmval, wctu_data_sf)
robust_se7ip <- sqrt(diag(vcovHC(lm1.7ip, type = "HC1")))

lm1.8ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year + log_totpop + log_mfgcap, wctu_data_sf)
robust_se8ip <- sqrt(diag(vcovHC(lm1.8ip, type = "HC1")))

lm1.9ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year + log_totpop + PresVoteTurnout1860, wctu_data_sf)
robust_se9ip <- sqrt(diag(vcovHC(lm1.9ip, type = "HC1")))

lm1.10ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year + log_totpop + PctVoteRepublican1860, wctu_data_sf)
robust_se10ip <- sqrt(diag(vcovHC(lm1.10ip, type = "HC1")))

# lm1.11ip <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year + log_totpop + PctVoteRepublican1860 + PresVoteTurnout1860 + log_mfgcap + log_farmval, wctu_data_sf)
# robust_se11ip <- sqrt(diag(vcovHC(lm1.11ip, type = "HC1")))

stargazer(#type = "text",
          lm1.5ip, lm1.6ip, lm1.7ip, lm1.8ip, lm1.9ip, lm1.10ip, #lm1.11ip,
          df = FALSE,
          se = list(robust_se5ip, robust_se6ip, robust_se7ip, robust_se8ip, robust_se9ip, robust_se10ip),
          title = "Socioeconomic/poltical controls",
          label = "table:econ_political_controls",
          covariate.labels = c("Percent disabled", "log(Total population, 1860)", "log(Cash value of farms, 1860)", "log(Manufacturing capital, 1860)","Voter turnout$^\\dagger$", "Percent Republican votes$^\\dagger$"),
          dep.var.caption = c("County has at least one local WCTU"),
          dep.var.labels = "Outcome mean in 1882, 0.59",
          omit = c("pct_pop_regoutx100", "pct_pop_woundedx100", "pct_pop_diedx100", "pct_pop_soldiersx100", "year", "pct_pop_disabledx100*year"),
          omit.stat = c("f", "ser", "rsq"),
          add.lines = list(c("Union Army percentages", rep("Yes", 6)),
                           c("Year F.E.", rep("Yes",6)),
                           c("Year x Percent disabled", rep("Yes",6))),
          notes = "$^\\dagger$1860 Presidential Election"
)




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
lm1.6ir <- lm1.6ip
robust_se6ir <- robust_se6ip

lm1.7ir <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year + log_totpop, wctu_data_sf %>% filter(year %in% c(1882,1890,1895)))
robust_se7ir <- sqrt(diag(vcovHC(lm1.7ir, type = "HC1")))

lm1.8ir <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year + log_totpop, wctu_correct_sf %>% filter(year %in% c(1882,1890,1895)))
robust_se8ir <- sqrt(diag(vcovHC(lm1.8ir, type = "HC1")))

lm1.9ir <- lm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year + log_totpop, hazard_sf %>% filter(year %in% c(1882,1890,1895)))
robust_se9ir <- sqrt(diag(vcovHC(lm1.9ir, type = "HC1")))

stargazer(lm1.6ir, lm1.7ir, lm1.8ir, lm1.9ir,
          df = FALSE,
          se = list(robust_se6ir, robust_se7ir, robust_se8ir, robust_se9ir),
          title = "Robustness",
          label = "table:robust",
          column.labels = c("All years", "Restricted years", "Corrected data", "Hazard data"),
          covariate.labels = c("Percent disabled", "log(Total population, 1860)"),
          dep.var.caption = c("County has at least one local WCTU"),
          dep.var.labels = "Outcome mean in 1882, 0.59",
          omit = c("pct_pop_regoutx100", "pct_pop_woundedx100", "pct_pop_diedx100", "pct_pop_soldiersx100", "year", "pct_pop_disabledx100*year"),
          omit.stat = c("f", "ser", "rsq"),
          add.lines = list(c("Union Army percentages", rep("Yes", 4)),
                           c("Year F.E.", rep("Yes",4)),
                           c("Year x Percent disabled", rep("Yes",4)))
)

# \\[-1.8ex] & \multicolumn{4}{c}{Outcome mean in 1882, 0.59} \\ & 1882-1898 & \multicolumn{3}{c}{1882-1895} \\ \cline{2-2}\cline{3-5} \\[-1.8ex] & All years & Restricted years & Corrected data & Hazard model \\ \\[-1.8ex] & (1) & (2) & (3) & (4)\\ \hline \\[-1.8ex]
