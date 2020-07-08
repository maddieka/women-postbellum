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
options(scipen=999) # prevent scientific notation

# load data
#source("~/git/women-postbellum/01-clean-merge-data.R")
source("~/git/women-postbellum/01-clean-map-data.R")

# 1890 WITHOUT CONTROLLING FOR POPULATION
has_wctu_1890_lm.1 <- lm(has_wctu_1890 ~ pct_pop_disabledx100  , mi_data_sf)
has_wctu_1890_lm.2 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100  , mi_data_sf)
has_wctu_1890_lm.3 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 , mi_data_sf)
has_wctu_1890_lm.4 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 + pct_pop_diedx100 , mi_data_sf)
has_wctu_1890_lm.5 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 + pct_pop_diedx100 + pct_pop_regoutx100 , mi_data_sf)
has_wctu_1890_lm.6 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 + pct_pop_diedx100 + pct_pop_soldiersx100, mi_data_sf)
mean(mi_data_sf$has_wctu_1890)
stargazer(has_wctu_1890_lm.1, has_wctu_1890_lm.2, has_wctu_1890_lm.3, has_wctu_1890_lm.4, has_wctu_1890_lm.5, has_wctu_1890_lm.6, 
          df = FALSE, 
          add.lines = list(c("Outcome Mean", rep(0.75, 6))))
autoplot(has_wctu_1890_lm.1)
autoplot(has_wctu_1890_lm.6) # this is NOT a good regression: residuals aren't normally distributed, we have a homoskedasticity problem, AND outliers have high leverage

# 1890 WITH POPULATION CONTROL
# mi_data_sf$population_control <- mi_data_sf$pct_urb860x100
# mi_data_sf$population_control <- mi_data_sf$totpop
# mi_data_sf$population_control <- mi_data_sf$log_totpop
mi_data_sf$population_control <- mi_data_sf$pop_per_sqmi860

has_wctu_1890_lm.1 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + population_control, mi_data_sf)
has_wctu_1890_lm.2 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + population_control, mi_data_sf)
has_wctu_1890_lm.3 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 + population_control, mi_data_sf)
has_wctu_1890_lm.4 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 + pct_pop_diedx100 + population_control, mi_data_sf)
has_wctu_1890_lm.5 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 + pct_pop_diedx100 + pct_pop_regoutx100 + population_control, mi_data_sf)
has_wctu_1890_lm.6 <- lm(has_wctu_1890 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + population_control, mi_data_sf)
mean(mi_data_sf$has_wctu_1890)
stargazer(has_wctu_1890_lm.1, has_wctu_1890_lm.2, has_wctu_1890_lm.3, has_wctu_1890_lm.4, has_wctu_1890_lm.5, has_wctu_1890_lm.6, 
          df = FALSE, 
          add.lines = list(c("Outcome Mean", rep(0.75, 6))))

# 1883 WITH CONTROLLING FOR log_totpop
has_wctu_1883_lm.1 <- lm(has_wctu_1883 ~ pct_pop_disabledx100 + pct_urb860x100, mi_data_sf)
has_wctu_1883_lm.2 <- lm(has_wctu_1883 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_urb860x100, mi_data_sf)
has_wctu_1883_lm.3 <- lm(has_wctu_1883 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 + pct_urb860x100, mi_data_sf)
has_wctu_1883_lm.4 <- lm(has_wctu_1883 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 + pct_pop_diedx100 + pct_urb860x100, mi_data_sf)
has_wctu_1883_lm.5 <- lm(has_wctu_1883 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 + pct_pop_diedx100 + pct_pop_regoutx100 + pct_urb860x100, mi_data_sf)
has_wctu_1883_lm.6 <- lm(has_wctu_1883 ~ pct_pop_disabledx100 + pct_pop_woundedx100 + pct_pop_desertedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + pct_urb860x100, mi_data_sf)
mean(mi_data_sf$has_wctu_1883)
stargazer(has_wctu_1883_lm.1, has_wctu_1883_lm.2, has_wctu_1883_lm.3, has_wctu_1883_lm.4, has_wctu_1883_lm.5, has_wctu_1883_lm.6, 
          df = FALSE, 
          add.lines = list(c("Outcome Mean", rep(0.24, 6))))
autoplot(has_wctu_1883_lm.6)


summary(mi_data_sf$totpop)
mi_data_sf$pop_above_median <- ifelse(test = mi_data_sf$totpop > 6242, yes = 1, no = 0)
mi_data_sf$log_kmw <- log(mi_data_sf$kmw + 1)

summary(data$totpop)
data$pop_above_median <- ifelse(test = data$totpop > 16180, yes = 1, no = 0)
data$log_kmw <- log(data$kmw + 1)

a <- lm(mainbattlenum ~ log_totpop, mi_data_sf)
b <- lm(mainbattlenum ~ log_kmw, mi_data_sf)
c <- lm(mainbattlenum ~ log_totpop + log_kmw, mi_data_sf)
d <- lm(mainbattlenum ~ log_totpop, data)
e <- lm(mainbattlenum ~ log_kmw, data)
f <- lm(mainbattlenum ~ log_totpop + log_kmw, data)
stargazer(a, b, c, d,e, f,df = FALSE)

ggplot(data, aes(x = log_totpop, y = mainbattlenum)) + geom_point() + geom_smooth(method = "lm")
ggsave("~/Documents/Pitt/Projects/women_civil_war/meeting_notes/mainbattlenum_on_logtotpop.png", width = 10, height = 6)
ggplot(data, aes(x = kmw, y = mainbattlenum)) + geom_point() + geom_smooth(method = "lm")
ggsave("~/Documents/Pitt/Projects/women_civil_war/meeting_notes/mainbattlenum_on_kmw.png", width = 10, height = 6)
ggplot(data, aes(x = log_kmw, y = mainbattlenum)) + geom_point() 
ggplot(data, aes(x = log_totpop, y = log_kmw)) + geom_point() + geom_smooth(method = "lm")
ggsave("~/Documents/Pitt/Projects/women_civil_war/meeting_notes/logkmw_on_logtotpop.png", width = 10, height = 6)
ggplot(data, aes(x = totpop, y = kmw)) + geom_point() + geom_smooth(method = "lm") +coord_cartesian(xlim = c(0, 200000))


# mi_data_sf$log_disabled <- log(mi_data_sf$disabled + 1)
# mi_data_sf$log_wounded <- log(mi_data_sf$wounded + 1)
# mi_data_sf$log_deserted <- log(mi_data_sf$deserted + 1)
# mi_data_sf$log_died <- log(mi_data_sf$died + 1)
# mi_data_sf$log_regout <- log(mi_data_sf$regout + 1)
# mi_data_sf$log_soldiers <- log(mi_data_sf$total_soldiers + 1)
# 
# has_wctu_1890_lm.1 <- lm(has_wctu_1890 ~ log_disabled + log_totpop, mi_data_sf)
# has_wctu_1890_lm.2 <- lm(has_wctu_1890 ~ log_disabled + log_wounded + log_totpop, mi_data_sf)
# has_wctu_1890_lm.3 <- lm(has_wctu_1890 ~ log_disabled + log_wounded + log_deserted + log_totpop, mi_data_sf)
# has_wctu_1890_lm.4 <- lm(has_wctu_1890 ~ log_disabled + log_wounded + log_deserted + log_died + log_totpop, mi_data_sf)
# has_wctu_1890_lm.5 <- lm(has_wctu_1890 ~ log_disabled + log_wounded + log_deserted + log_died + log_regout + log_totpop, mi_data_sf)
# has_wctu_1890_lm.6 <- lm(has_wctu_1890 ~ log_disabled + log_wounded + log_deserted + log_died + log_soldiers + log_totpop, mi_data_sf)
# mean(mi_data_sf$has_wctu_1890)
# stargazer(has_wctu_1890_lm.1, has_wctu_1890_lm.2, has_wctu_1890_lm.3, has_wctu_1890_lm.4, has_wctu_1890_lm.5, has_wctu_1890_lm.6, 
#           df = FALSE, 
#           add.lines = list(c("Outcome Mean", rep(0.75, 6))))
