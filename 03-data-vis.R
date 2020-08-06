library(ggplot2)
library(viridis)
library(tidyr)
library(gridExtra)
library(stargazer)
library(readxl)
library(mapproj)
library(USAboundaries)
library(sf)
library(pryr)
library(rgeos)

# load data
source("~/git/women-postbellum/01-clean-map-data.R")

ggplot() +
    geom_sf(data = full_data_sf %>% filter(!is.na(mainbattlenum_discrete)), aes(fill = mainbattlenum_discrete), color = "black", size = .2) +
    scale_fill_viridis(discrete = TRUE) +
    geom_sf(data = full_data_sf, aes(fill = NA), color = "black", size = .2) +
    geom_sf(data = union_states_sf, fill = NA, color = "black", size = .5) +
    theme_void() +
    labs(fill = "Mean number of major battles fought")+ theme(legend.position="bottom")
ggsave("~/Documents/Pitt/Projects/women_civil_war/figures/map_mainbattlenum_discrete.png", width = 8, height = 6)

# This would be a good map if 1860 totpop was available for every county pictured (e.g. Cameron, PA didn't exist in the 1860 Census, so UA data exists for it, but not totpop)
# full_data_sf$round_pct_disabled <- round(full_data_sf$pct_pop_disabled, 2)
# ggplot() +
#     geom_sf(data = full_data_sf %>% filter(pct_pop_disabled < .05), aes(fill = factor(round_pct_disabled)), color = "black", size = .2) +
#     scale_fill_viridis(discrete = TRUE) +
#     geom_sf(data = union_states_sf, fill = NA, color = "black") +
#     theme_void() + labs(title = "Percent of male population disabled in the Civil War")

# summary(full_data_sf$pct_pop_soldiers)
# full_data_sf$above_median <- ifelse(full_data_sf$pct_pop_soldiers >= 0.135, yes = "Above median", no = "Below median")
# full_data_sf$quantile_soldiers <- ntile(full_data_sf$pct_pop_soldiers, 4)
# ggplot() +
#     geom_sf(data = full_data_sf, aes(fill = as.factor(quantile_soldiers)), color = "black", size = .2) + scale_fill_viridis_d() +
#     geom_sf(data = union_states_sf, fill = NA, color = "black") +
#     theme_void() + labs(title = "Percent of male population fighting in the Civil War") +
#     labs(fill = "Quantile, % Men")
#
# summary(full_data_sf$pct_pop_disabled)
# full_data_sf$above_median_disabled <- ifelse(full_data_sf$pct_pop_disabled >= 0.011926, yes = "Above median", no = "Below median")
# full_data_sf$quantile_disabled <- ntile(full_data_sf$pct_pop_disabled, 4)
# data$above_median_disabled <- ifelse(data$pct_pop_disabled >= 0.011926, yes = "Above median", no = "Below median")
# data$quantile_disabled <- ntile(data$pct_pop_disabled, 4)
#
# data_long <- gather(data, year, Female_lfp, Female_lfp_1850, Female_lfp_1860, Female_lfp_1870, Female_lfp_1880, Female_lfp_1900, factor_key = TRUE)
# data_long$year <- gsub(pattern = "Female_lfp_", replacement = "", x = data_long$year)
# # female lfp by median soldier pop
# ggplot(data = data_long %>% filter(!is.na(above_median)), aes(x = year, y = Female_lfp, fill = above_median)) +
#     geom_boxplot() +
#     labs(x = "Census Year", y = "Female LFP", fill = "Counties' % soldier population") +
#     scale_fill_brewer(palette="Accent")
# ggsave("~/Documents/Pitt/Projects/women_civil_war/meeting_notes/boxplot_lfp_soldier_median.png", width = 8, height = 5)

# ggplot(data = data_long %>% filter(!is.na(above_median)), aes(x = year, y = Female_lfp, fill = as.factor(quantile_soldiers))) +
#     geom_boxplot() +
#     labs(x = "Census Year", y = "Female LFP", fill = "Quantile % soldier population") +
#     scale_fill_brewer(palette="Accent")
# ggsave("~/Documents/Pitt/Projects/women_civil_war/meeting_notes/boxplot_lfp_soldier_quantile.png", width = 8, height = 5)

# female lfp by % disabled
# ggplot(data = data_long %>% filter(!is.na(above_median_disabled)), aes(x = year, y = Female_lfp, fill = above_median_disabled)) +
#     geom_boxplot() +
#     labs(x = "Census Year", y = "Female LFP", fill = "Counties' % soldier disabled") +
#     scale_fill_brewer(palette="Accent")
# ggsave("~/Documents/Pitt/Projects/women_civil_war/meeting_notes/boxplot_lfp_disabled_median.png", width = 8, height = 5)

# ggplot(data = data_long %>% filter(!is.na(above_median_disabled) & quantile_disabled %in% c(1,4)), aes(x = year, y = Female_lfp, fill = as.factor(quantile_disabled))) +
#     geom_boxplot() +
#     labs(x = "Census Year", y = "Female LFP", fill = "Quantile % disabled population") +
#     scale_fill_brewer(palette="Accent")
# ggsave("~/Documents/Pitt/Projects/women_civil_war/meeting_notes/boxplot_lfp_disabled_quantile4.png", width = 8, height = 5)


# female lfp by mainbattlenum
# ggplot(data = data_long %>% filter(!is.na(mainbattlenum_discrete)), aes(x = year, y = Female_lfp, fill = mainbattlenum_discrete)) +
#     geom_boxplot() +
#     labs(x = "Census Year", y = "Female LFP", fill = "Mean # major battles", subtitle = "379, 360, 30, and 4 observations, respectively")
# ggplot(data_long %>% filter(kmw < 10000), aes(x = kmw, y = Female_lfp, color = year)) + geom_point() + scale_color_viridis_d()
# ggplot(data_long, aes(x = mainbattlenum, y = Female_lfp, color = year)) + geom_point() + scale_color_viridis_d()

# data_summary <- function(data, varname, groupnames){
#     require(plyr)
#     summary_func <- function(x, col){
#         c(mean = mean(x[[col]], na.rm=TRUE),
#           sd = sd(x[[col]], na.rm=TRUE),
#           median = median(x[[col]], na.rm=TRUE))
#     }
#     data_sum<-ddply(data, groupnames, .fun=summary_func,
#                     varname)
#     data_sum <- rename(data_sum, c("mean" = paste0(varname,"_mean"), "median" = paste0(varname,"_median")))
#     return(data_sum)
# }
#
# data_long$quantile_disabled <- ntile(data_long$pct_pop_disabled, 4)
# temp <- data_summary(data_long, varname = "Female_lfp", groupnames = c("year", "quantile_disabled"))
# ggplot(temp %>% filter(!is.na(quantile_disabled)), aes(x = year, y = Female_lfp_mean, group = as.factor(quantile_disabled), color = as.factor(quantile_disabled))) +
#     geom_errorbar(aes(ymin = Female_lfp_mean-sd, ymax=Female_lfp_mean+sd), width=.1) +
#     geom_line() + geom_point()+
#     scale_color_brewer(palette="Dark2")+theme_minimal() + labs(x = "Year", y = "Mean LFP - Women", color = "Quantile Pct Pop Disabled")
#
