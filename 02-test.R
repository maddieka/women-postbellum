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
library(margins) # for marginal effects of probit
library(mfx)
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

mean(wctu_data_sf$has_union[wctu_data_sf$year == 1882], na.rm = TRUE)
ggplot(wctu_data_sf %>% filter(!is.na(year))) + geom_sf(aes(fill = as.factor(has_union)), size = .2, color = "black") + facet_wrap(~ year) + theme_void() + scale_fill_brewer(palette = "Paired") + labs(fill = "Has WCTU Union")
ggsave("~/Documents/Pitt/Projects/women_civil_war/figures/original_data.png", width = 8, height = 6)

# Main table
lm1.1i <- glm(has_union ~ pct_pop_disabledx100,
              data = wctu_data_sf,
              family = binomial(link = "probit")) # baseline model
robust_se1i <- sqrt(diag(vcovHC(lm1.1i, type = "HC1"))) # calculate robust se

lm1.2i <- glm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + year + pct_pop_disabledx100*year,
              data = wctu_data_sf,
              family = binomial(link = "probit"))
robust_se2i <- sqrt(diag(vcovHC(lm1.2i, type = "HC1")))

lm1.3i <- glm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + year + pct_pop_disabledx100*year,
              data = wctu_data_sf,
              family = binomial(link = "probit"))
robust_se3i <- sqrt(diag(vcovHC(lm1.3i, type = "HC1")))

lm1.4i <- glm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + year + pct_pop_disabledx100*year,
              data = wctu_data_sf,
              family = binomial(link = "probit"))
robust_se4i <- sqrt(diag(vcovHC(lm1.4i, type = "HC1")))

lm1.5i <- glm(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year,
              data = wctu_data_sf,
              family = binomial(link = "probit"))
robust_se5i <- sqrt(diag(vcovHC(lm1.5i, type = "HC1")))

# visualize predicted probabilities
range(wctu_data_sf$pct_pop_disabledx100, na.rm = TRUE)
xweight <- seq(0, 3.7, 0.01)
lm1.1i <- lm(has_union ~ pct_pop_disabledx100 , wctu_data_sf) # baseline model
yweight <- as.data.frame(stats::predict.lm(lm1.1i, newdata = data.frame(pct_pop_disabledx100=xweight), type="response", se.fit = TRUE))
yweight$x <- xweight
yweight$lowci <- yweight$fit-1.96*yweight$se.fit
yweight$highci <- yweight$fit+1.96*yweight$se.fit

lm <- ggplot() +
  geom_ribbon(data = yweight, aes(x = x, ymin = lowci, ymax=highci), alpha=.2) +
  geom_point(data = yweight, aes(x = x, y = fit)) +
  geom_point(data = wctu_data_sf, aes(x = pct_pop_disabledx100, y = has_union)) +
  labs(x = "Percent disabled", y = "Predicted probability", title = 'lm(has_union ~ pct_pop_disabledx100 , wctu_data_sf)')

lm1.1i <- glm(has_union ~ pct_pop_disabledx100,
              data = wctu_data_sf,
              family = binomial(link = "probit"))
yweight <- as.data.frame(stats::predict.glm(lm1.1i, newdata = data.frame(pct_pop_disabledx100=xweight), type="response", se.fit = TRUE))
yweight$x <- xweight
yweight$lowci <- yweight$fit-1.96*yweight$se.fit
yweight$highci <- yweight$fit+1.96*yweight$se.fit

glm <- ggplot() +
geom_ribbon(data = yweight, aes(x = x, ymin = lowci, ymax=highci), alpha = .2) +
geom_point(data = yweight, aes(x = x, y = fit)) +
geom_point(data = wctu_data_sf, aes(x = pct_pop_disabledx100, y = has_union, color = year)) +
labs(x = "Percent disabled", y = "Predicted probability", title = 'glm(has_union ~ pct_pop_disabledx100,
data = wctu_data_sf,
family = binomial(link = "probit"))')

grid.arrange(lm, glm, ncol = 2)

plot(wctu_data_sf$pct_pop_disabledx100, wctu_data_sf$has_union, pch = 16)
lines(xweight, yweight)

# comparing raw output and marginal effects...
summary(lm1.5i)
me1.5i <- margins(lm1.5i)
plot(me1.5i)

sum_me1.5i <- summary(margins(lm1.5i))

ggplot(data = sum_me1.5i) +
  geom_point(aes(factor, AME)) +
  geom_errorbar(aes(x = factor, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))




probit_margins <- probitmfx(has_union ~ pct_pop_disabledx100 + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + year + pct_pop_disabledx100*year,
                            data = wctu_data_sf)$mfxest
stargazer(lm1.5i, coef = probit_margins[,1], se = probit_margins[,2], type = "text")

stargazer(type = "text",
          lm1.1i, lm1.2i, lm1.3i, lm1.4i, lm1.5i,
          df = FALSE,
          se = list(robust_se1i, robust_se2i, robust_se3i, robust_se4i, robust_se5i),
          title = "Main table",
          label = "table:uavars",
          covariate.labels = c("Percent disabled", "Percent exited regularly","Percent wounded","Percent died","Percent fought"),
          dep.var.caption = c("County has at least one local WCTU"),
          dep.var.labels = "Outcome mean in 1882, 0.59",
          omit = c("year", "pct_pop_disabledx100*year"),
          add.lines = list(#c("Union Army percentages", rep("Yes", 5)),
            c("Year F.E.", rep("Yes", 5)),
            c("County F.E.", rep("No", 5)),
            c("Year x Percent disabled", rep("Yes", 5)))
)


