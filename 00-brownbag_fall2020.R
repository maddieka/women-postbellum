# create suffrage/women's temperance graph for beginning of fall 2020 brown bag

# load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)

# settings
options(scipen=999) # prevent scientific notation
setwd("~/Documents/Pitt/Projects/women_civil_war")

#load data
suffrage <- read.csv("data/suffrage/suffrage_timeline.csv") %>% filter(suffrage_type %in% c("school", "full", "partial"))
orgs <- data.frame(year = c(1873, 1879, 1883, 1890, 1900, 1910, 1920, 1893, 1905,1907,1910,1915,1917, 1890, 1851, 1855, 1865, 1869),
                   # count_states_wctu = c(0,24,42,48,52,53,53),
                   # count_unions_wctu = c(0,1118,2580,7126,7067,12000,12000),
                   org = c(rep("wctu", 7), rep("nawsa", 7), rep("good templars", 4)),
                   natl_membership = c(0,26843,73176,149527,168324,248343,345949,13150,17000,45501,75000,150000,200000,7000, 11/2, 70000/2, 60000/2, 400000/2))

data <- data.frame(year = seq(1860, 1920, 1), count_full = NA, count_school = NA, count_partial = NA)


for (i in data$year) {

    j <- i - 1859
    data$count_full[j] <- as.numeric(suffrage %>% filter(suffrage_type == "full") %>% summarise(sum(year <= i)))
    data$count_school[j] <- as.numeric(suffrage %>% filter(suffrage_type == "school") %>% summarise(sum(year <= i)))
    data$count_partial[j] <- as.numeric(suffrage %>% filter(suffrage_type == "partial") %>% summarise(sum(year <= i)))

}

data[nrow(data), ] <- c(1920, 48, 48, 48)

data_long <- gather(data, suffrage_type, count_states, count_full:count_partial, factor_key=TRUE)
# data_long$prop_states <- data_long$count_states / 48


ggplot(data = data_long, aes(x = year, y = count_states, fill = suffrage_type)) +
    geom_bar(stat = "identity", position=position_dodge()) +
    annotate("rect", xmin = 1861, xmax = 1865, ymin = 0, ymax = 48, alpha = .2) +
    scale_fill_brewer(palette = "Set2", labels = c("Full", "School","Partial")) +
    # scale_fill_manual(values=c("#33a02c", "#fb9a99", "#e31a1c"), labels = c("Full", "School","Partial")) +
    theme_minimal()+
    # xlim(1850, 1920) +
    labs(x = "Year", y = "Number of states with suffrage legislation passed", fill = "Suffrage type")
ggsave("figures/suffrage_hist.png", width = 8, height = 5)

a <-
    ggplot(data = data_long, aes(x = year, y = count_states, fill = suffrage_type)) +
    geom_bar(stat = "identity", position=position_dodge()) +
    annotate("rect", xmin = 1861, xmax = 1865, ymin = 0, ymax = 48, alpha = .2) +
    scale_fill_brewer(palette = "Set2", labels = c("Full", "School","Partial")) +
    # scale_fill_manual(values=c("#33a02c", "#fb9a99", "#e31a1c"), labels = c("Full", "School","Partial")) +
    theme_minimal()+
    theme(legend.position="bottom") +
    xlim(1850, 1921) +
    labs(x = "Year", y = "Number of states", fill = "Type of suffrage legislation passed")

# plot_data <- merge(data_long, orgs, by = "year", all.x = TRUE)
#
# ggplot(data = plot_data) +
#     geom_bar(aes(x = year, y = count_states, fill = suffrage_type),
#              stat = "identity",
#              position=position_dodge()) +
#     geom_point(aes(x = year, y = count_states_wctu))

# head(orgs)
#
# dates <- data.frame(year = c(1865, 1869, 1873, 1890),
#                     ymax = rep(250000, 4),
#                     event = c("End of Civil War","NWSA and AWSA founded", "WCTU founded", "NWSA and AWSA merge into NAWSA"))
ggplot() +
    geom_point(data = orgs, aes(x = year, y = natl_membership, color = org, group = org)) +
    geom_line(data = orgs, aes(x = year, y = natl_membership, color = org, group = org)) +
    # geom_segment(data = dates, aes(x = year, xend = year, y = 0 , yend = ymax)) +
    annotate("rect", xmin = 1861, xmax = 1865, ymin = 0, ymax = 350000, alpha = .2) +
    scale_color_brewer(palette = "Set1", labels = c("Good Templars", "NAWSA", "WCTU")) +
    # xlim(1850, 1920) +
    theme_minimal() +
    labs(x = "Year", y = "National Membership", color = "Organization")
ggsave("figures/org_membership_plot.png", width = 8, height = 5)

b <-
ggplot() +
    geom_point(data = orgs, aes(x = year, y = natl_membership, color = org, group = org)) +
    geom_line(data = orgs, aes(x = year, y = natl_membership, color = org, group = org)) +
    # geom_segment(data = dates, aes(x = year, xend = year, y = 0 , yend = ymax)) +
    annotate("rect", xmin = 1861, xmax = 1865, ymin = 0, ymax = 350000, alpha = .2) +
    scale_color_brewer(palette = "Set1", labels = c("Good Templars", "NAWSA", "WCTU")) +
    xlim(1850, 1921) +
    theme_minimal() +
    theme(legend.position="top",
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    labs(x = "", y = "Membership", color = "Organization")

# stack the two plots
plot_grid(b, a, align = 'v', hjust = -1, nrow = 2)
ggsave("figures/combined.png", width = 6, height = 4)
# here you add the legend
p <- plot_grid( prow, legenda, legendb , rel_widths = c(3, .3))
p

# plot on same graph
head(orgs)
head(data_long)

df <- merge(x = data_long, y = orgs, all.x = TRUE)

ggplot(data = df, aes(x = year)) +
    geom_bar(aes(y = count_states, fill = suffrage_type), stat = "identity", position=position_dodge()) +
    geom_point(aes(y = natl_membership / 10000, color = org)) +
    geom_line(aes(y = natl_membership / 10000, color = org)) +
    scale_y_continuous(sec.axis = sec_axis(~.*10000, name = "National Membership")) +
    scale_color_brewer(palette = "Greys") +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal()
