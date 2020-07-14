# make correlation table to see which confounders I should control for in defense of disabled~wctu mechanism

# load packages
library(corrplot)
library(Hmisc)
library(haven)

# load data
source("git/women-postbellum/01-clean-map-data.R")
votes <- read_dta(file = "~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_08611/DS0001/08611-0001-Data.dta")[,c(1:3,110,115)]
names(votes) <- c("STATEICP","county_name","COUNTYICP","PresVoteTurnout1860","PctVoteRepublican1860")
# merge voting data with wctu_data_sf
wctu_data_sf$churches_per_capita <- wctu_data_sf$churches / wctu_data_sf$totpop
wctu_data_sf <- merge(x = wctu_data_sf, y = votes, by = c("STATEICP","COUNTYICP"), all.x = TRUE)
cor_data <- as.data.frame(wctu_data_sf)[,c(25,26,40,41,44:49,53,55:61,64:65,67:69,71:76,87,93,99,84,90,96,102,107:108,110,115,117,121,122,123,133,139,142,143,145:146,120)]

# lapply(cor_data, class)
# res <- cor(cor_data, use = "complete.obs")
# round(res, 2)
# corrplot(res, type = "upper", tl.col = "black", tl.srt = 45, method = "color")

res2 <- rcorr(as.matrix(cor_data))
flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        cor  =(cormat)[ut],
        p = pmat[ut]
    )
}
flattenCorrMatrix(res2$r, res2$P)
corrplot(res2$r, type="upper", p.mat = res2$P, sig.level = 0.01, insig = "blank", tl.col = "black", tl.cex = 1, na.label = "-")

