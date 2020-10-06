# make correlation table to see which confounders I should control for in defense of disabled~wctu mechanism

# load packages
library(corrplot)
library(Hmisc)
library(haven)

# load data
source("~/git/women-postbellum/01-clean-map-data.R")
votes <- read_dta(file = "~/Documents/Pitt/Projects/women_civil_war/data/ICPSR_08611/DS0001/08611-0001-Data.dta")[,c(1:3,110,115)]
names(votes) <- c("STATEICP","county_name","COUNTYICP","PctVoteRepublican1860","PresVoteTurnout1860")
votes[votes == max(votes$PctVoteRepublican1860)] <- NA # 999.9 is code for NA
votes$PresVoteTurnout1860[votes$PresVoteTurnout1860 > 100] <- NA # percentages over 100% are obviously invalid, so make NA
# merge voting data with wctu_data_sf
wctu_data_sf$churches_per_capita <- wctu_data_sf$churches / wctu_data_sf$totpop
wctu_data_sf <- merge(x = wctu_data_sf, y = votes, by = c("STATEICP","COUNTYICP"), all.x = TRUE)
wctu_data_sf$has_union <- ifelse(wctu_data_sf$count_unions>0, yes = 1, no = 2)
cor_data <- as.data.frame(wctu_data_sf)[,c("kmw","mainbattlenum","urb860","urb25", "farmval", "homemfg","mfgestab","mfgcap","mfgout","realest","churches","water","rail","fbwtot","Female_lfp_1860","Male_lfp_1860",
                                           #"Female_lfp_1870","Male_lfp_1870","Female_lfp_1880","Male_lfp_1880","Female_lfp_1900","Male_lfp_1900",
                                           "totpop","pct_urb860","has_union",
                                           "pct_pop_died","pct_pop_disabwound","pct_pop_soldiers","pct_pop_mainbattle","pct_pop_whole","pct_pop_wounded","pct_pop_deserted","pct_pop_regout",
                                           "pop_per_sqmi860","churches_per_capita","PresVoteTurnout1860","PctVoteRepublican1860","pct_pop_disabled")]
cor_data$log_urb860 <- log(cor_data$urb860 + 1)
cor_data$log_urb25 <- log(cor_data$urb25 + 1)
cor_data$log_farmval <- log(cor_data$farmval + 1)
cor_data$log_mfgcap <- log(cor_data$mfgcap + 1)
cor_data$log_mfgout <- log(cor_data$mfgout + 1)
cor_data$log_realest <- log(cor_data$realest + 1)
cor_data$log_fbwtot <- log(cor_data$fbwtot + 1)

# cor_data <- subset(cor_data, select =  -c(urb860, urb25, farmval, mfgcap, mfgout,realest, fbwtot))
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
corrplot(res2$r, method = "color", type="upper", p.mat = res2$P, sig.level = 0.01, insig = "blank", tl.col = "black", tl.cex = .75, na.label = "-")

cor_data$pct_urb860x100 <- cor_data$pct_urb860*100

econ <- lm(pct_pop_disabled ~ scale(homemfg) + scale(mfgestab) +scale(realest)+scale(mfgout)+scale(mfgcap) + scale(farmval) , cor_data)
county <- lm(pct_pop_disabled ~ water + rail + scale(totpop) + scale(pop_per_sqmi860) + scale(pop_per_sqmi860) + scale(urb860) + scale(urb25), cor_data)
culture <- lm(pct_pop_disabled ~ scale(churches_per_capita) + scale(PresVoteTurnout1860) + scale(PctVoteRepublican1860)+scale(Female_lfp_1860) + scale(Male_lfp_1860) +scale(fbwtot), cor_data)
all <- lm(pct_pop_disabled ~ water + rail + scale(totpop) + scale(pop_per_sqmi860) + scale(urb860) + scale(urb25) +scale(homemfg) + scale(mfgestab) +scale(realest)+scale(mfgout)+scale(mfgcap) + scale(farmval) +scale(churches_per_capita) + scale(PresVoteTurnout1860) + scale(PctVoteRepublican1860)+scale(Female_lfp_1860) + scale(Male_lfp_1860) +scale(fbwtot), cor_data)
econ2 <- lm(has_union ~ scale(homemfg) + scale(mfgestab) +scale(realest)+scale(mfgout)+scale(mfgcap) + scale(farmval) , cor_data)
county2 <- lm(has_union ~ water + rail + scale(totpop) + scale(pop_per_sqmi860) + scale(pop_per_sqmi860) + scale(urb860) + scale(urb25), cor_data)
culture2 <- lm(has_union ~ scale(churches_per_capita) + scale(PresVoteTurnout1860) + scale(PctVoteRepublican1860)+scale(Female_lfp_1860) + scale(Male_lfp_1860) +scale(fbwtot), cor_data)
all2 <- lm(has_union ~ water + rail + scale(totpop) + scale(pop_per_sqmi860) + scale(urb860) + scale(urb25) +scale(homemfg) + scale(mfgestab) +scale(realest)+scale(mfgout)+scale(mfgcap) + scale(farmval) +scale(churches_per_capita) + scale(PresVoteTurnout1860) + scale(PctVoteRepublican1860)+scale(Female_lfp_1860) + scale(Male_lfp_1860) +scale(fbwtot), cor_data)

sd(cor_data$PresVoteTurnout1860, na.rm = TRUE)
sd(cor_data$realest, na.rm = TRUE)
sd(cor_data$PctVoteRepublican1860, na.rm = TRUE)
#county_chars <- lm(pct_pop_disabled ~ ., cor_data)

test <- all
test$coefficients[1] <- NA
test$coefficients[2] <- NA
test$coefficients[3] <- NA
test$coefficients[4] <- round(sd(cor_data$totpop, na.rm = TRUE), digits = 0)
test$coefficients[5] <- round(sd(cor_data$pop_per_sqmi860, na.rm = TRUE), digits = 0)
test$coefficients[6] <- round(sd(cor_data$urb860, na.rm = TRUE), digits = 0)
test$coefficients[7] <- round(sd(cor_data$urb25, na.rm = TRUE), digits = 0)
test$coefficients[8] <- round(sd(cor_data$homemfg, na.rm = TRUE), digits = 0)
test$coefficients[9] <- round(sd(cor_data$mfgestab, na.rm = TRUE), digits = 0)
test$coefficients[10] <- round(sd(cor_data$realest, na.rm = TRUE), digits = 0)
test$coefficients[11] <- round(sd(cor_data$mfgout, na.rm = TRUE), digits = 0)
test$coefficients[12] <- round(sd(cor_data$mfgcap, na.rm = TRUE), digits = 0)
test$coefficients[13] <- round(sd(cor_data$farmval, na.rm = TRUE), digits = 0)
test$coefficients[14] <- round(sd(cor_data$churches_per_capita, na.rm = TRUE), digits = 0)
test$coefficients[15] <- round(sd(cor_data$PresVoteTurnout1860, na.rm = TRUE), digits = 0)
test$coefficients[16] <- round(sd(cor_data$PctVoteRepublican1860, na.rm = TRUE), digits = 0)
test$coefficients[17] <- round(sd(cor_data$Female_lfp_1860, na.rm = TRUE), digits = 0)
test$coefficients[18] <- round(sd(cor_data$Male_lfp_1860, na.rm = TRUE), digits = 0)
test$coefficients[19] <- round(sd(cor_data$fbwtot, na.rm = TRUE), digits = 0)

stargazer(culture, county, econ, all, all2, #type = "text",
          df = FALSE, font.size = "small", report = "vc*", omit.stat = c("f", "ser", "rsq"),
          label = "county_chars",
          dep.var.labels = c("Percent disabled", "Has WCTU"),

          covariate.labels = c("Churches per capita",
                               "Voter turnout$^\\dagger$",
                               "Percent Republican votes$^\\dagger$",
                               "Female LFP",
                               "Male LFP",
                               "Foreign born white population",
                               "Water transport access$^\\ddagger$",
                               "Rail access$^\\ddagger$",
                               "Total population",
                               "Population per sq mile",
                               "Population in places 2,500 +",
                               "Population in places 25,000 +",
                               "Value of domestic manufacurers",
                               "Manufacturing establishments",
                               "Value of real estate",
                               "Value of manufacturing output",
                               "Manufacturing capital",
                               "Cash value of farms"),
          notes = c("$^\\dagger$1860 Presidential Election", "$^\\ddagger$Dummy variable; not scaled for interpretation by standard deviation.")
)


