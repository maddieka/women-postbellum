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
wctu_data_sf$has_union <- ifelse(wctu_data_sf$count_unions>0, yes = 1, no = 0)

cor_data <- as.data.frame(wctu_data_sf)[ ,c("kmw","mainbattlenum","urb860","urb25", "farmval", "homemfg","mfgestab","mfgcap","mfgout","realest","churches",
                                           "water","rail","fbwtot","Female_lfp_1860","Male_lfp_1860","log_totpop",
                                           #"Female_lfp_1870","Male_lfp_1870","Female_lfp_1880","Male_lfp_1880","Female_lfp_1900","Male_lfp_1900",
                                           "totpop","pct_urb860","has_union","died_battle","state_terr","total_soldiers", "pct_pop_disabledx100",#"wounded","died","regout",
                                           "pct_pop_diedx100","pct_pop_disabwound","pct_pop_soldiersx100","pct_pop_mainbattle","pct_pop_whole","pct_pop_woundedx100","pct_pop_deserted","pct_pop_regoutx100",
                                           "pop_per_sqmi860","churches_per_capita","PresVoteTurnout1860","PctVoteRepublican1860","pct_pop_disabled")
                                        ]
# cor_data$log_urb860 <- log(cor_data$urb860 + 1)
# cor_data$log_urb25 <- log(cor_data$urb25 + 1)
# cor_data$log_farmval <- log(cor_data$farmval + 1)
# cor_data$log_mfgcap <- log(cor_data$mfgcap + 1)
# cor_data$log_mfgout <- log(cor_data$mfgout + 1)
# cor_data$log_realest <- log(cor_data$realest + 1)
# cor_data$log_fbwtot <- log(cor_data$fbwtot + 1)

# cor_data <- subset(cor_data, select =  -c(urb860, urb25, farmval, mfgcap, mfgout,realest, fbwtot))
# lapply(cor_data, class)
# res <- cor(cor_data, use = "complete.obs")
# round(res, 2)
# corrplot(res, type = "upper", tl.col = "black", tl.srt = 45, method = "color")

##### ugly correlation things to ignore #####
# res2 <- rcorr(as.matrix(cor_data))
# flattenCorrMatrix <- function(cormat, pmat) {
#     ut <- upper.tri(cormat)
#     data.frame(
#         row = rownames(cormat)[row(cormat)[ut]],
#         column = rownames(cormat)[col(cormat)[ut]],
#         cor  =(cormat)[ut],
#         p = pmat[ut]
#     )
# }
# flattenCorrMatrix(res2$r, res2$P)
# corrplot(res2$r, method = "color", type="upper", p.mat = res2$P, sig.level = 0.01, insig = "blank", tl.col = "black", tl.cex = .75, na.label = "-")

##### robustness table - syp september draft version (may look different from pdf based on which state/year crossections are selected in 01-clean-map-data.R) #####
cor_data$pct_urb860x100 <- cor_data$pct_urb860*100

econ <- lm(pct_pop_disabled ~ scale(homemfg) + scale(mfgestab) +scale(realest)+scale(mfgout)+scale(mfgcap) + scale(farmval) , cor_data)
county <- lm(pct_pop_disabled ~ water + rail + scale(totpop) + scale(pop_per_sqmi860) + scale(pop_per_sqmi860) + scale(urb860) + scale(urb25), cor_data)
culture <- lm(pct_pop_disabled ~ scale(churches_per_capita) + scale(PresVoteTurnout1860) + scale(PctVoteRepublican1860)+scale(Female_lfp_1860) + scale(Male_lfp_1860) +scale(fbwtot), cor_data)
all <- lm(pct_pop_disabled ~ water + rail + scale(totpop) + scale(pop_per_sqmi860) + scale(urb860) + scale(urb25) +scale(homemfg) + scale(mfgestab) +scale(realest)+scale(mfgout)+scale(mfgcap) + scale(farmval) +scale(churches_per_capita) + scale(PresVoteTurnout1860) + scale(PctVoteRepublican1860)+scale(Female_lfp_1860) + scale(Male_lfp_1860) +scale(fbwtot), cor_data)
econ2 <- lm(has_union ~ scale(homemfg) + scale(mfgestab) +scale(realest)+scale(mfgout)+scale(mfgcap) + scale(farmval) , cor_data)
county2 <- lm(has_union ~ water + rail + scale(totpop) + scale(pop_per_sqmi860) + scale(pop_per_sqmi860) + scale(urb860) + scale(urb25), cor_data)
culture2 <- lm(has_union ~ scale(churches_per_capita) + scale(PresVoteTurnout1860) + scale(PctVoteRepublican1860)+scale(Female_lfp_1860) + scale(Male_lfp_1860) +scale(fbwtot), cor_data)
all2 <- lm(has_union ~ water + rail + scale(totpop) + scale(pop_per_sqmi860) + scale(urb860) + scale(urb25) +scale(homemfg) + scale(mfgestab) +scale(realest)+scale(mfgout)+scale(mfgcap) + scale(farmval) +scale(churches_per_capita) + scale(PresVoteTurnout1860) + scale(PctVoteRepublican1860)+scale(Female_lfp_1860) + scale(Male_lfp_1860) +scale(fbwtot), cor_data)


stargazer(culture, county, econ, all, all2, type = "text",
          df = FALSE, font.size = "small", report = "vc*", omit.stat = c("f", "ser", "rsq"),
          label = "county_chars",
          # dep.var.labels = c("Percent disabled", "Has WCTU"),
          # covariate.labels = c("Churches per capita",
          #                      "Voter turnout$^\\dagger$",
          #                      "Percent Republican votes$^\\dagger$",
          #                      "Female LFP",
          #                      "Male LFP",
          #                      "Foreign born white population",
          #                      "Water transport access$^\\ddagger$",
          #                      "Rail access$^\\ddagger$",
          #                      "Total population",
          #                      "Population per sq mile",
          #                      "Population in places 2,500 +",
          #                      "Population in places 25,000 +",
          #                      "Value of domestic manufacurers",
          #                      "Manufacturing establishments",
          #                      "Value of real estate",
          #                      "Value of manufacturing output",
          #                      "Manufacturing capital",
          #                      "Cash value of farms"),
          notes = c("$^\\dagger$1860 Presidential Election", "$^\\ddagger$Dummy variable; not scaled for interpretation by standard deviation.")
)

##### add to robustness table for iv motivation #####

## exclusion restriciton
all <- lm(pct_pop_disabled ~ water + rail + scale(totpop) + scale(pop_per_sqmi860) + scale(urb860) + scale(urb25) +scale(homemfg) + scale(mfgestab) +scale(realest)+scale(mfgout)+scale(mfgcap) + scale(farmval) +scale(churches_per_capita) + scale(PresVoteTurnout1860) + scale(PctVoteRepublican1860)+scale(Female_lfp_1860) + scale(Male_lfp_1860) +scale(fbwtot) + scale(total_soldiers), cor_data)
all2 <- lm(has_union ~ water + rail + scale(totpop) + scale(pop_per_sqmi860) + scale(urb860) + scale(urb25) +scale(homemfg) + scale(mfgestab) +scale(realest)+scale(mfgout)+scale(mfgcap) + scale(farmval) +scale(churches_per_capita) + scale(PresVoteTurnout1860) + scale(PctVoteRepublican1860)+scale(Female_lfp_1860) + scale(Male_lfp_1860) +scale(fbwtot) + scale(total_soldiers), cor_data)
all3 <- lm(log(kmw + 1) ~ water + rail + scale(totpop) + scale(pop_per_sqmi860) + scale(urb860) + scale(urb25) +scale(homemfg) + scale(mfgestab) +scale(realest)+scale(mfgout)+scale(mfgcap) + scale(farmval) +scale(churches_per_capita) + scale(PresVoteTurnout1860) + scale(PctVoteRepublican1860)+scale(Female_lfp_1860) + scale(Male_lfp_1860) +scale(fbwtot) + scale(total_soldiers), cor_data)
all4 <- lm(mainbattlenum ~ water + rail + scale(totpop) + scale(pop_per_sqmi860) + scale(urb860) + scale(urb25) +scale(homemfg) + scale(mfgestab) +scale(realest)+scale(mfgout)+scale(mfgcap) + scale(farmval) +scale(churches_per_capita) + scale(PresVoteTurnout1860) + scale(PctVoteRepublican1860)+scale(Female_lfp_1860) + scale(Male_lfp_1860) +scale(fbwtot) + scale(total_soldiers), cor_data)
all5 <- lm(log(died_battle + 1) ~ water + rail + scale(totpop) + scale(pop_per_sqmi860) + scale(urb860) + scale(urb25) +scale(homemfg) + scale(mfgestab) +scale(realest)+scale(mfgout)+scale(mfgcap) + scale(farmval) +scale(churches_per_capita) + scale(PresVoteTurnout1860) + scale(PctVoteRepublican1860)+scale(Female_lfp_1860) + scale(Male_lfp_1860) +scale(fbwtot) + scale(total_soldiers), cor_data)

summary(all)
stargazer(all)

stargazer(all, all2, all3, all4, all5, type = "text",
          df = FALSE, font.size = "small", report = "vc*", omit.stat = c("f", "ser", "rsq"),
          label = "county_chars",
          # # dep.var.labels = c("Percent disabled", "Has WCTU", "KMW","MBN"),
          # #
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


## instrumental relevance
cor_data_complete <-
  cor_data %>%
  select(kmw, pct_pop_disabledx100, pct_pop_regoutx100, pct_pop_woundedx100, pct_pop_diedx100, pct_pop_soldiersx100, PctVoteRepublican1860, PresVoteTurnout1860, log_totpop, farmval, mfgcap, urb860, state_terr, has_union) %>%
  na.omit

test1 <- lm(pct_pop_disabledx100 ~ log(kmw + 1) + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 +
              PctVoteRepublican1860 + PresVoteTurnout1860 + log(mfgcap+1) + log(farmval+1) + log(urb860+1) +
              state_terr + log_totpop , data = cor_data_complete)
test2 <- lm(pct_pop_disabledx100 ~ mainbattlenum + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + PctVoteRepublican1860 + PresVoteTurnout1860 + log_totpop + log(mfgcap+1) + log(farmval+1) + log(urb860+1) + state_terr, cor_data_complete)
test3 <- lm(pct_pop_disabledx100 ~ log(died_battle + 1) + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + PctVoteRepublican1860 + PresVoteTurnout1860 + log_totpop + log(mfgcap+1) + log(farmval+1) + log(urb860+1) + state_terr, cor_data_complete)

stargazer(test1, test2, test3, type = "text"
          )

## iv results
d.hat.kmw <- test1$fitted.values

tsls2.kmw <- lm(has_union ~ d.hat.kmw + pct_pop_regoutx100 + pct_pop_woundedx100 + pct_pop_diedx100 + pct_pop_soldiersx100 + PctVoteRepublican1860 + PresVoteTurnout1860 + log_totpop + log(mfgcap+1) + log(farmval+1) + log(urb860+1) + state_terr, data = cor_data_complete)
# tsls2.mbn <- lm(has_union ~ d.hat.mbn + log(totpop+1) + state_terr, cor_data_complete)
# tsls2.died <- lm(has_union ~ d.hat.died + log(totpop+1) + state_terr, cor_data_complete)

stargazer(tsls2.kmw,  type = "text")

