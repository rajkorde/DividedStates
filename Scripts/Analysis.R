library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(maptools)
library(ggthemes)
library(mapdata)
library(scales)
library(viridis)
library(gridExtra)
library(forcats)
library(xlsx)

theme_set(theme_bw())

#Median Income
d <- read.csv(file = "Data/medianincome.csv")

d <- d %>% mutate(MedianIncome = as.numeric(gsub(",", "", MedianIncome)),
                  State = tolower(State)) %>%
    filter(!State %in% c("hawaii", "alaska")) %>%
    do(assignorder(d = ., p = r$VoteNorm, col = "MedianIncome")) %>%
    mutate(Diff = MedianIncomeOrder - r$VoteNorm) %>%
    inner_join(r, by = "State")

diff = sum(abs(d$Diff))
index = 1 - (diff/randomdiff)

g1 <- plotUSmap(d, "Median Income", col = "MedianIncome", 
                med = median(d$MedianIncome), 
                low = min(d$MedianIncome), 
                high = max(d$MedianIncome))

x <- d %>% select(State, VoteNorm, MedianIncomeOrder, Diff) %>%
    gather(Type, Norm, c(MedianIncomeOrder, VoteNorm)) %>%
    mutate(BarValue = rep(1, nrow(.)), 
           State = fct_rev(factor(State)),
           Type = fct_rev(factor(Type, labels = c("Income", "VoteHistory"))))

g2 <- plottable(x, "Type", "State", "Norm", labs = c("voted republican\nlow income", 
                                               "", "", "", "", 
                                               "voted democrat\nhigh income"))
g3 <- plotmeter(index)

g <- plotall(g1, g2, g3)
ggsave(filename = "Plots/MedianIncome.png", plot = g, 
       width = 4, height = 3, scale = 2.5)

#Beer wine index
library(xlsx)
library(stringr)

d <- read.xlsx(file = "Data/BeerWinePerCapita.xlsx", sheetName = "CombinedT")

d <- d %>% select(State = STATE, BWIndex) %>%
    mutate(State = tolower(State)) %>%
    filter(!State %in% c("alaska", "hawaii", "district of columbia")) %>%
    mutate(State = str_trim(State)) %>%
    do(assignorder(d = ., p = r$VoteNorm, col = "BWIndex")) %>%
    mutate(Diff = BWIndexOrder - r$VoteNorm) %>%
    inner_join(r, by = "State")

diff = sum(abs(d$Diff))
index = 1 - (diff/randomdiff)

g1 <- plotUSmap(d, "Beer Wine Preference", col = "BWIndex", 
                med = median(d$BWIndex), 
                low = min(d$BWIndex), 
                high = max(d$BWIndex))

x <- d %>% select(State, VoteNorm, BWIndexOrder, Diff) %>%
    gather(Type, Norm, c(BWIndexOrder, VoteNorm)) %>%
    mutate(BarValue = rep(1, nrow(.)), 
           State = fct_rev(factor(State)),
           Type = fct_rev(factor(Type, 
                                 labels = c("BeerWineIndex", "VoteHistory"))))

g2 <- plottable(x, "Type", "State", "Norm", labs = c("voted republican\nlikes beer", 
                                                     "", "", "", "", 
                                                     "voted democrat\nlikes wine"))
g3 <- plotmeter(index)

g <- plotall(g1, g2, g3)
ggsave(filename = "Plots/BWPref.png", plot = g, 
       width = 4, height = 3, scale = 2.5)

#obestiy
d <- read_csv("Data/obesity.csv")
d <- select(d, State = `State value`, Obesity = `Adultobesity number`) %>%
    mutate(Obesity = as.numeric(gsub("%", "", Obesity))) %>%
    mutate(State = tolower(State)) %>%
    filter(!State %in% c("district of columbia", "alaska", "hawaii")) %>%
    arrange(State) %>%
    do(assignorder(d = data.frame(.), p = r$VoteNorm, 
                   col = "Obesity", reverse = TRUE)) %>%
    mutate(Diff = ObesityOrder - r$VoteNorm) %>%
    inner_join(r, by = "State")

diff = sum(abs(d$Diff))
index = 1 - (diff/randomdiff)

g1 <- plotUSmap(d, "Obesity per capita", col = "Obesity", 
                med = median(d$Obesity), 
                low = min(d$Obesity), 
                high = max(d$Obesity),
                reverse = TRUE)

x <- d %>% select(State, VoteNorm, ObesityOrder, Diff) %>%
    gather(Type, Norm, c(ObesityOrder, VoteNorm)) %>%
    mutate(BarValue = rep(1, nrow(.)), 
           State = fct_rev(factor(State)),
           Type = fct_rev(factor(Type, 
                                 labels = c("Obesity", "VoteHistory"))))

g2 <- plottable(x, "Type", "State", "Norm", labs = c("voted republican\nmore obese", 
                                                     "", "", "", "", 
                                                     "voted democrat\nless obese"))
g3 <- plotmeter(index)

g <- plotall(g1, g2, g3)
ggsave(filename = "Plots/Obesity.png", plot = g, 
       width = 4, height = 3, scale = 2.5)

#abortion data

d <- read_csv("Data/abortion.csv")
d <- select(d, State = Link, Rate) %>%
    mutate(State = tolower(State)) %>%
    filter(!State %in% c("district of columbia", "alaska", "hawaii")) %>%
    arrange(State) %>%
    do(assignorder(d = data.frame(.), p = r$VoteNorm, 
                   col = "Rate")) %>%
    mutate(Diff = RateOrder - r$VoteNorm) %>%
    inner_join(r, by = "State")

diff = sum(abs(d$Diff))
index = 1 - (diff/randomdiff)

g1 <- plotUSmap(d, "Abortion Rate", col = "Rate", 
                med = median(d$Rate), 
                low = min(d$Rate), 
                high = max(d$Rate))

x <- d %>% select(State, VoteNorm, RateOrder, Diff) %>%
    gather(Type, Norm, c(RateOrder, VoteNorm)) %>%
    mutate(BarValue = rep(1, nrow(.)), 
           State = fct_rev(factor(State)),
           Type = fct_rev(factor(Type, 
                                 labels = c("AbortionRate", "VoteHistory"))))

g2 <- plottable(x, "Type", "State", "Norm", labs = c("voted republican\nless rate", 
                                                     "", "", "", "", 
                                                     "voted democrat\nhigh rate"))
g3 <- plotmeter(index)

g <- plotall(g1, g2, g3)
ggsave(filename = "Plots/AbortionRate.png", plot = g, 
       width = 4, height = 3, scale = 2.5)


#abortion clinics
d <- read_csv("Data/abortion.csv")
d <- select(d, State = Link, Clinics) %>%
    mutate(State = tolower(State)) %>%
    filter(!State %in% c("district of columbia", "alaska", "hawaii")) %>%
    arrange(State) %>%
    inner_join(r, by = "State") %>%
    mutate(ClinicRate = (Clinics*1000)/Population) %>%
    do(assignorder(d = data.frame(.), p = r$VoteNorm, 
                   col = "ClinicRate")) %>%
    mutate(Diff = ClinicRateOrder - r$VoteNorm)
    

diff = sum(abs(d$Diff))
index = 1 - (diff/randomdiff)

g1 <- plotUSmap(d, "Abortion Clinics per 1000 people", col = "ClinicRate", 
                med = median(d$ClinicRate), 
                low = min(d$ClinicRate), 
                high = max(d$ClinicRate))

x <- d %>% select(State, VoteNorm, ClinicRateOrder, Diff) %>%
    gather(Type, Norm, c(ClinicRateOrder, VoteNorm)) %>%
    mutate(BarValue = rep(1, nrow(.)), 
           State = fct_rev(factor(State)),
           Type = fct_rev(factor(Type, 
                                 labels = c("Clinics", "VoteHistory"))))

g2 <- plottable(x, "Type", "State", "Norm", 
                labs = c("voted republican\nless clinics", "", "", "", "", 
                         "voted democrat\nmore clinics"))
g3 <- plotmeter(index)

g <- plotall(g1, g2, g3)
ggsave(filename = "Plots/AbortionClinics.png", plot = g, 
       width = 4, height = 3, scale = 2.5)

#hate groups

d <- read_csv("Data/Hategroups.csv")
d <- mutate(d, State = tolower(State)) %>%
    arrange(State) %>%
    inner_join(r, by = "State") %>%
    mutate(HateGroupRate = (HateGroups*1000)/Population) %>%
    do(assignorder(d = data.frame(.), p = r$VoteNorm, 
                   col = "HateGroupRate", reverse = TRUE)) %>%
    mutate(Diff = HateGroupRateOrder - r$VoteNorm)


diff = sum(abs(d$Diff))
index = 1 - (diff/randomdiff)

g1 <- plotUSmap(d, "Hate groups per 1000 people", col = "HateGroupRate", 
                med = median(d$HateGroupRate), 
                low = min(d$HateGroupRate), 
                high = max(d$HateGroupRate), 
                reverse = TRUE)

x <- d %>% select(State, VoteNorm, HateGroupRateOrder, Diff) %>%
    gather(Type, Norm, c(HateGroupRateOrder, VoteNorm)) %>%
    mutate(BarValue = rep(1, nrow(.)), 
           State = fct_rev(factor(State)),
           Type = fct_rev(factor(Type, 
                                 labels = c("HateGroups", "VoteHistory"))))

g2 <- plottable(x, "Type", "State", "Norm", 
                labs = c("voted republican\nmore hate groups", "", "", "", "", 
                         "voted democrat\nless hate groups"))
g3 <- plotmeter(index)

g <- plotall(g1, g2, g3)
ggsave(filename = "Plots/Hategroups.png", plot = g, 
       width = 4, height = 3, scale = 2.5)

