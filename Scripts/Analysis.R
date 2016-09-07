library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(maptools)
library(ggthemes)
library(mapdata)
library(scales)


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

plotmap(d, "MedianIncome", mid = median(d$MedianIncome), 
        low = min(d$MedianIncome), high = max(d$MedianIncome))
plotmap(d, "Diff")

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

plotmap(d, "BWIndex", mid = median(d$BWIndex), 
        low = min(d$BWIndex), high = max(d$BWIndex))
plotmap(d, "Diff")

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

plotmap(d, "BWIndex", mid = median(d$BWIndex), 
        low = min(d$BWIndex), high = max(d$BWIndex))
plotmap(d, "Diff")

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

plotmap(d, "Rate", mid = median(d$Rate), 
        low = min(d$Rate), high = max(d$Rate))
plotmap(d, "Diff")


#clinics
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

plotmap(d, "ClinicRate", mid = median(d$ClinicRate), 
        low = min(d$ClinicRate), high = max(d$ClinicRate))
plotmap(d, "Diff")
