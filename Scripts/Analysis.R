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

g1 <- ggplot(d, aes(map_id = State)) +
    geom_map(aes(fill = MedianIncome), map = states) +
    expand_limits(x = states$long, y = states$lat) +
     scale_fill_gradient2(low = muted("red"),  
                          mid = "lightgrey", high = muted("blue"),
                          midpoint = median(d$MedianIncome), 
                          limits = c(min(d$MedianIncome), max(d$MedianIncome))) + 
#    scale_fill_viridis() +
    ggtitle("Median Income") +
    theme_map() +
#    theme(plot.margin=margin(20, 20, 20, 20)) +
    theme(legend.position=c(0.85, 0.2)) +
    geom_text(aes(x=x, y=y, label=StateAbb, group=NULL), size=2)

g1 <- plotmap(d, "MedianIncome", mid = median(d$MedianIncome), 
        low = min(d$MedianIncome), high = max(d$MedianIncome))
plotmap(d, "Diff")

x <- d %>% select(State, VoteNorm, MedianIncomeOrder, Diff) %>%
    gather(Type, Norm, c(MedianIncomeOrder, VoteNorm)) %>%
    mutate(BarValue = rep(1, nrow(.)), 
           State = fct_rev(factor(State)),
           Type = fct_rev(factor(Type)))



ggplot(x, aes(State, BarValue, fill = Norm)) + 
    geom_bar(stat = "identity", alpha = 0.8) + 
    coord_flip() +
    scale_fill_gradient2(low = muted("red"),  
                         mid = "lightgrey", high = muted("blue"),
                         midpoint = 3.5, 
                         limits = c(1, 6)) +
    theme_minimal() +
    theme(legend.position="none") + 
    labs(x = "", y = "") +
    theme(axis.text.x = element_blank())

ggplot(d, aes(State, abs(Diff), fill = MedianIncomeOrder)) + 
    geom_bar(stat = "identity", alpha = 0.8) + 
    coord_flip() +
    scale_fill_gradient2(low = muted("red"),  
                         mid = "lightgrey", high = muted("blue"),
                         midpoint = 3.5, 
                         limits = c(1, 6)) +
    theme_minimal() 

ggplot(data.frame(Name = "Index", Value = index), aes(Name, Value, fill = Name)) + 
    geom_bar(stat = "identity") +
    expand_limits(y = c(0, 1)) + scale_y_continuous(limits=c(0, 1)) +
    geom_text(aes(label=round(Value * 100, 1), hjust=-0.5), size = 20) +
    coord_flip() +
    theme_classic() +
    labs(x = "", y = "") +
    theme(axis.text.y = element_blank(), axis.ticks = element_blank()) +
    theme(legend.position="right", legend.text = element_blank(),
          legend.key = element_blank()) +
    scale_fill_discrete(name = "New Legend Title") 
    
#    theme(panel.grid.major = element_blank())

g2 <- ggplot(x, aes(Type, State)) + 
    geom_tile(aes(fill = Norm), alpha = I(0.5)) +
    scale_fill_gradient2(low = "red",  
                         mid = "lightgrey", high = "blue",
                         midpoint = 3.5, 
                         limits = c(1, 6),
                         labels = c("voted republican\nlow income", 
                                    "", "", "", "", 
                                    "voted democrat\nhigh income")) +
    theme_minimal() +
    ggtitle("Vote Differential") +
    theme(legend.position="bottom", legend.title=element_blank()) + 
    labs(x = "", y = "") +
    theme(panel.grid.major = element_blank()) +
    theme(axis.text.y = element_text(margin=margin(0,-10,0,0)))


grid.arrange(g1 + theme(aspect.ratio = .67), g2, ncol=2, nrow=1, widths=c(4, 2), respect = FALSE)

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
