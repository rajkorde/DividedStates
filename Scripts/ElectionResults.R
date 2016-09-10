library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(maptools)
library(ggthemes)
library(mapdata)
library(scales)


votes <- read_csv("Data/elec.csv")
votes <- votes %>% 
    separate(Year, c("Year", "President", sep = "-")) %>%
    select(Year, State, Party) %>%
    mutate(State = tolower(State), 
           Year = as.numeric(Year)) %>%
    filter(Year >= 1996, State!= "dist. of col.") %>%
    mutate(Party = factor(Party))


pop <- read_csv("Data/population.csv")
pop <- pop %>% mutate(State = tolower(State)) %>%
    select(State, Population)
    
r <- votes %>%
    group_by(State) %>%
    summarise(Vote = table(Party)[["Democratic"]] - 
                  table(Party)[["Republican"]]) %>%
    mutate(VoteNorm = ((Vote - 1)/2) + 4) %>%
    inner_join(pop, by = "State")

states <- map_data("state")
statesname <- data.frame(state.center, StateAbb = state.abb, 
                         State = tolower(state.name))

r <- merge(r, statesname, by = "State")
r <- r[!r$StateAbb %in% c("AK", "HI"),]

row.names(r) <- NULL

ggplot(r, aes(map_id = State)) +
    geom_map(aes(fill = VoteNorm), map = states, alpha = 0.8) +
    expand_limits(x = states$long, y = states$lat) +
    scale_fill_gradient2(low = muted("red"),  
                         mid = "lightgrey", high = muted("blue"),
                         midpoint = 3.5, 
                         limits = c(1, 6)) + 
    # coord_map("polyconic") +
    theme_map() +
    theme(plot.margin=margin(20, 20, 20, 20)) +
    theme(legend.position=c(0.85, 0.2)) +
    geom_text(aes(x=x, y=y, label=StateAbb, group=NULL), size=2) 

assignorder <- function(d, p, col, statecol = "State", reverse = FALSE) {
    s <- sort(p)
    if(reverse == TRUE) {
        d <- d[order(d[, col], decreasing = TRUE),] 
    } else {
        d <- d[order(d[, col]),]
    }
    name <- paste0(col, "Order")
    d[, name] <- s
    d[order(d[, statecol]),]
}

plotmap <- function(d, col, mid = 0, low = -6, high = 6) {
    ggplot(d, aes(map_id = State)) +
        geom_map(aes_string(fill = col), map = states) +
        expand_limits(x = states$long, y = states$lat) +
        scale_fill_gradient2(low = muted("red"),  
                         mid = "lightgrey", high = muted("blue"),
                         midpoint = mid, 
                         limits = c(low, high)) + 
        theme_map() +
        theme(plot.margin=margin(20, 20, 20, 20)) +
        theme(legend.position=c(0.85, 0.2)) +
        geom_text(aes(x=x, y=y, label=StateAbb, group=NULL), size=2) 
}

#maxdiff is 218
maxdiff <- (14 * 5) + (9 * 4) + (2 * 3) + (3 * 3) + (3 * 4) + (17 * 5)


n <- 10000
total <- 0
set.seed(420)
for(i in 1:n) {
    r <- sample(results$VoteSix)
    total <- total + (sum(abs(r - results$VoteSix)))
}
randomdiff <- round(total/n)
#113
randomdiff 

mindiff <- 0

save(r, maxdiff, mindiff, randomdiff, assignorder, plotmap,
     file = "Objects/cleandata.RData")
