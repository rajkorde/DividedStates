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

g1 <- ggplot(r, aes(map_id = State)) +
    geom_map(aes(fill = VoteNorm), map = states, alpha = 0.8) +
    expand_limits(x = states$long, y = states$lat) +
    scale_fill_gradient2(low = muted("red"),  
                         mid = "lightgrey", high = muted("blue"),
                         midpoint = 3.5, 
                         limits = c(1, 6),
                         labels = c("all republican wins", 
                                    "", "", "", "", 
                                    "all democrat wins")) + 
    ggtitle("Voting History is last 5 elections") +
    theme_map() +
    theme(plot.margin=margin(20, 20, 20, 20)) +
    theme(legend.position=c(0.83, 0.2), legend.title=element_blank()) +
    geom_text(aes(x=x, y=y, label=StateAbb, group=NULL), size=2) +
    theme(title = element_text(size = 12, face = "bold"))

g2 <- ggplot(r, aes(Vote)) + 
    geom_histogram(bins = 11, fill = "mediumpurple") +
    theme_minimal() +
    ylab("Number of States") +
    xlab("") +
    ggtitle("Number of wins in last 5 elections") +
    scale_x_continuous(breaks = seq(-5, 5, 2), 
                       labels = c("all republican wins",
                                  "4 republican wins",
                                  "3 republican wins",
                                  "3 democrat wins",
                                  "4 democrat wins",
                                  "all democrat wins")) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) 

lay = cbind(c(1, 1, 2),
            c(1, 1, 2))
g <- grid.arrange(g1 + theme(aspect.ratio = .67), g2, 
                 layout_matrix = lay)

ggsave(filename = "Plots/ElectionResults.png", plot = g, 
       width = 3, height = 3, scale = 2.5)


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

#maxdiff is 218
maxdiff <- (14 * 5) + (9 * 4) + (2 * 3) + (3 * 3) + (3 * 4) + (17 * 5)


n <- 10000
total <- 0
set.seed(420)
for(i in 1:n) {
    r1 <- sample(r$VoteNorm)
    total <- total + (sum(abs(r1 - r$VoteNorm)))
}
randomdiff <- round(total/n)
#113
randomdiff 

mindiff <- 0

plotUSmap <- function(d, title, col, med, low, high, reverse = FALSE) {
    states <- map_data("state")
    
    if(reverse == FALSE) {
        lowcolor <- muted("red")
        highcolor <- muted("blue")
    } else {
        highcolor <- muted("red")
        lowcolor <- muted("blue")
    }
    
    ggplot(d, aes(map_id = State)) +
        geom_map(aes_string(fill = col), map = states) +
        expand_limits(x = states$long, y = states$lat) +
        scale_fill_gradient2(low = lowcolor,  
                             mid = "lightgrey", high = highcolor,
                             midpoint = med, 
                             limits = c(low, high)) + 
        ggtitle(title) +
        theme_map() +
        theme(legend.position=c(0.85, 0.2), legend.title = element_blank()) +
        geom_text(aes(x=x, y=y, label=StateAbb, group=NULL), size=2) +
        theme(title = element_text(size = 16, face = "bold"))
}

plottable <- function(x, type, state, norm, labs) {
    
    ggplot(x, aes_string(type, state)) + 
        geom_tile(aes_string(fill = norm), alpha = I(0.8)) +
        scale_fill_gradient2(low = muted("red"),  
                             mid = "lightgrey", high = muted("blue"),
                             midpoint = 3.5, 
                             limits = c(1, 6),
                             labels = labs) +
        theme_minimal() +
        ggtitle("Vote Differential") +
        theme(legend.position="bottom", legend.title=element_blank()) + 
        labs(x = "", y = "") +
        theme(panel.grid.major = element_blank()) +
        theme(axis.text.y = element_text(margin=margin(0,-10,0,0))) +
        theme(title = element_text(size = 12, face = "bold"))
}

plotmeter <- function(index) {
    
    if(index > 0 & index < .25) {
        schism <- "Low"
    } else if(index > .25 & index < .50) {
        schism <- "Moderate" 
    } else if(index > .50 & index < .75) {
        schism <- "High" 
    } else if(index > .75 & index < 1) {
        schism <- "Extremely High" 
    }
    
    ggplot(data.frame(Name = "Index", Value = round(index * 100, 1)), 
           aes(Name, Value)) + 
        geom_bar(stat = "identity", fill = "mediumpurple") +
        expand_limits(y = c(0, 100)) +
        geom_text(aes(label = paste0(Value, " (", schism, ")"), 
                                    hjust=-0.1), size = 10) +
        coord_flip() +
        theme_classic() +
        labs(x = "", y = "0-25: Low, 25-50: Moderate, 50-75: High, 75-100: Extremely High") +
        theme(axis.text.y = element_blank(), axis.ticks = element_blank()) +
        ggtitle("Schismometer") +
        theme(axis.title.x = element_text(vjust = 0.3, size = 10)) +
        theme(title = element_text(size = 12, face = "bold"))
}

plotall <- function(g1, g2, g3) {
    lay = cbind(c(1, 1, 1, 1, 3),
                c(1, 1, 1, 1, 3),
                c(2, 2, 2, 2, 2))
    g = grid.arrange(g1 + theme(aspect.ratio = .67), g2, g3, 
                     layout_matrix = lay)
}


#save(r, maxdiff, mindiff, randomdiff, assignorder, plotmap,
#     file = "Objects/cleandata.RData")
