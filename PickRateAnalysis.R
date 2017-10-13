# Overwatch Pick Rate Analysis

# Load packages
library(rvest)
library(ggplot2)
library(grid)
library(reshape2)

# Load in the data frame; change the timestamp date to match the file that is saved
timestamp <- "10-12-2017"
filename <- paste0("Heroes_", timestamp, ".Rda")
load(filename)

# Convert percentages of pick rate into numeric rates
Heroes$PickRate <- Heroes$PickRate %>%
  gsub("%", "", .) %>%
  as.numeric()

# Add in hero difficulty ratings (taken manually from the game)
Heroes$Difficulty <- as.factor(c(3, 3, 1, 1, 2, 2, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 3, 3, 1, 1, 2, 2, 1, 1, 
                                 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3))

# Subset by mode
HeroQuick <- subset(Heroes, Mode == "Quickplay")
HeroComp <- subset(Heroes, Mode == "Competitive")

# Order by pick rate
HeroQuick <- HeroQuick[order(HeroQuick$PickRate, decreasing = TRUE), ]
HeroComp <- HeroComp[order(HeroComp$PickRate, decreasing = TRUE), ]

# Create data frame that will be populated with rankings
HeroList <- (data.frame(Name = character(25), Type = factor(25), RankQuick = numeric(25), RankComp = numeric(25)))
HeroList$Name <- unique(Heroes$Name)
HeroList$Type <- as.factor(Heroes$Type[seq(1,50,2)])

# Find ranking order of hero pick rates
for (i in 1:length(HeroList$Name)){
 HeroList$RankQuick[i] <- which(HeroQuick$Name == HeroList$Name[i])
 HeroList$RankComp[i] <- which(HeroComp$Name == HeroList$Name[i])
}

# Scatterplot contrasting pick rates in competitive vs. quickplay modes
p <- ggplot(data = HeroList, aes(x = RankQuick, y = RankComp, color = Type)) +
  geom_abline(intercept = 0, slope = -1, linetype="dashed", size = 1) + geom_point(size = 5) +
  scale_y_continuous(trans = "reverse", breaks = seq(1,25,1)) + scale_x_continuous(breaks = seq(1,25,1)) +
  labs(title = "Ranks of Pick Rates for Heroes in Overwatch Game Modes", x = "Quickplay Rank", y = "Competitive Rank") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label = Name), hjust = -0.30) +
  scale_color_manual(values=c("#8d8b8b", "#e08906", "#0180fe", "#3c4044"))

# Make sure all the label text is readable (allow it to go past the coordinate bounds)
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

# Kendall's Tau
ranks <- cbind(HeroList$RankQuick, HeroList$RankComp)
corr <- cor(ranks, method="kendall")

## Which heroes have the biggest difference?
HeroList$Diff <- HeroList$RankQuick - HeroList$RankComp
HeroList <- HeroList[order(HeroList$Diff, decreasing = TRUE), ]
HeroList

stable <- c("Orisa", "Bastion", "Mei", "Soldier: 76", "Doomfist", "Torbjörn", "Pharah", "Roadhog", "Ana", "Junkrat", "Sombra")
stableSet <- Heroes[Heroes$Name %in% stable, ]
stableSet <- stableSet[order(stableSet$PickRate, decreasing = TRUE), ]

high <- c("Reinhardt", "Lúcio", "Winston", "Zarya", "Zenyatta")
highSet <- Heroes[Heroes$Name %in% high, ]
highSet <- highSet[order(highSet$PickRate, decreasing = TRUE), ]

low <- c("Widowmaker", "Hanzo", "Genji", "Tracer", "McCree")
lowSet <- Heroes[Heroes$Name %in% low, ]
lowSet <- lowSet[order(lowSet$PickRate, decreasing = TRUE), ]

# 
page <- read_html("http://www.overbuff.com/roles")
types <-  page%>%
  html_nodes("td") %>%
  html_text()

TypesQuick <- (data.frame(Role = character(4), Popularity = character(4), Picked = character(4), WinRate = character(4), 
                     OnFire = character(4), EDRatio = character(4)))
TypesQuick$Role <- c(types[2], types[25], types[48], types[71])
TypesQuick$Popularity <- c(types[3], types[26], types[49], types[72])
TypesQuick$Picked <- c(types[4], types[27], types[50], types[73]) %>%
  gsub(" /game", "", .)
TypesQuick$WinRate <- c(types[5], types[28], types[51], types[74])
TypesQuick$OnFire <- c(types[6], types[29], types[52], types[75])
TypesQuick$EDRatio <- c(types[7], types[30], types[53], types[76]) %>%
  gsub("\n\n", "", .)

TypesQuick$Mode <- replicate(4, "Quickplay")


page <- read_html("http://www.overbuff.com/roles?mode=competitive")
types <-  page %>%
  html_nodes("td") %>%
  html_text()

TypesComp <- (data.frame(Role = character(4), Popularity = character(4), Picked = character(4), WinRate = character(4), 
                          OnFire = character(4), EDRatio = character(4)))
TypesComp$Role <- c(types[2], types[25], types[48], types[71])
TypesComp$Popularity <- c(types[3], types[26], types[49], types[72])
TypesComp$Picked <- c(types[4], types[27], types[50], types[73]) %>%
  gsub(" /game", "", .)
TypesComp$WinRate <- c(types[5], types[28], types[51], types[74])
TypesComp$OnFire <- c(types[6], types[29], types[52], types[75])
TypesComp$EDRatio <- c(types[7], types[30], types[53], types[76]) %>%
  gsub("\n\n", "", .)

TypesComp$Mode <- replicate(4, "Competitive")

Types <- rbind(TypesQuick, TypesComp)
Types <- Types[order(Types$Role), ]

# Reformat
Types$Role <- as.factor(Types$Role)
Types$Mode <- as.factor(Types$Mode)
Types$Picked <- as.numeric(Types$Picked)

# Plot
ggplot(data = Types, aes(x = Role, y = Picked, fill = Mode)) +
  geom_bar(stat = "identity", position = "dodge", color = "#3c4044") + 
  scale_fill_manual(values=c("#0180fe", "#e08906")) +
  labs(title = "Types of Heroes in Competitive and Quickplay Games", x = "Hero Type", y = "Picked per Game") +
  theme(plot.title = element_text(hjust = 0.5))
  
#
Types$Popularity <- gsub("%", "", Types$Popularity) %>%
  as.numeric()

ggplot(data = Types, aes(x = Mode, y = Popularity, fill = Role, label = Popularity)) +
  geom_bar(stat = "identity", position = "stack", color = "white") + 
  scale_fill_manual(values=c("#8d8b8b", "#e08906", "#0180fe", "#3c4044")) +
  labs(title = "Popualrity of Hero Types in Competitive and Quickplay Games", x = "Game Mode", y = "Popularity") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_text(size = 4, color = "white", position = position_stack(vjust = 0.5))
