# Scrape Overbuff.com Data for the past month on PC

# install.packages("rvest", dependecies = TRUE)
library(rvest)

# Read the HTML on the main page
URL <- "https://www.overbuff.com/heroes"
AllHeroes <- read_html(URL)

# Get the list of hero names, as used in their respective URLs (so take out spaces and special characters)
HeroList <- AllHeroes %>%
  html_nodes("td.r-none-mobile") %>%
  html_text() %>%
  sort() %>%
  gsub("Offense|Defense|Tank|Support" , "", .) %>%
  gsub("[. \t :]", "", .) %>%
  gsub("ö", "o", .) %>%
  gsub("ú", "u", .) %>%
  tolower()

# Get URLs for all the characters
pages <- character(0)
modes <- c("quickplay", "competitive")
for (i in 1:length(HeroList)){
  page1 <- paste0(URL, "/", HeroList[i], "?mode=", modes[1])
  page2 <- paste0(URL, "/", HeroList[i], "?mode=", modes[2])
    
  pages <- c(pages, page1, page2)
}

# Create a data frame for all of the heroes
Heroes <- (data.frame(Name=character(0), Type = character(0), Tags = character(0), Mode = character(0),
                    ElimRatio = numeric(0), OnFire = numeric(0), PickRate = numeric(0), WinRate = numeric(0)))

for (i in 1:length(pages)){
  # Create a temporary data frame for one hero/game mode
  Hero <- (data.frame(Name=character(1), Type = character(1), Tags = character(1), Mode = character(1),
                      ElimRatio = numeric(1), OnFire = numeric(1), PickRate = numeric(1), WinRate = numeric(1)))
  
  # Read the html data from each hero URL
  HeroURL <- pages[i]
  HeroText <- read_html(HeroURL)
  
  # Get the hero name and remove the hero type, since that will be in a different column
  Hero$Name <-  HeroText %>%
    html_node("h1") %>%
    html_text() %>%
    gsub("Offense|Defense|Tank|Support" , "", .)
  
  # Get the hero type (offense, defense, tank, or support)
  Hero$Type <- HeroText %>%
    html_node(".layout-header-primary-bio small") %>%
    html_text()
  
  # Get the tags that Overbuff includes on each hero (e.g. healer, stationary, flanker, etc.)
  Hero$Tags <-  HeroText %>%
    html_node(".smaller") %>%
    html_text()
  
  # Include the game mode so we know which stats are from what game type
  if (i %% 2){
    Hero$Mode <- "Quickplay"
  } else{
    Hero$Mode <- "Competitive"
  }
  
  # Get the hero's elimination ratio
  Hero$ElimRatio <- HeroText %>%
    html_node("dl:nth-child(1) dd") %>%
    html_text() %>%
    as.numeric
  
  # Get the hero's on fire percentage
  Hero$OnFire <- HeroText %>%
    html_node(".layout-header-secondary .color-stat-loss") %>%
    html_text()
  
  # Get the hero's pick rate
  Hero$PickRate <- HeroText %>%
    html_node(".color-stat-game") %>%
    html_text()
  
  # Get the hero's win rate
  Hero$WinRate <- HeroText %>%
    html_node(".color-stat-win") %>%
    html_text()
  
  # Combine the current Hero data frame with the running Heroes data frame
  Heroes <- rbind(Heroes, Hero)
}
# Convert the game mode into a factor with 2 levels
Heroes$Mode <- as.factor(Heroes$Mode)

# save and timestamp the data, since the "past month" data depends on when we're accessing it
save(Heroes, file = paste0("Heroes_", format(Sys.Date(), "%m-%d-%Y"), ".Rda"))
