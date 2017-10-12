
## Overbuff Scraper in R

### Synopsis

The site [Overbuff]("http://www.overbuff.com") has a large amount of statistics on Overwatch. For the purposes of this analysis, I was primarily interested in comparing Hero pick rates in QuickPlay vs. Competitive matches. I scraped the following pieces of data from **PC** games for the **past month**:

* Hero Name & Class Type
* Stats: Elimination Ratio, On Fire, Pick Rate, and Win Rate

This scraper may get updated as I analyze more pieces of statistics from the website, such as platform differences, or the changes of pick rates from the last 6 months vs. this week.

For the scraper, I used the `R` package `rvest`, which makes it a lot easier to collect html data from web pages. If you don't have it, make sure to install it along with its dependencies.




```r
library(rvest)
```

***

### Scrape the main page for a list of heroes

The individual hero stats can be found on their own pages. They are available on the main [Overbuff heroes page]("https://www.overbuff.com/heroes"), but only by clicking through the table, which makes it impossible to extract data using the `rvest` package, which needs a specific, unique URL to access the source data. (Note: if this is untrue, contact me because I'd like to know how it can be done!)


```r
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

HeroList
```

```
##  [1] "ana"        "bastion"    "dva"        "doomfist"   "genji"     
##  [6] "hanzo"      "junkrat"    "lucio"      "mccree"     "mei"       
## [11] "mercy"      "orisa"      "pharah"     "reaper"     "reinhardt" 
## [16] "roadhog"    "soldier76"  "sombra"     "symmetra"   "torbjorn"  
## [21] "tracer"     "widowmaker" "winston"    "zarya"      "zenyatta"
```

Now we have a list of Hero names, all lowercase, with no special characters, ready to be used in a URL to access each individual Hero's stats. We now need to create a list of these URLs that we can access later. Apart from the hero name, the URL also includes a play mode, quickplay or competitive. I'm interested in both, so we're going to end up with a list of 50 (25 heroes x 2 game modes) web pages to access.


```r
pages <- character(0)
modes <- c("quickplay", "competitive")
for (i in 1:length(HeroList)){
  page1 <- paste0(URL, "/", HeroList[i], "?mode=", modes[1])
  page2 <- paste0(URL, "/", HeroList[i], "?mode=", modes[2])
    
  pages <- c(pages, page1, page2)
}

length(pages)
```

```
## [1] 50
```

***

### Create a data frame with all the heroes and their stats

Now that we have a list of URLs to scrape the hero stats from, we'll access each one individually, make a row of a data frame, and then combine all of the hero rows to create one big data frame.

```r
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

head(Heroes)
```

```
##      Name    Type                     Tags        Mode ElimRatio OnFire
## 1     Ana Support                   Healer   Quickplay      1.27   7.4%
## 2     Ana Support                   Healer Competitive      1.28  11.9%
## 3 Bastion Defense  High Damage, Stationary   Quickplay      2.59  14.6%
## 4 Bastion Defense  High Damage, Stationary Competitive      2.61  20.4%
## 5    D.Va    Tank                Disruptor   Quickplay      3.72   9.1%
## 6    D.Va    Tank                Disruptor Competitive      3.88  11.1%
##   PickRate WinRate
## 1    4.07% 100.00%
## 2    4.60%  43.68%
## 3    0.70%  99.96%
## 4    0.64%  50.61%
## 5    4.90% 100.01%
## 6    6.61%  51.50%
```
Now we can use this data frame for analysis!
