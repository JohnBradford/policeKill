library(dplyr)
library(ggmap)

#Washington Post
wapo <- read.csv("https://cdn.rawgit.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv")
wapo$date <- as.Date(wapo$date)
# filter by unarmed vs other, create new categorical variable and recombine
w_unarmed <- wapo %>%
  filter(armed == "unarmed") %>%
  mutate(unarmed = "Unarmed")
w_other <- wapo %>%
  filter(armed != "unarmed" | is.na(armed)) %>%
  mutate(unarmed = "Armed/Undetermined")
wapo <- bind_rows(w_unarmed,w_other)

# collapse the race categories into black, white, hispanic, and other
w_main_races <- wapo %>%
  filter(race == "B" | race == "W" | race == "H") %>%
  mutate(Race = race)
w_other_races <- anti_join(wapo, w_main_races, by="id") %>%
  mutate(Race = "Other")
wapo <- bind_rows(w_main_races,w_other_races)

wapo$Race[which(wapo$Race=="W")] <- "White"
wapo$Race[which(wapo$Race=="B")] <- "Black"
wapo$Race[which(wapo$Race=="H")] <- "Hispanic"

##getting coordinates
## THIS TAKES AWHILE!
# w <- cbind.data.frame(geocode(as.character(paste(wapo$city, wapo$state, sep=" ")), 
#                               source="google"), 
#                               wapo)

##Reducing, renaming
names(w)[names(w)=="signs_of_mental_illness"] <- "mental_illness"
names(w)[names(w)=="unarmed" ] <- "Armed"
w <- w[,-c(3,4,7,10)]
source("stateListFunction.R")
w$state <- stateFromLower(wapo$state)

#write.csv(w, file="wShort.csv")

##Guardian
G <- read.csv("https://cdn.rawgit.com/flother/thecounted/master/data/the-counted-2015.csv")

G2 <- read.csv("https://cdn.rawgit.com/flother/thecounted/master/data/the-counted-2016.csv")

G <- bind_rows(G, G2)

names(G)[names(G)=="raceethnicity"] <- "race"
#names(G)[names(G)=="armed" ] <- "Armed"
names(G)[names(G)=="classification" ] <- "manner_of_death"
G$m <- NA
for(i in 1:nrow(G)) {
G$m[i] <- switch(as.character(G$month[i]), 
              "January" = 1, "February" =2, "March"  = 3, "April" =4, 
              "May" = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9, 
              "October" = 10, "November" = 11, "December" = 12)}
G$date <- as.Date(paste(G$year, G$m, G$day, sep="-"))

# collapse the race categories into black, white, hispanic, and other
g_main_races <- G %>%
  filter(race == "Black" | race == "White" | race == "Hispanic/Latino") %>%
  mutate(Race = race)
g_other_races <- anti_join(G, g_main_races, by="uid") %>%
  mutate(Race = "Other")
G <- bind_rows(g_main_races,g_other_races)

G$Race[which(G$Race =="Hispanic/Latino")] <- "Hispanic"


# filter by unarmed vs armed/unknown/disputed, create new categorical variable and recombine
g_unarmed <- G %>%
  filter(armed=="No") %>%
  mutate(Armed="Unarmed")
g_other <- G %>%
  filter(armed != "No") %>%
  mutate(Armed="Other")
G <- bind_rows(g_unarmed,g_other)
