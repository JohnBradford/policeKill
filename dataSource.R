library(dplyr)
library(ggmap)

wapo <- read.csv("https://cdn.rawgit.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv")

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
w <- cbind.data.frame(geocode(as.character(paste(wapo$city, wapo$state, sep=" ")), 
                              source="google"), 
                              wapo)

##Reducing, renaming
names(w)[names(w)=="signs_of_mental_illness"] <- "mental illness"
names(w)[names(w)=="unarmed" ] <- "Armed"
w <- w[,-c(3,4,7,10)]
source("stateListFunction.R")
w$state <- stateFromLower(wapo$state)

write.csv(w, file="wShort.csv")
