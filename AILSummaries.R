#AIL Season 6 Summary

#packages needed
library(DBI)
library(tidyverse)
library(stringr)
library(googlesheets4)

#authentication
gs4_auth(email = "kaile.phelps@gmail.com")

#setup parameters

googleSheetId <- "1FZ3rV8uBOyxD09hh2AgaTxlO7xJqriqgB048qluoyKg"
tourId <- "AIL06"

#Load Data

source("C:/Users/kphelps/OneDrive - Epic/Desktop/Kaile's Things/R Projects/AshesProjects/LoadAshesDB.R")
conn <- connect2db()
loadPhgData(conn)
loadTourData(conn)

#Load Summary Functions
source("C:/Users/kphelps/OneDrive - Epic/Desktop/Kaile's Things/R Projects/AshesProjects/Create Summaries.R")

#create summaries
gameResults <- gameResults 
lineGameResults <- getLineGameResults(gameResults)
deckStats <- getDeckStats(lineGameResults)
tourStats <- getTourStats(deckInfo,tourInfo)
pbTourStats <- getPBTourStats(lineGameResults,deckInfo,cardInfo,tourStats,expInfo,deckStats)

#Create data frames to write to page

#Deck Links
diceSpread <- deckDice %>%
  filter(Number > 1) %>%
  group_by(Deck_ID) %>%
  summarise(Dice_Spread = paste(Dice_Type,collapse = "/"))

deckInfo <- deckInfo %>%
  select(-Dice_Spread) %>%
  left_join(diceSpread, by = "Deck_ID")

deckLinks <- deckInfo %>%
  filter(Tour_ID == tourId) %>%
  left_join(playerInfo, by = "Player_ID") %>%
  left_join(tourInfo, by = "Tour_ID") %>%
  select(Player_Name,
         Tour_Name,
         Round_Num,
         Deck_Link,
         Deck_Name,
         PB_Name,
         Dice_Spread)

#PB_Stats
printPbStats <- pbTourStats %>%
  left_join(tourInfo, by = "Tour_Name") %>%
  filter(Tour_ID == tourId) %>%
  ungroup() %>%
  select(PB_Name,
         Tour_Name,
         Tot_Decks,
         Num_Decks,
         Deck_Pct,
         Deck_Pct_Change,
         W,
         L,
         Win_Pct)

#player standings

lineGameResults <- getLineGameResults(gameResults)
swissStandings <- getPlayerSwissStandings(lineGameResults)
sos <- getPlayerSos(lineGameResults,swissStandings) %>%
  mutate(Opp_Win_Pct = Opp_W/(Opp_W + Opp_L))

playerSeasonInfo <- deckInfo %>%
  group_by(Player_ID,Tour_ID) %>%
  summarise(PB_Count = n_distinct(PB_Name),
            Unq_Dice_Spread = n_distinct(Dice_Spread))

playerPBRecord <- deckInfo %>%
  left_join(tourInfo, by= "Tour_ID") %>%
  left_join(pbTourStats, by = c("PB_Name","Tour_Name")) %>%
  group_by(Player_ID,Tour_ID) %>%
  summarise(PB_W = sum(W),
            PB_L = sum(L)) %>%
  mutate(PB_Win_Pct = PB_W/(PB_W+PB_L), .after = PB_L)

printPlayerStandings <- swissStandings %>%
  left_join(sos, by = c("Tour_ID","Player_ID")) %>%
  left_join(playerSeasonInfo, by = c("Tour_ID","Player_ID")) %>%
  left_join(playerPBRecord, by = c("Tour_ID", "Player_ID")) %>%
  left_join(tourInfo, by = "Tour_ID") %>%
  left_join(playerInfo, by = "Player_ID") %>%
  filter(Tour_ID == tourId) %>%
  ungroup() %>%
  select(Player_Name,
         Tour_Name,
         W,
         L,
         D,
         Opp_W,
         Opp_L,
         Opp_D,
         Opp_Win_Pct,
         PB_Count,
         Unq_Dice_Spread,
         PB_W,
         PB_L,
         PB_Win_Pct)

#write to googlesheet
range_write(deckLinks, ss = googleSheetId, sheet = "Deck Links", reformat = FALSE)
range_write(printPbStats, ss= googleSheetId, sheet = "PB Stats", reformat = FALSE)
range_write(printPlayerStandings, ss = googleSheetId, sheet = "Player Standings", reformat = FALSE)
