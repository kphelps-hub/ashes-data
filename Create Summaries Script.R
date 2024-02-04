#Script to run daily

#packages needed
library(DBI)
library(tidyverse)
library(stringr)
library(googlesheets4)

#authentication
gs4_auth(email = "kaile.phelps@gmail.com")

#setup parameters

googleSheetId <- "1Xfs0NgxKigrkt61_IVn0sByYpCV85d5dQQntzcqdY5o"
pbInfoSheet <- "PB Data"
playerInfoSheet <- "Player Data"
cardInfoSheet <- "Card Data"
tourInfoSheet <- "Tour Data"

#Load Data

source("C:/Users/kphelps/OneDrive - Epic/Desktop/Kaile's Things/R Projects/AshesProjects/LoadAshesDB.R")
conn <- connect2db()
loadPhgData(conn)
loadTourData(conn)

#Load Summary Functions
source("C:/Users/kphelps/OneDrive - Epic/Desktop/Kaile's Things/R Projects/AshesProjects/Create Summaries.R")

#create summaries
lineGameResults <- getLineGameResults(gameResults)
deckStats <- getDeckStats(lineGameResults)
tourStats <- getTourStats(deckInfo,tourInfo)
pbTourStats <- getPBTourStats(lineGameResults,deckInfo,cardInfo,tourStats,expInfo,deckStats)
playerStats <- getPlayerStats(lineGameResults,tourInfo,playerInfo)
cardTourStats <- getCardTourStats(cardInfo,expInfo,deckStats,tourStats,deckCards,lineGameResults)

#write to googlesheet
range_write(googleSheetId,pbTourStats,sheet = pbInfoSheet, range = "A1", reformat = FALSE)
range_write(googleSheetId,playerStats,sheet = playerInfoSheet, range = "A1", reformat = FALSE)
range_write(googleSheetId,cardTourStats,sheet = cardInfoSheet, range = "A1", reformat = FALSE)
