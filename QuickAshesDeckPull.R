#Pull Data from decklist

library("stringr")
#library("reshape")
library("dplyr")
library("googlesheets4")
library("googledrive")
library("httr")
library("jsonlite")
library("tidyr")
gs4_auth("kaile.phelps@gmail.com")

#Load Data

source("C:/Users/kphelps/OneDrive - Epic/Desktop/Kaile's Things/R Projects/AshesProjects/LoadAshesDB.R")
conn <- connect2db()
loadPhgData(conn)
loadTourData(conn)

tour_Id <- "SB08"

deckLists <- read_sheet("1tqhUkWF6h7UEkQh1DohppaCzbDWp4ATHDMkDh7BP7A0")

deckLists$GamerTag[2] <- "Asterix"
deckLists$GamerTag[11] <- "inquisitor0684"
deckLists$GamerTag[15] <- "massimo"
deckLists$GamerTag[23] <- "shadowfire430"
deckLists$GamerTag[24] <- "ShufflebusJesse"
deckLists$GamerTag[25] <- "ShufflebusNeil"

deckLists <- deckLists %>%
  rename(Player_Name = "GamerTag",
         Deck_Link = "Decklist") %>%
  left_join(playerInfo, by = "Player_Name") %>%
  mutate(Deck_ID = paste(tour_Id,
                        str_pad(row_number(),2,pad="0"),
                        sep=""),
         Round_Num = "All",
         Tour_ID = tour_Id) %>%
  select(Deck_ID,
         Player_ID,
         Deck_Link,
         Round_Num,
         Tour_ID)


getDeckAsList <- function(deckLinkDF) {
  deckAsList <- getDeckFromLink(deckLinkDF[["Deck_Link"]])
  deckAsList$Deck_Link <- deckLinkDF[["Deck_Link"]]
  deckAsList$Player_ID <- deckLinkDF[["Player_ID"]]
  deckAsList$Deck_ID <- deckLinkDF[["Deck_ID"]]
  deckAsList$Round_Num <- deckLinkDF[["Round_Num"]]
  deckAsList$Tour_ID <- deckLinkDF[["Tour_ID"]]
  return(deckAsList)
}

getDeckFromLink <- function(link){
  link_split <- str_split(link,"/", simplify = TRUE)
  private <- 'share' %in% link_split
  link_id <- link_split[length(link_split)-1]
  if (private) {
    api <- paste("https://api.ashes.live/v2/decks/shared/",link_id,"/",sep = "")
    jsonData <- GET(api)
    deckData <- fromJSON(rawToChar(jsonData$content))
  } else {
    api <- paste("https://api.ashes.live/v2/decks/",link_id,"/",sep = "")
    jsonData <- GET(api)
    deckData <- fromJSON(rawToChar(jsonData$content))
    deckData <- deckData$deck
  }
  return(deckData)
}

getnewDeckInfo <- function(deckAsList){
  deckLine <- c(deckAsList[["Deck_ID"]],
                deckAsList[["Deck_Link"]],
                deckAsList[["title"]],
                deckAsList[["Tour_ID"]],
                deckAsList[["Round_Num"]],
                deckAsList[["Player_ID"]],
                deckAsList[["phoenixborn"]][["name"]])
  names(deckLine) <- c("Deck_ID",
                       "Deck_Link",
                       "Deck_Name",
                       "Tour_ID",
                       "Round_Num",
                       "Player_ID",
                       "PB_Name")
  return(deckLine)
}

getnewDeckCards <- function(deckAsList) {
  newDeckCards <- deckAsList[["cards"]]
  newDeckCards <- newDeckCards %>%
    mutate("Deck_ID" = deckAsList[["Deck_ID"]]) %>%
    rename(Number = count,
           Card_Name = name) %>%
    select(Deck_ID,
           Card_Name,
           Number,
           stub)
}

getnewDeckDice <- function(deckAsList) {
  newDeckDice <- deckAsList[["dice"]]
  newDeckDice <- newDeckDice %>%
    mutate("Deck_ID" = deckAsList[["Deck_ID"]],
           name = str_to_title(name)) %>%
    rename(Number = count,
           Dice_Type = name)
}


decksAsLists <- apply(deckLists,1,getDeckAsList)

newDeckInfo <- lapply(decksAsLists,getnewDeckInfo)
newDeckInfo <- do.call(rbind,newDeckInfo)
newDeckInfo <- as.data.frame(newDeckInfo)

newDeckCards <- lapply(decksAsLists,getnewDeckCards)
newDeckCards <- do.call(rbind,newDeckCards)
newDeckCards <- newDeckCards %>%
  left_join(cardInfo[,1:2], by = "Card_Name") %>%
  select(Deck_ID,
         Card_ID,
         Card_Name,
         Number)

newDeckDice <- lapply(decksAsLists,getnewDeckDice)
newDeckDice <- do.call(rbind,newDeckDice)
newDeckDice <- newDeckDice %>%
  select(Deck_ID,
         Dice_Type,
         Number)

diceSpread <- newDeckDice %>%
  filter(Number > 1) %>%
  group_by(Deck_ID) %>%
  summarise(Dice_Spread = paste(Dice_Type,collapse = "/"))


newDeckInfo <- newDeckInfo %>%
  left_join(diceSpread, by = "Deck_ID")


dataSheet <- "1jLzTvr5c1vfZjcEPEjawdqFk-jhIXJC8GkY0-pQPu8k"
sheet_append(dataSheet,newDeckInfo,sheet = "Deck Info")
sheet_append(dataSheet,newDeckCards,sheet = "Deck Cards")
sheet_append(dataSheet,newDeckDice,sheet = "Deck Dice")
