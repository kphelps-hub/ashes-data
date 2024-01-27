#packages needed
library(DBI)
library(googlesheets4)
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)

#Tour Line Info
getTourLineInfo <- function(conn,tourId) {
  assign("tourInfo", dbReadTable(conn, "Tournament Info"),
         envir = .GlobalEnv)
  tourLineInfo <- tourInfo[tourInfo$Tour_ID==tourId,]
  
  return(tourLineInfo)
}

#create and organize Columns
formatRawGameResults <- function(newGameResults){
  #Format: Timestamp | Winning Discord Name | Losing Discord Name | Blood Points | Winning Player's Deck | Losing Player's Deck
  
  colNames <- c("Report_Instant","Winning_Discord_ID","Losing_Discord_ID","Losing_Blood_Points",
                "Winning_Deck_Link","Losing_Deck_Link")
  
  #Add Columns that are missing
  
  numCol <- length(colNames)
  nColAdd <- numCol - ncol(newGameResults)
  
  if (nColAdd != 0) {
    for (i in colNames[(numCol-nColAdd+1):numCol]) {
      newGameResults[,i] <- NA
    }
  }
  
  names(newGameResults) <- colNames
  
  return(newGameResults)
}

#filter to new results
getFilteredNewResults <- function(tourLineInfo, newGameResults) {
  lastExtractDate <- tourLineInfo$Last_Extract_Date
  if (is.na(lastExtractDate)) {
    lastExtractDate <- tourLineInfo$Tour_Start_Date
  }
  
  newGameResults <- newGameResults %>%
    filter(Report_Instant > lastExtractDate)
  
  return(newGameResults)
}

#Get Player Id's
addPlayerIds <- function(conn,newGameResults){
  assign("playerInfo", dbReadTable(conn, "Player Info"),
         envir = .GlobalEnv)
  newGameResults <- newGameResults %>%
    left_join(playerInfo, by = c("Winning_Discord_ID" = "Player_Name"),
              na_matches = "never") %>%
    rename(Winning_Player_ID = Player_ID) %>%
    select(-Discord_Name,
           -Winning_Discord_ID) %>%
    left_join(playerInfo, by = c("Losing_Discord_ID" = "Player_Name"),
              na_matches = "never") %>%
    rename(Losing_Player_ID = Player_ID) %>%
    select(-Discord_Name,
           -Losing_Discord_ID)
  
  return(newGameResults)
}

#Function to take raw game result data from reporting form to be ready to extract
formatNewGameResults <- function(newGameResults, conn, tourLineInfo, tourId){
  newGameResults <- formatRawGameResults(newGameResults)
  newGameResults <- getFilteredNewResults(tourLineInfo, newGameResults)
  newGameResults <- addPlayerIds(conn, newGameResults)
  #assign Tour ID
  newGameResults <- newGameResults %>%
    mutate(Tour_ID = tourId)
  
  return(newGameResults)
  
}

extractGameLine <- function(newGameLine, tourLineInfo, conn, gameType, resultType, gsRawData){
  newGameLine <- as.data.frame(t(as.data.frame(newGameLine)))
  
  assign("error",NA, envir = .GlobalEnv)
  
  #assign Game IDs
  assign("gameResults",dbReadTable(conn, "Game Results"),
         envir = .GlobalEnv)
  tourGameResults <- gameResults %>%
    filter(Tour_ID == newGameLine$Tour_ID)
  newGameLine <- assignGameIds(newGameLine, tourGameResults)
  
  #get or assign deck IDs and deck information
  locked <- "Yes" == tourLineInfo$Locked_Lists
  if (locked) {
    assign("deckInfo", dbReadTable(conn, "Deck Info"),
           envir = .GlobalEnv)
    tourDeckInfo <- deckInfo %>%
      filter(Tour_ID == newGameLine$Tour_ID)
    newGameLine <- getDeckIds(newGameLine,tourDeckInfo)
  }
  if (!locked) {
    #assign Deck Id's
    newGameLine <- assignDeckIds(newGameLine)
    
    #get deck Links and call API
    deckLinkDF <- getDeckLinkDF(newGameLine)
    decksAsLists <- apply(deckLinkDF,1,getDeckAsList)
    
    #update Deck Dice
    newDeckDice <- lapply(decksAsLists,getnewDeckDice)
    newDeckDice <- do.call(rbind,newDeckDice)
    newDeckDice <- newDeckDice %>%
      select(Deck_ID,
             Dice_Type,
             Number)
    sheet_append(gsRawData,newDeckDice,sheet = "Deck Dice")
    dbWriteTable(conn,
                 name = "Deck Dice",
                 value = newDeckDice,
                 append = TRUE)
    
    #update Deck Cards
    newDeckCards <- lapply(decksAsLists,getnewDeckCards)
    newDeckCards <- do.call(rbind,newDeckCards)
    newDeckCards <- newDeckCards %>%
      left_join(cardInfo[,1:2], by = "Card_Name") %>%
      select(Deck_ID,
             Card_ID,
             Card_Name,
             Number)
    sheet_append(gsRawData,newDeckCards,sheet = "Deck Cards")
    dbWriteTable(conn,
                 name = "Deck Cards",
                 value = newDeckCards,
                 append = TRUE)
    
    #update Deck Info
    newDeckInfo <- lapply(decksAsLists,getnewDeckInfo)
    newDeckInfo <- do.call(rbind,newDeckInfo)
    newDeckInfo <- as.data.frame(newDeckInfo)
    sheet_append(gsRawData,newDeckInfo,sheet = "Deck Info")
    dbWriteTable(conn,
                 name = "Deck Info",
                 value = newDeckInfo,
                 append = TRUE)
  }
  
  #format for Game Results
  #Game_ID	Tour_ID	Game_Num	Round_Num	Game_Type	Result_Type	Winning_Deck_ID	
  #Winning_Player_ID	Losing_Deck_ID	Losing_Player_ID	Losing_Blood_Points	Report_Instant
  newGameLine <- newGameLine %>%
    mutate(Game_Type = gameType,
           Result_Type = ifelse(is.na(Losing_Player_ID),"Bye",
                                resultType),
           Error_Message = error) %>%
    select(Game_ID,
           Tour_ID,
           Game_Num,
           Round_Num,
           Game_Type,
           Result_Type,
           Winning_Deck_ID,
           Winning_Player_ID,
           Losing_Deck_ID,
           Losing_Player_ID,
           Losing_Blood_Points,
           Report_Instant,
           Error_Message)
  
  sheet_append(gsRawData,newGameLine,sheet = "Game Results")
  dbWriteTable(conn,
               name = "Game Results",
               value = newGameLine,
               append = TRUE)
  
  return(newGameLine)
}

#figure out the round, game number, game ID. Function takes a single entry and finds
#Game ID, Round Num, Game Num

assignGameIds <- function(newGameLine, tourGameResults){
  lastRounddf <- tourGameResults %>%
    filter(Winning_Player_ID == newGameLine$Winning_Player_ID |
             Losing_Player_ID == newGameLine$Winning_Player_ID)
  
  wLastRound <- max(c(as.numeric(lastRounddf$Round_Num),0))
  
  lastRounddf <- tourGameResults %>%
    filter(Winning_Player_ID == newGameLine$Losing_Player_ID |
             Losing_Player_ID == newGameLine$Losing_Player_ID) 
  
  lLastRound <- max(c(as.numeric(lastRounddf$Round_Num),0))
  
  lastRound <- max(wLastRound,lLastRound)
  
  newGameLine$Round_Num <- lastRound+1
  
  lastGamedf <- tourGameResults %>%
    filter(Round_Num == newGameLine$Round_Num)
  
  lastGame <- max(c(lastGamedf$Game_Num,0))
  
  newGameLine$Game_Num <- lastGame+1
  
  newGameLine$Game_ID <- paste(newGameLine$Tour_ID,
                                     "R",
                                     str_pad(newGameLine['Round_Num'],2,pad="0"),
                                     str_pad(newGameLine['Game_Num'],2,pad="0"),
                                     sep = "")
  
  return(newGameLine)
}

#Get Deck IDs from Deck Info. Only used for Locked formats
getDeckIds <- function(newGameLine,tourDeckInfo) {
    newGameLine <- newGameLine %>%
      left_join(
        select(tourDeckInfo, c(Deck_ID,Player_ID)),
        by = c("Winning_Player_ID" = "Player_ID")) %>%
      rename(Winning_Deck_ID = Deck_ID) %>%
      left_join(
        select(tourDeckInfo, c(Deck_ID,Player_ID)),
        by = c("Losing_Player_ID" = "Player_ID")) %>%
      rename(Losing_Deck_ID = Deck_ID)
    return(newGameLine)
}

#Assign Deck IDs based on game/round. 
assignDeckIds <- function(newGameLine) {
    newGameLine$Winning_Deck_ID = paste(newGameLine$Tour_ID,
                                        str_pad(newGameLine$Round_Num,2,pad="0"),
                                        str_pad(newGameLine$Game_Num*2-1,2,pad="0"),
                                        sep="")
    newGameLine$Losing_Deck_ID = paste(newGameLine$Tour_ID,
                                       str_pad(newGameLine$Round_Num,2,pad="0"),
                                       str_pad(newGameLine$Game_Num*2,2,pad="0"),
                                       sep="")
    return(newGameLine)
}

#update Last Extract Date
updateLastExtractDate <- function(tourInfo,tourId,gsRawData,conn){
  extractDate <- Sys.time()
  extractDate <- force_tz(Sys.time(),tzone="UTC")
  tourInfo[tourInfo$Tour_ID==tourId,]$Last_Extract_Date <- extractDate
  
  sheet_write(tourInfo, gsRawData, sheet = "Tournament Info")
  dbWriteTable(conn, "Tournament Info", tourInfo, overwrite = TRUE)
  
  return(1)
}

getDeckLinkDF <-function(gameResults){
  winners <- gameResults %>%
    rename(Deck_ID = Winning_Deck_ID,
           Player_ID = Winning_Player_ID,
           Deck_Link = Winning_Deck_Link) %>%
    select(Deck_ID,
           Player_ID,
           Deck_Link,
           Round_Num,
           Tour_ID)
  
  losers <- gameResults %>%
    rename(Deck_ID = Losing_Deck_ID,
           Player_ID = Losing_Player_ID,
           Deck_Link = Losing_Deck_Link) %>%
    select(Deck_ID,
           Player_ID,
           Deck_Link,
           Round_Num,
           Tour_ID)
  
  deckLinkDF <- bind_rows(winners,losers) %>%
    arrange(Deck_ID)
  
  return(deckLinkDF)
}

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
  if (private) {
    idIndex <- match("share",link_split)+1
    link_id <- link_split[idIndex]
    api <- paste("https://api.ashes.live/v2/decks/shared/",link_id,"/",sep = "")
    jsonData <- GET(api)
    deckData <- fromJSON(rawToChar(jsonData$content))
  } else if ("mine" %in% link_split) {               #if user submits private deck link rather than shareable/public link
    idIndex <- match("mine", link_split)+1
    link_id <- link_split[idIndex]
    api <- paste("https://api.ashes.live/v2/decks/",link_id,"/",sep = "")
    jsonData <- GET(api)
    deckData <- fromJSON(rawToChar(jsonData$content))
    deckData <- deckData$deck
  } else {
    idIndex <- match("decks",link_split)+1
    link_id <- link_split[idIndex]
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
  cardCount <- sum(newDeckCards$Number)
  if (cardCount != 30) {
    assign("error", paste(error,"Incorrect Number of Cards", sep = "|"),
           envir = .GlobalEnv)
  }
  return(newDeckCards)
}

getnewDeckDice <- function(deckAsList) {
  newDeckDice <- deckAsList[["dice"]]
  newDeckDice <- newDeckDice %>%
    mutate("Deck_ID" = deckAsList[["Deck_ID"]],
           name = str_to_title(name)) %>%
    rename(Number = count,
           Dice_Type = name)
  
  diceCount <- sum(newDeckDice$Number)
  if (diceCount != 10) {
    assign("error", paste(error,"Incorrect Number of Dice", sep = "|"),
           envir = .GlobalEnv)
  }
  return(newDeckDice)
}


