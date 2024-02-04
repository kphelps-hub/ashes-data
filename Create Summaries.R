#Load Data
library(DBI)
library(tidyverse)
library(stringr)
# source("C:/Users/kphelps/OneDrive - Epic/Desktop/Kaile's Things/R Projects/AshesProjects/LoadAshesDB.R")
# conn <- connect2db()
# loadPhgData(conn)
# loadTourData(conn)

lineGameResultsCols <- function(){
  cols <- c("Game_ID",
            "Tour_ID",
            "Game_Num",
            "Round_Num",
            "Game_Type",
            "Result_Type",
            "Winning_Deck_ID",
            "Winning_Player_ID",
            "Losing_Deck_ID",
            "Losing_Player_ID")
  return(cols)
}

getLineGameResults <- function(gameResults){
  
  #Define Columns to Select
  cols <- lineGameResultsCols()
  
  #Create df for winners
  winners <- gameResults %>%
    filter(Result_Type != "Drop") %>%
    select(all_of(cols)) %>%
    mutate(Blood_Points = ifelse(Result_Type == "Bye",0,25),
           Result = "W") %>%
    rename(Deck_ID = Winning_Deck_ID,
           Player_ID = Winning_Player_ID,
           Opp_Deck_ID = Losing_Deck_ID,
           Opp_Player_ID = Losing_Player_ID)
  
  #create df for losers
  losers <- gameResults %>%
    filter(Result_Type != "Drop") %>%
    select(all_of(cols),
           Losing_Blood_Points) %>%
    mutate(Result = "L",
           Losing_Blood_Points = ifelse(Result_Type == "Concession",0,
                                        Losing_Blood_Points)) %>%
    rename(Deck_ID = Losing_Deck_ID,
           Player_ID = Losing_Player_ID,
           Opp_Deck_ID = Winning_Deck_ID,
           Opp_Player_ID = Winning_Player_ID,
           Blood_Points = Losing_Blood_Points)
  
  #create df for drops
  drops <- gameResults %>%
    filter(Result_Type == "Drop") %>%
    select(all_of(cols)) %>%
    mutate(Blood_Points = 0,
           Result = "D") %>%
    rename(Deck_ID = Losing_Deck_ID,
           Player_ID = Losing_Player_ID,
           Opp_Deck_ID = Winning_Deck_ID,
           Opp_Player_ID = Winning_Player_ID)
  
  #bind all results together
  lineGameResults <- bind_rows(winners,
                               losers,
                               drops) %>%
    filter(is.na(Player_ID)==FALSE) %>%
    arrange(Game_ID)
  
  return(lineGameResults)
}

# lineGameResults <- getLineGameResults(gameResults)

deckTopCutResults <- function(lineGameResults,topCut){
  if (topCut == "Tour_Winner") {
    
    deckTopCut <- lineGameResults %>%
      filter(Game_Type == "Top 2",
             Result == "W") %>%
      select(Deck_ID) %>%
      mutate(!!str_replace(topCut," ","_") := "Yes")
  }
  
  else if (topCut == "Top 2") {
    deckTopCut <- lineGameResults %>%
      filter(Game_Type == topCut,
             Result_Type == "Match") %>%
      select(Deck_ID) %>%
      mutate(!!str_replace(topCut," ","_") := "Yes")
  }
  
  else {
    
    deckTopCut <- lineGameResults %>%
      filter(Game_Type == topCut) %>%
      select(Deck_ID) %>%
      mutate(!!str_replace(topCut," ","_") := "Yes")
  }
  
  return(deckTopCut)
}

getDeckSwissStandings <- function(lineGameResults){
  
  swissStandings <- lineGameResults %>%
    filter(Game_Type == "Swiss",
           Result_Type == "Match") %>%
    select(Deck_ID,
           Result) %>%
    group_by(Deck_ID) %>%
    summarise(W = sum(Result=="W"),
              L = sum(Result=="L"))
  
  return(swissStandings)
}

getDeckTopCutRecord <- function(lineGameResults){
  topCutRecord <- lineGameResults %>%
    filter(Round_Num == "TC",
           Result_Type == "Match") %>%
    select(Deck_ID,
           Result) %>%
    group_by(Deck_ID) %>%
    summarise(Top_Cut_W = sum(Result=="W"),
              Top_Cut_L = sum(Result=="L"))
  return(topCutRecord)
}

#Create a data frame with a new column for each top cut level
getDeckTopCutLevels <- function(lineGameResults){
  topCutTypes <- levels(as.factor(lineGameResults$Game_Type))
  topCutTypes <- topCutTypes[! topCutTypes %in% c("Swiss")]
  topCutDf <- lineGameResults %>%
    select(Deck_ID) %>%
    distinct()
  
  for (i in topCutTypes){
    topCutSplit <- str_split(i," ", simplify = TRUE)
    topCutNum <- topCutSplit[2]
    var <- paste("top",topCutNum,"Decks",sep = "")
    assign(var,deckTopCutResults(lineGameResults,i),envir = .GlobalEnv)
    topCutDf <- topCutDf %>%
      left_join(eval(as.name(var)), by = "Deck_ID")
  }
  
  tourWinner <- deckTopCutResults(lineGameResults,"Tour_Winner")
  
  topCutDf <- topCutDf %>%
    left_join(tourWinner, by = "Deck_ID")
  
  return(topCutDf)
}

getDeckStats <- function(lineGameResults) {
  
  #Swiss Record
  swissStandings <- getDeckSwissStandings(lineGameResults)
  
  #Top Cut Record
  topCutRecord <- getDeckTopCutRecord(lineGameResults)
  
  #Top Cut Levels
  topCutDf <- getDeckTopCutLevels(lineGameResults)
  
  #Join Swiss record, top cut record and top cut levels together
  deckStats <- swissStandings %>%
    full_join(topCutRecord, by = "Deck_ID") %>%
    full_join(topCutDf, by = "Deck_ID")
  
  return(deckStats)
}

# deckStats <- getDeckStats(lineGameResults)

getTourStats <- function(deckInfo,tourInfo) {
  tourStats <- deckInfo %>%
    group_by(Tour_ID)%>%
    summarise(Participants = n_distinct(Player_ID),
              Tot_Decks = n(),
              Tot_Swiss_Decks = sum(!Round_Num %in% c("TC","All")),
              Tot_Top_Cut_Decks = sum(Round_Num == "TC")) %>%
    left_join(tourInfo, by = "Tour_ID") %>%
    relocate(Participants, .after = Tour_Start_Date)
  return(tourStats)
}

# tourStats <- getTourStats(deckInfo,tourInfo)

getPbList <- function(cardInfo,tourStats,expInfo){
  
  pbList <- cardInfo %>%
    filter(Card_Type == "Phoenixborn") %>%
    left_join(expInfo, by = c("Precon_ID", "Precon_Name")) %>%
    merge(tourStats,
          all=TRUE) %>%
    rename(Exp_Spoil_Date = Spoil_Date,
           Exp_Receive_Date = Receive_Date,
           PB_Name = Card_Name) %>%
    mutate(Legality = Tour_Start_Date > Exp_Spoil_Date,
           Tot_Decks = Legality * Tot_Decks,
           Tot_Swiss_Decks = Legality * Tot_Swiss_Decks,
           Tot_Top_Cut_Decks = Legality * Tot_Top_Cut_Decks) 
  
  return(pbList)
}

getPbUsage <- function(deckInfo){
  pbUsage <- deckInfo %>%
    group_by(PB_Name,
             Tour_ID) %>%
    summarize(Num_Decks = n(),
              Swiss_Decks = sum(!Round_Num %in% c("TC","All")),
              Top_Cut_Decks = sum(Round_Num == "TC"))
  return(pbUsage)
}

getPbStats <- function(deckStats, deckInfo){
  
  #join deckInfo and stats together
  deckStats <- deckInfo %>%
    left_join(deckStats, by = "Deck_ID")
  
  #calculate swiss and top cut records
  pbRecords <- deckStats %>%
    group_by(PB_Name,
             Tour_ID) %>%
    summarise(
      across(.cols = c("W","L","Top_Cut_W","Top_Cut_L"),
             .fns = ~ sum(.,na.rm = TRUE)
             )
    )
  
  #calculate top cut counts
  pbTopCutCount <- deckStats %>%
    group_by(PB_Name,
             Tour_ID) %>%
    summarise(
      across(.cols = c(num_range("Top_",1:1000),"Tour_Winner"),
             .fns = ~ sum(!is.na(.))
      )
    )
  
  #join results together
  pbDeckStats <- pbRecords %>%
    left_join(pbTopCutCount, by = c("PB_Name","Tour_ID"))
  
  return(pbDeckStats)
}

getCalcPbTourData <- function(pbTourStats){
  
  calcPbTourData <- pbTourStats %>%
    mutate_if(is.numeric, replace_na, replace = 0) %>%
    
    #create all pct's
    mutate(Deck_Pct = Num_Decks / Tot_Decks,
           Swiss_Deck_Pct = Swiss_Decks / Tot_Swiss_Decks,
           Top_Cut_Deck_Pct = Top_Cut_Decks / Tot_Top_Cut_Decks,
           Win_Pct = W/(W+L),
           Top_Cut_Win_Pct = Top_Cut_W/(Top_Cut_W+Top_Cut_L)) %>%
    
    #create lag deck pct
    group_by(PB_Name,
             Tour_Series) %>%
    mutate(Deck_Pct_Change = Deck_Pct - lag(Deck_Pct),.after = Deck_Pct)
  
  return(calcPbTourData)
}

pbPrintTourCols <- function(){
  cols <- c("Season",
            "Tour_Series",
            "Tour_Name",
            "PB_Name",
            "Num_Decks",
            "Deck_Pct",
            "Deck_Pct_Change",
            "W",
            "L",
            "Win_Pct",
            "Top_Cut_W",
            "Top_Cut_L",
            "Top_Cut_Win_Pct",
            "Tour_Winner",
            "Top_2",
            "Top_4",
            "Top_8",
            "Top_12",
            "Swiss_Deck_Pct",
            "Top_Cut_Deck_Pct",
            "Tot_Decks",
            "Tour_Start_Date")
  
  return(cols)
}

getPBTourStats <- function(lineGameResults,deckInfo,cardInfo,tourStats,expInfo,deckStats) {
  
  #Create DF of PB's, Possible Decks, for each Tounament
  pbList <- getPbList(cardInfo,tourStats,expInfo)
  
  #Get PB Usage Count
  #Need to join with PBList and add deck Pct
  pbUsage <- getPbUsage(deckInfo)
  
  #Get Pb Deck Stats
  pbStats <- getPbStats(deckStats,deckInfo)
  
  #Combine PB Tour Stats
  pbTourStats <- pbList %>%
    left_join(pbUsage, by = c("PB_Name","Tour_ID")) %>%
    left_join(pbStats, by = c("PB_Name","Tour_ID"))
  
  #Calculate PB Tour Columns
  pbTourStats <- getCalcPbTourData(pbTourStats)
  
  #select columns and arrange
  cols <- pbPrintTourCols()
  pbTourStats <- pbTourStats %>%
    arrange(Season,Tour_Start_Date,PB_Name) %>%
    select(all_of(cols))
  
  return(pbTourStats)
}

# pbTourStats <- getPBTourStats(lineGameResults,deckInfo,cardInfo,tourStats,expInfo,deckStats)

playerTopCutResults <- function(lineGameResults,topCut){
  if (topCut == "Tour_Winner") {
    
    deckTopCut <- lineGameResults %>%
      filter(Game_Type == "Top 2",
             Result == "W") %>%
      select(Player_ID,Tour_ID) %>%
      mutate(!!str_replace(topCut," ","_") := "Yes")
  }
  
  else if (topCut == "Top 2"){
    
    deckTopCut <- lineGameResults %>%
      filter(Game_Type == topCut,
             Result_Type == "Match") %>%
      select(Player_ID,Tour_ID) %>%
      mutate(!!str_replace(topCut," ","_") := "Yes")
  }
  
  else {
    
    deckTopCut <- lineGameResults %>%
      filter(Game_Type == topCut) %>%
      select(Player_ID,Tour_ID) %>%
      mutate(!!str_replace(topCut," ","_") := "Yes")
  }
  
  return(deckTopCut)
}

getPlayerSwissStandings <- function(lineGameResults){
  
  swissStandings <- lineGameResults %>%
    filter(Game_Type == "Swiss") %>%
    select(Player_ID,
           Tour_ID,
           Result) %>%
    group_by(Player_ID,
             Tour_ID) %>%
    summarise(W = sum(Result=="W"),
              L = sum(Result=="L"),
              D = sum(Result=="D"))
  
  return(swissStandings)
}

getPlayerSos <- function(lineGameResults, swissStandings){
  
  sos <- lineGameResults %>%
    filter(Game_Type == "Swiss") %>%
    left_join(swissStandings, by = c("Tour_ID" = "Tour_ID",
                                     "Opp_Player_ID" = "Player_ID")) %>%
    group_by(Tour_ID, Player_ID) %>%
    summarise(Opp_W = sum(W,na.rm = TRUE),
              Opp_L = sum(L,na.rm = TRUE),
              Opp_D = sum(D,na.rm = TRUE))
  
  return(sos)
}

getPlayerTopCutRecord <- function(lineGameResults){
  topCutRecord <- lineGameResults %>%
    filter(Round_Num == "TC",
           Result_Type == "Match") %>%
    select(Player_ID,
           Tour_ID,
           Result) %>%
    group_by(Player_ID,
             Tour_ID) %>%
    summarise(Top_Cut_W = sum(Result=="W"),
              Top_Cut_L = sum(Result=="L"))
  return(topCutRecord)
}

getPlayerTopCutLevels <- function(lineGameResults){
  topCutTypes <- levels(as.factor(lineGameResults$Game_Type))
  topCutTypes <- topCutTypes[! topCutTypes %in% c("Swiss")]
  
  #initial DF
  topCutDf <- lineGameResults %>%
    select(Player_ID,
           Tour_ID) %>%
    distinct()
  
  for (i in topCutTypes){
    topCutSplit <- str_split(i," ", simplify = TRUE)
    topCutNum <- topCutSplit[2]
    var <- paste("top",topCutNum,"Decks",sep = "")
    assign(var,playerTopCutResults(lineGameResults,i),envir = .GlobalEnv)
    topCutDf <- topCutDf %>%
      left_join(eval(as.name(var)), by = c("Player_ID", "Tour_ID"))
  }
  
  tourWinner <- playerTopCutResults(lineGameResults,"Tour_Winner")
  
  topCutDf <- topCutDf %>%
    left_join(tourWinner, by = c("Player_ID", "Tour_ID"))
  
  return(topCutDf)
}

playerPrintTourcols <- function(){
  cols <- c("Season",
            "Tour_Series",
            "Tour_Name",
            "Player_Name",
            "W",
            "L",
            "D",
            "Top_Cut_W",
            "Top_Cut_L",
            "Tour_Winner",
            "Top_2",
            "Top_4",
            "Top_8",
            "Top_12",
            "Opp_W",
            "Opp_L",
            "Opp_Win_Pct",
            "PB_Name",
            "Tour_Start_Date"
            )
  return(cols)
}

getPlayerLockedDeckInfo <- function(deckInfo,tourInfo){
  playerLockedDeckInfo <- tourInfo %>%
    filter(Locked_Lists == "Yes") %>%
    left_join(deckInfo, by = "Tour_ID") %>%
    select("Tour_ID",
           "Player_ID",
           "PB_Name")
  return(playerLockedDeckInfo)
}

getCalcPlayerStats <- function(playerTourResults){
  playerTourResults <- playerTourResults %>%
    mutate(Opp_Win_Pct = Opp_W/(Opp_W+Opp_L))
  return(playerTourResults)
}

getPlayerStats <- function(lineGameResults,tourInfo,playerInfo){
  
  #get players' swiss record
  swissStandings <- getPlayerSwissStandings(lineGameResults)
  
  #get players' strength of schedule
  sos <- getPlayerSos(lineGameResults,swissStandings)
  
  #get top cut record
  topCutRecord <- getPlayerTopCutRecord(lineGameResults)
  
  #get top cut levels
  topCutDf <- getPlayerTopCutLevels(lineGameResults)
  
  #get locked Deck Info
  playerLockedDeckInfo <- getPlayerLockedDeckInfo(deckInfo,tourInfo)
  
  playerTourResults <- swissStandings %>%
    left_join(topCutRecord, by = c("Player_ID",
                                "Tour_ID")) %>%
    left_join(topCutDf, by = c("Player_ID",
                                "Tour_ID")) %>%
    left_join(sos, by = c("Player_ID",
                                "Tour_ID")) %>%
    left_join(tourInfo, by = "Tour_ID") %>%
    left_join(playerInfo, by = "Player_ID") %>%
    left_join(playerLockedDeckInfo, by = c("Player_ID",
                                           "Tour_ID"))
  
  #calculate columns
  playerTourResults <- getCalcPlayerStats(playerTourResults)
  
  #get print columns
  cols <-playerPrintTourcols()
  
  playerTourResults <- playerTourResults %>%
    ungroup() %>%
    arrange(Season,Tour_Start_Date,Player_Name) %>%
    select(all_of(cols))
  
}

# playerStats <- getPlayerStats(lineGameResults,tourInfo,playerInfo)

getCardList <- function(cardInfo,tourStats,expInfo){
  
  cardList <- cardInfo %>%
    filter(Card_Type != "Conjuration",
           Card_Type != "Phoenixborn",
           Card_Type != "Conjured Alteration") %>%
    left_join(expInfo, by = c("Precon_ID", "Precon_Name")) %>%
    merge(tourStats,
          all=TRUE) %>%
    rename(Exp_Spoil_Date = Spoil_Date,
           Exp_Receive_Date = Receive_Date,
           Card_Name = Card_Name) %>%
    mutate(Legality = Tour_Start_Date > Exp_Spoil_Date,
           Tot_Decks = Legality * Tot_Decks,
           Tot_Swiss_Decks = Legality * Tot_Swiss_Decks,
           Tot_Top_Cut_Decks = Legality * Tot_Top_Cut_Decks) 
  
  return(cardList)
}

getCardTourCounts <- function(deckCards, deckInfo, deckStats) {
  cardTourCounts <- deckCards %>%
    left_join(deckStats, by = "Deck_ID") %>%
    left_join(deckInfo, by = "Deck_ID") %>%
    group_by(Tour_ID,Card_ID,Card_Name) %>%
    summarize(Num_Decks = sum(!is.na(Deck_ID)),
              Num_Copies = sum(Number, na.rm=TRUE),
              Swiss_Decks = sum(!Round_Num %in% c("TC","All")),
              Top_Cut_Decks = sum(Round_Num == "TC"),
              across(.cols = c("W", "L", "Top_Cut_W", "Top_Cut_L"),
                     .fns = ~ sum(., na.rm = TRUE)),
              across(.cols = c(num_range("Top_",1:1000),"Tour_Winner"),
                     .fns = ~ sum(!is.na(.))
              ))
  
  return(cardTourCounts)
}

getCalcCardTourData <- function(cardList,cardTourCounts){
  calcCardTourData <- cardList %>%
    left_join(cardTourCounts, by = c("Card_Name","Card_ID","Tour_ID")) %>%
    
    #replace NA's with 0
    mutate_if(is.numeric, replace_na, replace = 0) %>%
    
    #create all ratios
    mutate(Deck_Pct = Num_Decks / Tot_Decks,
           Avg_Copies = Num_Copies / Num_Decks,
           Swiss_Deck_Pct = Swiss_Decks / Tot_Swiss_Decks,
           Top_Cut_Deck_Pct = Top_Cut_Decks / Tot_Top_Cut_Decks,
           Win_Pct = W/(W+L),
           Top_Cut_Win_Pct = Top_Cut_W/(Top_Cut_W+Top_Cut_L),
           
           #Calculate Adjusted W/L
           Adj_W = W-Swiss_Mirrors,
           Adj_L = L-Swiss_Mirrors,
           Adj_Top_Cut_W = Top_Cut_W - Top_Cut_Mirrors,
           Adj_Top_Cut_L = Top_Cut_L - Top_Cut_Mirrors,
           Adj_Win_Pct = Adj_W/(Adj_W+Adj_L),
           Adj_Top_Cut_Win_Pct = Adj_Top_Cut_W/(Adj_Top_Cut_W+Adj_Top_Cut_L)
           ) %>%
    
    #create lag deck pct
    group_by(Card_ID,
             Card_Name,
             Tour_Series) %>%
    mutate(Deck_Pct_Change = Deck_Pct - lag(Deck_Pct),.after = Deck_Pct)
  
  return(calcCardTourData)
}

cardPrintTourCols <- function(){
  cols <- c("Season",
            "Tour_Series",
            "Tour_Name",
            "Card_Name",
            "Num_Decks",
            "Num_Copies",
            "Deck_Pct",
            "Deck_Pct_Change",
            "Avg_Copies",
            "W",
            "L",
            "Win_Pct",
            "Top_Cut_W",
            "Top_Cut_L",
            "Top_Cut_Win_Pct",
            "Tour_Winner",
            "Top_2",
            "Top_4",
            "Top_8",
            "Top_12",
            "Swiss_Deck_Pct",
            "Top_Cut_Deck_Pct",
            "Adj_W",
            "Adj_L",
            "Adj_Top_Cut_W",
            "Adj_Top_Cut_L",
            "Adj_Win_Pct",
            "Adj_Top_Cut_Win_Pct",
            "Tot_Decks",
            "Tour_Start_Date"
            )
  
  return(cols)
}

getCardTourStats <- function(cardInfo,expInfo,deckStats,tourStats,deckCards,lineGameResults) {

  cardList <- getCardList(cardInfo,tourStats,expInfo)
  
  cardTourCounts <- getCardTourCounts(deckCards, deckInfo, deckStats)
  
  cardTourMirrors <- getCardTourMirror(deckCards,lineGameResults)
  
  cardTourCounts <- cardTourCounts %>%
    left_join(cardTourMirrors, by = c("Tour_ID",
                                      "Card_Name"))
  
  calcCardTourData <- getCalcCardTourData(cardList,cardTourCounts)

   cols <- cardPrintTourCols()

   calcCardTourData <- calcCardTourData %>%
     ungroup() %>%
     arrange(Season,Tour_Start_Date,Card_Name) %>%
     select(all_of(cols))
    
  return(calcCardTourData)
}

# cardTourStats <- getCardTourStats(cardInfo,expInfo,deckStats,tourStats)

getCardTourMirror <- function(deckCards,lineGameResults){
  mirrors <- lineGameResults %>%
    left_join(deckCards, by = "Deck_ID") %>%
    inner_join(deckCards, by = c("Opp_Deck_ID" = "Deck_ID",
                                 "Card_Name" = "Card_Name",
                                 "Card_ID" = "Card_ID")) %>%
    group_by(Tour_ID,
             Card_Name) %>%
    summarise(Swiss_Mirrors = sum(Game_Type == "Swiss")/2,
              Top_Cut_Mirrors = sum(Game_Type != "Swiss")/2)
  
  return(mirrors)
}



