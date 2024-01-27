#head to head records by player

getH2HPlayerMatrix <- function(lineGameResults) {
  h2hResults <- lineGameResults %>%
    filter(!is.na(Player_ID),
           !is.na(Opp_Player_ID)) %>%
    select(Player_ID,
           Opp_Player_ID,
           Result) %>%
    mutate(Player_ID = replace(Player_ID,Player_ID!="",toupper(Player_ID))) %>%
    mutate(Opp_Player_ID = replace(Opp_Player_ID,Opp_Player_ID!="",toupper(Opp_Player_ID))) %>%
    group_by(Player_ID,
             Opp_Player_ID) %>%
    table()
  
  rowWinners <- h2hResults[,,"W"] %>%
    as.matrix()

  colWinners <- h2hResults[,,"L"] %>%
    as.matrix()

  h2hPlayerMatrix <- matrix(paste(colWinners,rowWinners,sep=" - "),
                 nrow = nrow(rowWinners),
                 dimnames = dimnames(h2hResults[,,"W"])) %>%
    as.data.frame()
  
  rownames(h2hPlayerMatrix) <- paste("vs. ",rownames(rowWinners))
  
  return(h2hPlayerMatrix)
}

getH2HPBMatrix <- function(lineGameResults) {
  h2hResults <- lineGameResults %>%
    filter(!is.na(Deck_ID),
           !is.na(Opp_Deck_ID)) %>%
    left_join(deckInfo,
              by = "Deck_ID") %>%
    left_join(deckInfo,
              by = c("Opp_Deck_ID"="Deck_ID")) %>%
    rename(PB_Name = PB_Name.x,
           Opp_PB_Name = PB_Name.y) %>%
    select(PB_Name,
           Opp_PB_Name,
           Result) %>%
    group_by(PB_Name,
             Opp_PB_Name) %>%
    table()
  
  rowWinners <- h2hResults[,,"W"] %>%
    as.matrix()
  
  colWinners <- h2hResults[,,"L"] %>%
    as.matrix()
  
  h2hPBMatrix <- matrix(paste(colWinners,rowWinners,sep=" - "),
                        nrow = nrow(rowWinners),
                        dimnames = dimnames(h2hResults[,,"W"])) %>%
    as.data.frame()
  
  rownames(h2hPBMatrix) <- paste("vs. ",rownames(rowWinners))
  
  return(h2hPBMatrix)
}


test <- gameResults %>%
  left_join(deckInfo, by = c("Winning_Deck_ID" = "Deck_ID")) %>%
  rename(Winning_PB = PB_Name) %>%
  left_join(deckInfo, by = c("Losing_Deck_ID" = "Deck_ID")) %>%
  rename(Losing_PB = PB_Name) %>%
  filter(
         Winning_PB == "Aradel Summergaard") %>%
  select(Tour_ID,
         Game_Type,
         Winning_Player_ID,
         Winning_PB,
         Losing_Player_ID,
         Losing_PB)



  
  select(Player_ID,
         Opp_Player_ID,
         Result) %>%
  mutate(Player_ID = replace(Player_ID,Player_ID!="",toupper(Player_ID))) %>%
  mutate(Opp_Player_ID = replace(Opp_Player_ID,Opp_Player_ID!="",toupper(Opp_Player_ID))) %>%
  group_by(Player_ID,
           Opp_Player_ID) %>%
  table()

rowWinners <- h2hResults[,,"W"] %>%
  as.matrix()

colWinners <- h2hResults[,,"L"] %>%
  as.matrix()

h2hPlayerMatrix <- matrix(paste(colWinners,rowWinners,sep=" - "),
                          nrow = nrow(rowWinners),
                          dimnames = dimnames(h2hResults[,,"W"])) %>%
  as.data.frame()




computePlayerH2HRecord <- function(plyr,opponent){
  wins <- subset(table,Winning_Player_ID == plyr&Losing_Player_ID == opponent)
  wins <- nrow(wins)
  losses <- subset(table,Winning_Player_ID == opponent&Losing_Player_ID == plyr)
  losses <- nrow(losses)
  return(paste(wins,losses,sep = "-"))
}

computePbH2HRecord <- function(pb,pbOpp){
  wins <- subset(table,Winning_Player_ID == pb&Losing_Player_ID == pbOpp)
  wins <- nrow(wins)
  losses <- subset(table,Winning_Player_ID == pbOpp&Losing_Player_ID == pb)
  losses <- nrow(losses)
  return(paste(wins,losses,sep = "-"))
}

vComputePlayerH2HRecord <- Vectorize(computePlayerH2HRecord)

x <- unique(deckInfo$Player)
y <- x
table <- subset(gameResults,Game_Type!="Bye"&Game_Type!="Drop")
results <- data.frame(outer(x,y,FUN = vComputePlayerH2HRecord))
rownames(results)<- x
y <- paste("vs",x)
colnames(results) <- y

plyr <- x[10]
versus <- x[15]
computePlayerH2HRecord(plyr,versus)
subset(table,Winning_Player_ID == plyrLosing_Player_ID == opponent)

test <- merge(gameResults,deckInfo, by.x = "Winning_Deck_ID",by.y="Deck_ID")
test <- merge(test,deckInfo,by.x = "Losing_Deck_ID",by.y="Deck_ID")

winLoss <- function(pbName){
  wins <- nrow(subset(test,PB_Name.x==pbName))
  losses <- nrow(subset(test,PB_Name.y==pbName))
  return(paste(wins,losses,sep="-"))
}
answer <- lapply(x,winLoss)
answer <- as.character(answer)
x <- unique(deckInfo$PB_Name)
x <-data.frame(x)
x$record <- answer


pbHistory <- function(pb1, pb2, gameResults, deckInfo, tourInfo) {
  df <- gameResults %>%
    left_join(tourInfo, by = "Tour_ID") %>%
    left_join(deckInfo, by = c("Winning_Deck_ID" = "Deck_ID",
                               "Tour_ID" = "Tour_ID")) %>%
    rename(Winning_PB = PB_Name,
           Winning_Deck_Link = Deck_Link,
           Winning_Deck_Name= Deck_Name) %>%
    left_join(deckInfo, by = c("Losing_Deck_ID" = "Deck_ID",
                               "Tour_ID" = "Tour_ID")) %>%
    rename(Losing_PB = PB_Name,
           Losing_Deck_Link = Deck_Link,
           Losing_Deck_Name= Deck_Name) %>%
    select(Tour_Name,
           Round_Num.x,
           Game_Type,
           Winning_PB,
           Losing_PB,
           Winning_Player_ID,
           Losing_Player_ID,
           Winning_Deck_Link,
           Losing_Deck_Link,
           Winning_Deck_Name,
           Losing_Deck_Name) %>%
    rename(Round_Num = Round_Num.x) %>%
    filter(Winning_PB == pb1 & Losing_PB ==pb2 |
             Winning_PB == pb2 & Losing_PB == pb1)
  
}

df1 <- pbHistory("Odette Diamondcrest","Maeoni Viper",gameResults,deckInfo,tourInfo)
df2 <- pbHistory("Harold Westraven","Coal Roarkwin",gameResults,deckInfo,tourInfo)
df3 <- pbHistory("Odette Diamondcrest","Brennen Blackcloud",gameResults,deckInfo,tourInfo)
df4 <- pbHistory("Echo Greystorm","Echo Greystorm",gameResults,deckInfo,tourInfo)

dfAll <- rbind(df1,df2,df3,df4)
write.csv(dfAll,"C:/Users/kphelps/OneDrive - Epic/Desktop/Kaile's Things/R Projects/AshesData/H2H_History2.csv")
