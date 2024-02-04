#SB Spring 2022 Script

library(googlesheets4)

#Parameters
googleSheetID <- "1czZfB9XNuniDZOGuJk5keSOcz57gUljyG2eMG614mAY"
tourId <- "SB07"
dataSheet <- "Form Responses 1"
gameType <- "Swiss"
resultType <- "Match"
gsRawData <- "1jLzTvr5c1vfZjcEPEjawdqFk-jhIXJC8GkY0-pQPu8k"

#future want to make this public
gs4_auth(email = "kaile.phelps@gmail.com")

#Connect to DB
source("C:/Users/kphelps/OneDrive - Epic/Desktop/Kaile's Things/R Projects/AshesProjects/LoadAshesDB.R")
conn <- connect2db()

#Read Game Log Functions
source("C:/Users/kphelps/OneDrive - Epic/Desktop/Kaile's Things/R Projects/AshesProjects/ReadGameLogs.R")

#Tour Line Info
tourLineInfo <- getTourLineInfo(conn,tourId)

#get Games to be processed
newGameResults <- read_sheet(googleSheetID,dataSheet)

#Extra formatting for SB7
newGameResults <- newGameResults%>%
  select(c(1,5,6,4))

#Format newGameResults
newGameResults <- formatNewGameResults(newGameResults, conn, tourLineInfo, tourId)

#Extract newgameresults
if (nrow(newGameResults) > 0){
  newGameResults <- apply(newGameResults,1,extractGameLine,
                          tourLineInfo = tourLineInfo,
                          conn = conn,
                          gameType = gameType,
                          resultType = resultType,
                          gsRawData = gsRawData)
  newGameResults <- do.call(rbind,newGameResults)
  newGameResults <- as.data.frame(newGameResults)
}


#update Last Extract Date
updateLastExtractDate(tourInfo,tourId,gsRawData,conn)

dbDisconnect(conn)
