#AIL 06 2022 Script
library(googlesheets4)

#Parameters
googleSheetID <- "1FZ3rV8uBOyxD09hh2AgaTxlO7xJqriqgB048qluoyKg"
tourId <- "AIL06"
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

#Extra formatting for AIL 06
newGameResults$Losing_Blood_Points <- NA
newGameResults <- newGameResults%>%
  select(c(1,2,3,6,4,5))

#Get CardInfo
assign("cardInfo", dbReadTable(conn, "Card Info"),
                   envir = .GlobalEnv)

#Format newGameResults
newGameResults <- formatNewGameResults(newGameResults, conn, tourLineInfo, tourId)

#Extract newgameresults
if (nrow(newGameResults)>0) {
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
