setwd("C:\\Users\\sabdo\\Google Drive\\Universiteit\\Methoden en Statistiek\\R\\Final Assignment")     ## Set working directory
load("AllData.RData")
allData[allData$SubjectNr == 1 & allData$partOfExperiment == "dualTask", ]$CurrentTime
##
curve(1.15^{-x} * 8 + 2, from=1, to = 20, n= 1, xlab="Aantal Woorden", ylab="Giving Up Time (S)", col = "blue", lwd=2)

#
# Motivatiepunten functies
#

motfun28 <- function(x){3*1.3^x + 50}
motfun14 <- function(x){6*1.3^x + 50}
motfun7  <- function(x){9*1.3^x + 50}
### Grafiek Motivatie
plot(0:12,motfun28(0:12), type = 'b', xlim = c(0,12), ylim = c(0,250), xlab = "Aantal woorden", ylab = "Motivatiepunten")
lines(0:12,motfun14(0:12),type = 'b', col = "green")
lines(0:12,motfun7(0:12),type = 'b', col = "red")
legend("topleft", legend=c("7", "14", "28"), fill = c("Black", "Green", "Red"),  cex = 1)

#
#### Functies Give Up Time 
#

#Dataset subject 1 in dualtask
Subject1data <- allData[allData$SubjectNr == "1" & allData$partOfExperiment == "dualTask",]

### Starttijd dualtask -- easy en hard voor subject 1 
starttijd_dualtask_easy<-(allData[allData$SubjectNr == "1" &allData$scrabbleCondition=="easy"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
starttijd_dualtask_hard<-(allData[allData$SubjectNr == "1" &allData$scrabbleCondition=="hard"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])

### Functies Starttijd dualtask -- easy en hard
funstarttijd_dualtask_easy <- function(x){(allData[allData$SubjectNr == x &allData$scrabbleCondition=="easy"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])}
funstarttijd_dualtask_hard <- function(x) {(allData[allData$SubjectNr == x &allData$scrabbleCondition=="hard"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])}

### Eindtijd dualtask -- easy en hard voor subject 1 
eindtijd_dualtask_hard<-(allData[allData$SubjectNr == "1" &allData$scrabbleCondition=="hard"& allData$partOfExperiment == "dualTask" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1])
eindtijd_dualtask_easy<-(allData[allData$SubjectNr == "1" &allData$scrabbleCondition=="easy"& allData$partOfExperiment == "dualTask" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1])

### Functies Eindtijd dualtask -- easy en hard 
funeindtijd_dualtask_easy <- function(x){(allData[allData$SubjectNr == x &allData$scrabbleCondition=="hard"& allData$partOfExperiment == "dualTask" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1])}
funeindtijd_dualtask_hard <- function(x){(allData[allData$SubjectNr == x &allData$scrabbleCondition=="easy"& allData$partOfExperiment == "dualTask" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1])}


### Total Time gebaseerd op vorige 
TotalTime_hard <- eindtijd_dualtask_hard - starttijd_dualtask_hard
TotalTime_easy <- eindtijd_dualtask_easy - starttijd_dualtask_easy


tijdtussencorrectnewword <- for{yey} 

