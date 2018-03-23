setwd("C:\\Users\\Sony\\Desktop\\UNI\\Blok 3\\MS\\R\\Assignment 4\\")

load("allData.Rdata")

motivatie_functie_28 <- function(x_waarde){3*1.3^x_waarde+50};motivatie_functie_14 <- function(x_waarde){6*1.3^x_waarde+50};motivatie_functie_7 <- function(x_waarde){9*1.3^x_waarde+50}

plot(0:12,motivatie_functie_28(0:12), type = 'b', xlim = c(0,12), ylim = c(0,250), xlab = "Aantal woorden", ylab = "Motivatiepunten");lines(0:12,motivatie_functie_14(0:12),type = 'b', col = "green");lines(0:12,motivatie_functie_7(0:12), type = 'b',col = "red")
legend("topleft", legend=c("7", "14", "28"), fill = c("Black", "Green", "Red"),  cex = 1)



total_time_until_giving_up_easy<- function(subject){
  for (var in allData$SubjectNr) {
    if (var == subject){
      starttijd_dualtask_easy<-(allData[allData$SubjectNr == subject &allData$scrabbleCondition=="easy"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
      eindtijd_dualtask_easy <- allData[allData$SubjectNr == subject & allData$partOfExperiment == "dualTask" &allData$scrabbleCondition=="easy" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1]
      return(eindtijd_dualtask_easy - starttijd_dualtask_easy)
    }
  }
}
total_time_until_giving_up_hard<- function(subject){
  for (var in allData$SubjectNr) {
    if (var == subject){
      starttijd_dualtask_hard<-(allData[allData$SubjectNr == subject &allData$scrabbleCondition=="hard"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
      eindtijd_dualtask_hard <- allData[allData$SubjectNr == subject & allData$partOfExperiment == "dualTask" &allData$scrabbleCondition=="hard" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1]
      return(eindtijd_dualtask_hard - starttijd_dualtask_hard)
    }
  }
}

total_time_until_giving_up_easy(3)
total_time_until_giving_up_hard(3)

allData[allData$SubjectNr == "1" &allData$scrabbleCondition=="easy"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1]


#allData[allData$SubjectNr == 2,]$CurrentTime

subject_1<-allData[allData$SubjectNr==1&allData$partOfExperiment=="dualTask"&allData$scrabbleCondition=="easy",]
eindtijd_subject_1<-subject_1[subject_1$Eventmessage1 == "SwitchWindow",]$CurrentTime[1]

#QUESTIONS#
#
# Why does the function change when negative x-values are added to the equation?
# Niet dus: Een model voorspelt hoeveel motivatie iemand had, door punten toe te voegen per nieuw woord. X in de formule is het aantal motivatie punten
#
#