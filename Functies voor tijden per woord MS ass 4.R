# Functies benodigd om tijden per woord te vinden

### EASY ###
total_time_until_giving_up_easy<- function(subject){
  for (var in allData$SubjectNr) {
    if (var == subject){
      starttijd_dualtask_easy<-(allData[allData$SubjectNr == subject &allData$scrabbleCondition=="easy"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
      eindtijd_dualtask_easy <- allData[allData$SubjectNr == subject & allData$partOfExperiment == "dualTask" &allData$scrabbleCondition=="easy" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1]
      return(eindtijd_dualtask_easy - starttijd_dualtask_easy)
    }
  }
}

eindtijden_easy<- function(subject){
  result <- c()
  for (var in allData$SubjectNr) {
    if (var == subject){
      #eindtijd_dualtask_easy <- allData[allData$SubjectNr == subject & allData$partOfExperiment == "dualTask" &allData$scrabbleCondition=="easy" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1]
      #eindtijd_scrabble <- total_time_until_giving_up_easy(subject)
      starttijd_scrabble <- (allData[allData$SubjectNr == subject &allData$scrabbleCondition=="easy"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
      eindtijd_scrabble <- allData[allData$SubjectNr == subject & allData$partOfExperiment == "dualTask" &allData$scrabbleCondition=="easy" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1]
      eindtijd <- (allData[allData$SubjectNr == subject & allData$Eventmessage2 == "correctNewWord",]$CurrentTime)
      for (tijd in eindtijd){
        if (tijd < eindtijd_scrabble){
          if (tijd > starttijd_scrabble){
            result <- c(result, (tijd-starttijd_scrabble))
          }
        }
      }
      return (result)
    }
  }
}

tijd_per_woord_easy <- function(subject){
  result <- c()
  eindtijden <- eindtijden_easy(subject)
  x <- 0
  for (tijd in eindtijden)
  {
    result <- c(result, (tijd-x))
    x <- tijd
  }
  return(result)
}

time_between_correct_word_easy<- function(subject){
  for (var in allData$SubjectNr){
    if (var==subject){
      starttijd_dualtask_easy<-(allData[allData$SubjectNr == subject &allData$scrabbleCondition=="easy"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
      begintijd <- allData[allData$SubjectNr == subject &allData$scrabbleCondition=="easy"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1]
      if ((allData[allData$eventMessage2 == "correctNewWord" & allData$SubjectNr == subject,]$LocalTime)>begintijd)
      {
        eindtijd <- allData[allData$eventMessage2 == "correctNewWord" & allData$SubjectNr == subject,]$LocalTime
      }
      return(eindtijd - begintijd)
    }
  }
}
### EASY ###


### HARD ###
total_time_until_giving_up_hard<- function(subject){
  for (var in allData$SubjectNr) {
    if (var == subject){
      starttijd_dualtask_hard<-(allData[allData$SubjectNr == subject &allData$scrabbleCondition=="hard"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
      eindtijd_dualtask_hard <- allData[allData$SubjectNr == subject & allData$partOfExperiment == "dualTask" &allData$scrabbleCondition=="hard" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1]
      return(eindtijd_dualtask_hard - starttijd_dualtask_hard)
    }
  }
}

eindtijden_easy<- function(subject){
  result <- c()
  for (var in allData$SubjectNr) {
    if (var == subject){
      #eindtijd_dualtask_easy <- allData[allData$SubjectNr == subject & allData$partOfExperiment == "dualTask" &allData$scrabbleCondition=="easy" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1]
      #eindtijd_scrabble <- total_time_until_giving_up_easy(subject)
      starttijd_scrabble <- (allData[allData$SubjectNr == subject &allData$scrabbleCondition=="hard"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
      eindtijd_scrabble <- allData[allData$SubjectNr == subject & allData$partOfExperiment == "dualTask" &allData$scrabbleCondition=="hard" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1]
      eindtijd <- (allData[allData$SubjectNr == subject & allData$Eventmessage2 == "correctNewWord",]$CurrentTime)
      for (tijd in eindtijd){
        if (tijd < eindtijd_scrabble){
          if (tijd > starttijd_scrabble){
            result <- c(result, (tijd-starttijd_scrabble))
          }
        }
      }
      return (result)
    }
  }
}

tijd_per_woord_hard <- function(subject){
  result <- c()
  eindtijden <- eindtijden_hard(subject)
  x <- 0
  for (tijd in eindtijden)
  {
    result <- c(result, (tijd-x))
    x <- tijd
  }
  return(result)
}

time_between_correct_word_hard<- function(subject){
  for (var in allData$SubjectNr){
    if (var==subject){
      starttijd_dualtask_hard<-(allData[allData$SubjectNr == subject &allData$scrabbleCondition=="hard"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
      begintijd <- allData[allData$SubjectNr == subject &allData$scrabbleCondition=="hard"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1]
      if ((allData[allData$eventMessage2 == "correctNewWord" & allData$SubjectNr == subject,]$LocalTime)>begintijd)
      {
        eindtijd <- allData[allData$eventMessage2 == "correctNewWord" & allData$SubjectNr == subject,]$LocalTime
      }
      return(eindtijd - begintijd)
    }
  }
}
### HARD ###


### AANROEP ###
tijd_per_woord_easy(1)

tijd_per_woord_hard(1)
### AANROEP ###