setwd("C:\\Users\\Pieter Hornix\\Desktop\\UNI\\Blok 3\\M&S\\")
load("allData.Rdata")

### MODEL ###
# functies
motivatie_functie_28 <- function(x_waarde){3*1.3^x_waarde+50};motivatie_functie_14 <- function(x_waarde){6*1.3^x_waarde+50};motivatie_functie_7 <- function(x_waarde){9*1.3^x_waarde+50}
mot = function(x, y, conditiepunten){
  y = (conditiepunten * x) + 40
}

# plot
par(mfrow = c(1,2))
  
#Exponentieel
plot(0:12,motivatie_functie_28(0:12), type = 'b', xlim = c(0,12), ylim = c(0,250), xlab = "Aantal woorden", ylab = "Motivatiepunten", col = "hotpink")
lines(0:12,motivatie_functie_14(0:12),type = 'b', col = "green")
lines(0:12,motivatie_functie_7(0:12), type = 'b',col = "blue")
legend("topleft", legend=c("7", "14", "28"), fill = c("Hotpink", "Green", "Blue"),  cex = 1)
  
# Lineair
plot(0:10,mot(0:10,0,7), xlab = "words", ylab = "motivatie",xlim = c(0,12),ylim = c(0,400), type = "b", col = "hotpink")
lines(0:10, mot(0:10,0,14), type = "b", col = "green")
lines(0:10, mot(0:10,0,28), type = "b", col = "blue")
legend("topleft", legend=c("7", "14", "28"), fill = c("Hotpink", "Green", "Blue"),  cex = 1)
### MODEL ###

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
points_easy1 <- function(subject){
  x<-1
  while (x<=length(tijd_per_woord_easy(subject))) {
    points(x, tijd_per_woord_easy(subject)[x], pch = 0)
    x<-x+1
  }
}
points_easy2 <- function(subject){
  x<-1
  while (x<=length(tijd_per_woord_easy(subject))) {
    points(x, tijd_per_woord_easy(subject)[x], pch = 3)
    x<-x+1
  }
}
points_easy3 <- function(subject){
  x<-1
  while (x<=length(tijd_per_woord_easy(subject))) {
    points(x, tijd_per_woord_easy(subject)[x], pch = 1)
    x<-x+1
  }
}
x_easy <- function(){
  result<- c()
  for (k in Opgeef_frame_easy$subject) {
    result<- c(result, length(eindtijden_easy(k)))
  }
  return(result)
}
y_easy <- function(){
  result<- c()
  for (subject in Opgeef_frame_easy$subject) {
    eindtijd_dualtask_easy <- allData[allData$SubjectNr == subject & allData$partOfExperiment == "dualTask" &allData$scrabbleCondition=="easy" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1]
    tijd_laatste_woord <- eindtijden_easy(subject)[length(eindtijden_easy(subject))]
    starttijd_scrabble <- (allData[allData$SubjectNr == subject &allData$scrabbleCondition=="easy"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
    y<-((eindtijd_dualtask_easy - tijd_laatste_woord) - starttijd_scrabble)
    result<- c(result, y)
  }
  return(result)
}
actual_giving_up_time_easy <- function(subject){
  eindtijd_dualtask_easy <- allData[allData$SubjectNr == subject & allData$partOfExperiment == "dualTask" &allData$scrabbleCondition=="easy" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1]
  tijd_laatste_woord <- eindtijden_easy(subject)[length(eindtijden_easy(subject))]
  starttijd_scrabble <- (allData[allData$SubjectNr == subject &allData$scrabbleCondition=="easy"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
  y<-((eindtijd_dualtask_easy - tijd_laatste_woord) - starttijd_scrabble)
  #result <- c(result, (eindtijd_dualtask_easy - tijd_laatste_woord) - starttijd_scrabble, length(eindtijden_easy(subject)))
  points(length(eindtijden_easy(subject)), y)
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
eindtijden_hard<- function(subject){
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
points_hard1 <- function(subject){
  x<-1
  while (x<=length(tijd_per_woord_hard(subject))) {
    points(x, tijd_per_woord_hard(subject)[x], pch = 0, col = "red")
    x<-x+1
  }
}
points_hard2 <- function(subject){
  x<-1
  while (x<=length(tijd_per_woord_hard(subject))) {
    points(x, tijd_per_woord_hard(subject)[x], pch = 3, col = "red")
    x<-x+1
  }
}
points_hard3 <- function(subject){
  x<-1
  while (x<=length(tijd_per_woord_hard(subject))) {
    points(x, tijd_per_woord_hard(subject)[x], pch = 1, col = "red")
    x<-x+1
  }
}
x_hard <- function(){
  result<- c()
  for (k in Opgeef_frame_hard$subject) {
    result<- c(result, length(eindtijden_hard(k)))
  }
  return(result)
}
y_hard <- function(){
  result<- c()
  for (subject in Opgeef_frame_hard$subject) {
    eindtijd_dualtask_hard <- allData[allData$SubjectNr == subject & allData$partOfExperiment == "dualTask" &allData$scrabbleCondition=="hard" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1]
    tijd_laatste_woord <- eindtijden_hard(subject)[length(eindtijden_hard(subject))]
    starttijd_scrabble <- (allData[allData$SubjectNr == subject &allData$scrabbleCondition=="hard"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
    y<-((eindtijd_dualtask_hard - tijd_laatste_woord) - starttijd_scrabble)
    result<- c(result, y)
  }
  return(result)
}
actual_giving_up_time_hard <- function(subject){
  eindtijd_dualtask_hard <- allData[allData$SubjectNr == subject & allData$partOfExperiment == "dualTask" &allData$scrabbleCondition=="hard" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1]
  tijd_laatste_woord <- eindtijden_hard(subject)[length(eindtijden_hard(subject))]
  starttijd_scrabble <- (allData[allData$SubjectNr == subject &allData$scrabbleCondition=="hard"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
  y<-((eindtijd_dualtask_hard - tijd_laatste_woord) - starttijd_scrabble)
  #result <- c(result, (eindtijd_dualtask_hard - tijd_laatste_woord) - starttijd_scrabble, length(eindtijden_hard(subject)))
  points(length(eindtijden_hard(subject)), y, col = "red")
}
### HARD ###

### LIJSTJES ###
werkt_easy1<-c(1,2,3,4,8,10,11,12,13,15,16,20,22,23,26,27,28,29,30,32)#14,31
werkt_easy2<-c(103,105,106,107,108,109,112,115,116,117,118,119,122,125,126,127)#113
werkt_easy3<-c(201,202,203,204,206,207,208,209,210,211,213,215,217,218,219,221,
              222,223,226,229,233,234,235,236,237,240,241,242,244,246)#205,212,230
werkt_hard1<-c(1,3,4,5,6,8,10,11,12,14,17,25,26,30,31,32)#13,29
werkt_hard2<-c(103,105,106,107,108,111,112,113,116,117,118,119,120,122,125,127)#126
werkt_hard3<-c(201,202,204,205,208,209,210,211,212,213,214,215,217,226,
              231,233,234,235,236,239,240,241,242,243,244,246)#206,208,218,220,223,229,230
### LIJSTJES ###

### AANTALLEN ###
# 32 proefpersonen
# 27 proefpersonen
# 48 proefpersonen
### AANTALLEN ###

### OPGEEF DATA FRAME AANMAKEN ###
Opgeef_frame_easy <- data.frame(matrix(ncol = 3, nrow = length(unique(c(werkt_easy1, werkt_easy2, werkt_easy3)))))
colnames(Opgeef_frame_easy) <- c("subject", "x", "y")
Opgeef_frame_easy$subject <- c(werkt_easy1, werkt_easy2, werkt_easy3)
Opgeef_frame_easy$x <- x_easy()
Opgeef_frame_easy$y <- y_easy()

Opgeef_frame_hard <- data.frame(matrix(ncol = 3, nrow = length(unique(c(werkt_hard1, werkt_hard2, werkt_hard3)))))
colnames(Opgeef_frame_hard) <- c("subject", "x", "y")
Opgeef_frame_hard$subject <- c(werkt_hard1, werkt_hard2, werkt_hard3)
Opgeef_frame_hard$x <- x_hard()
Opgeef_frame_hard$y <- y_hard()
### OPGEEF DATA FRAME AANMAKEN ###

### DATA FRAME ###
# easy
easy_1 <- data.frame(matrix(ncol = 3, nrow = length(unique(werkt_easy1))))
colnames(easy_1) <- c("subject", "x", "y")
easy_1$subject <- werkt_easy1
x_easy_1 <- function(){
  result<- c()
  for (k in easy_1$subject) {
    result<- c(result, length(eindtijden_easy(k)))
  }
  return(result)
}
y_easy_1 <- function(){
  result<- c()
  for (subject in easy_1$subject) {
    eindtijd_dualtask_easy <- allData[allData$SubjectNr == subject & allData$partOfExperiment == "dualTask" &allData$scrabbleCondition=="easy" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1]
    tijd_laatste_woord <- eindtijden_easy(subject)[length(eindtijden_easy(subject))]
    starttijd_scrabble <- (allData[allData$SubjectNr == subject &allData$scrabbleCondition=="easy"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
    y<-((eindtijd_dualtask_easy - tijd_laatste_woord) - starttijd_scrabble)
    result<- c(result, y)
  }
  return(result)
}
easy_1$x <- x_easy_1()
easy_1$y <- y_easy_1()

easy_2 <- data.frame(matrix(ncol = 3, nrow = length(unique(werkt_easy2))))
colnames(easy_2) <- c("subject", "x", "y")
easy_2$subject <- werkt_easy2
x_easy_2 <- function(){
  result<- c()
  for (k in easy_2$subject) {
    result<- c(result, length(eindtijden_easy(k)))
  }
  return(result)
}
y_easy_2 <- function(){
  result<- c()
  for (subject in easy_2$subject) {
    eindtijd_dualtask_easy <- allData[allData$SubjectNr == subject & allData$partOfExperiment == "dualTask" &allData$scrabbleCondition=="easy" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1]
    tijd_laatste_woord <- eindtijden_easy(subject)[length(eindtijden_easy(subject))]
    starttijd_scrabble <- (allData[allData$SubjectNr == subject &allData$scrabbleCondition=="easy"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
    y<-((eindtijd_dualtask_easy - tijd_laatste_woord) - starttijd_scrabble)
    result<- c(result, y)
  }
  return(result)
}
easy_2$x <- x_easy_2()
easy_2$y <- y_easy_2()

easy_3 <- data.frame(matrix(ncol = 3, nrow = length(unique(werkt_easy3))))
colnames(easy_3) <- c("subject", "x", "y")
easy_3$subject <- werkt_easy3
x_easy_3 <- function(){
  result<- c()
  for (k in easy_3$subject) {
    result<- c(result, length(eindtijden_easy(k)))
  }
  return(result)
}
y_easy_3 <- function(){
  result<- c()
  for (subject in easy_3$subject) {
    eindtijd_dualtask_easy <- allData[allData$SubjectNr == subject & allData$partOfExperiment == "dualTask" &allData$scrabbleCondition=="easy" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1]
    tijd_laatste_woord <- eindtijden_easy(subject)[length(eindtijden_easy(subject))]
    starttijd_scrabble <- (allData[allData$SubjectNr == subject &allData$scrabbleCondition=="easy"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
    y<-((eindtijd_dualtask_easy - tijd_laatste_woord) - starttijd_scrabble)
    result<- c(result, y)
  }
  return(result)
}
easy_3$x <- x_easy_3()
easy_3$y <- y_easy_3()

#hard
hard_1 <- data.frame(matrix(ncol = 3, nrow = length(unique(werkt_hard1))))
colnames(hard_1) <- c("subject", "x", "y")
hard_1$subject <- werkt_hard1
x_hard_1 <- function(){
  result<- c()
  for (k in hard_1$subject) {
    result<- c(result, length(eindtijden_hard(k)))
  }
  return(result)
}
y_hard_1 <- function(){
  result<- c()
  for (subject in hard_1$subject) {
    eindtijd_dualtask_hard <- allData[allData$SubjectNr == subject & allData$partOfExperiment == "dualTask" &allData$scrabbleCondition=="hard" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1]
    tijd_laatste_woord <- eindtijden_hard(subject)[length(eindtijden_hard(subject))]
    starttijd_scrabble <- (allData[allData$SubjectNr == subject &allData$scrabbleCondition=="hard"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
    y<-((eindtijd_dualtask_hard - tijd_laatste_woord) - starttijd_scrabble)
    result<- c(result, y)
  }
  return(result)
}
hard_1$x <- x_hard_1()
hard_1$y <- y_hard_1()

hard_2 <- data.frame(matrix(ncol = 3, nrow = length(unique(werkt_hard2))))
colnames(hard_2) <- c("subject", "x", "y")
hard_2$subject <- werkt_hard2
x_hard_2 <- function(){
  result<- c()
  for (k in hard_2$subject) {
    result<- c(result, length(eindtijden_hard(k)))
  }
  return(result)
}
y_hard_2 <- function(){
  result<- c()
  for (subject in hard_2$subject) {
    eindtijd_dualtask_hard <- allData[allData$SubjectNr == subject & allData$partOfExperiment == "dualTask" &allData$scrabbleCondition=="hard" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1]
    tijd_laatste_woord <- eindtijden_hard(subject)[length(eindtijden_hard(subject))]
    starttijd_scrabble <- (allData[allData$SubjectNr == subject &allData$scrabbleCondition=="hard"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
    y<-((eindtijd_dualtask_hard - tijd_laatste_woord) - starttijd_scrabble)
    result<- c(result, y)
  }
  return(result)
}
hard_2$x <- x_hard_2()
hard_2$y <- y_hard_2()

hard_3 <- data.frame(matrix(ncol = 3, nrow = length(unique(werkt_hard3))))
colnames(hard_3) <- c("subject", "x", "y")
hard_3$subject <- werkt_hard3
x_hard_3 <- function(){
  result<- c()
  for (k in hard_3$subject) {
    result<- c(result, length(eindtijden_hard(k)))
  }
  return(result)
}
y_hard_3 <- function(){
  result<- c()
  for (subject in hard_3$subject) {
    eindtijd_dualtask_hard <- allData[allData$SubjectNr == subject & allData$partOfExperiment == "dualTask" &allData$scrabbleCondition=="hard" & allData$userStringLetterTask == "-10" & allData$Eventmessage2 == "OpeningLetter",]$CurrentTime[1]
    tijd_laatste_woord <- eindtijden_hard(subject)[length(eindtijden_hard(subject))]
    starttijd_scrabble <- (allData[allData$SubjectNr == subject &allData$scrabbleCondition=="hard"& allData$partOfExperiment == "dualTask" & allData$LengthOfUserLetterString == "0",]$CurrentTime[1])
    y<-((eindtijd_dualtask_hard - tijd_laatste_woord) - starttijd_scrabble)
    result<- c(result, y)
  }
  return(result)
}
hard_3$x <- x_hard_3()
hard_3$y <- y_hard_3()
### DATA FRAME ###

### PLOT ###
plot(1, type="n", xlab="Hoeveelste woord", ylab="Aantal secondes over woord", xlim=c(0, 40), ylim=c(0, 70))
teken<-function(){
  for (x in werkt_easy1) {
    points_easy1(x)
  }
  for (x in werkt_easy2) {
    points_easy2(x)
  }
  for (x in werkt_easy3) {
    points_easy3(x)
  }
  for (x in werkt_hard1) {
    points_hard1(x)
  }
  for (x in werkt_hard2) {
    points_hard2(x)
  }
  for (x in werkt_hard3) {
    points_hard3(x)
  }
}
teken_give_up <- function(){
  for (x in werkt_easy1) {
    actual_giving_up_time_easy(x)
  }
  for (x in werkt_easy2) {
    actual_giving_up_time_easy(x)
  }
  for (x in werkt_easy3) {
    actual_giving_up_time_easy(x)
  }
  for (x in werkt_hard1) {
    actual_giving_up_time_hard(x)
  }
  for (x in werkt_hard2) {
    actual_giving_up_time_hard(x)
  }
  for (x in werkt_hard3) {
    actual_giving_up_time_hard(x)
  }
}

# alle data punten plot per difficulty
par(mfrow = c(1,2))
plot(Opgeef_frame_easy$x, Opgeef_frame_easy$y, xlab = "Aantal woorden gevonden", ylab = "Giving-up time (sec)", main = "Alle easy")
abline(lm(Opgeef_frame_easy$y ~ Opgeef_frame_easy$x))
plot(Opgeef_frame_hard$x, Opgeef_frame_hard$y, xlab = "Aantal woorden gevonden", ylab = "Giving-up time (sec)", main = "Alle hard")
abline(lm(Opgeef_frame_hard$y ~ Opgeef_frame_hard$x))

# alle data punten plot per difficulty en reward
par(mfrow=c(2,3))
plot(easy_1$x, easy_1$y, xlab = "Aantal woorden gevonden", ylab = "Giving-up time (sec)", main = "Easy; 7")
abline(lm(easy_1$y ~ easy_1$x))
plot(easy_2$x, easy_2$y, xlab = "Aantal woorden gevonden", ylab = "Giving-up time (sec)", main = "Easy; 14")
abline(lm(easy_2$y ~ easy_2$x))
plot(easy_3$x, easy_3$y, xlab = "Aantal woorden gevonden", ylab = "Giving-up time (sec)", main = "Easy; 28")
abline(lm(easy_3$y ~ easy_3$x))
plot(hard_1$x, hard_1$y, xlab = "Aantal woorden gevonden", ylab = "Giving-up time (sec)", main = "Hard; 7")
abline(lm(hard_1$y ~ hard_1$x))
plot(hard_2$x, hard_2$y, xlab = "Aantal woorden gevonden", ylab = "Giving-up time (sec)", main = "Hard; 14")
abline(lm(hard_2$y ~ hard_2$x))
plot(hard_3$x, hard_3$y, xlab = "Aantal woorden gevonden", ylab = "Giving-up time (sec)", main = "Hard; 28")
abline(lm(hard_3$y ~ hard_3$x))

par(mfrow=c(1,2))
plot(1, type = "n", xlab = "Aantal woorden gevonden", ylab = "Giving-up time (sec)", xlim = c(0, 35), ylim = c(0, 40), main = "Easy")
points(easy_1$x, easy_1$y, pch = 0, col = "Hotpink")
points(easy_2$x, easy_2$y, pch = 3, col = "Green")
points(easy_3$x, easy_3$y, pch = 5, col = "Blue")
abline(lm(easy_1$y ~ easy_1$x), col = "Hotpink")
abline(lm(easy_2$y ~ easy_2$x), col = "Green")
abline(lm(easy_3$y ~ easy_3$x), col = "Blue")
legend("topright", legend=c("7", "14", "28"), fill = c("Hotpink", "Green", "Blue"),  cex = 1)
plot(1, type = "n", xlab = "Aantal woorden gevonden", ylab = "Giving-up time (sec)", xlim = c(0, 35), ylim = c(0, 40), main = "Hard")
points(hard_1$x, hard_1$y, pch = 0, col = "Hotpink")
points(hard_2$x, hard_2$y, pch = 3, col = "Green")
points(hard_3$x, hard_3$y, pch = 5, col = "Blue")
abline(lm(hard_1$y ~ hard_1$x), col = "Hotpink")
abline(lm(hard_2$y ~ hard_2$x), col = "Green")
abline(lm(hard_3$y ~ hard_3$x), col = "Blue")
legend("topright", legend=c("7", "14", "28"), fill = c("Hotpink", "Green", "Blue"),  cex = 1)
### PLOT ###


### TESTING ###
#subject6 <- allData[allData$SubjectNr == "6",]
# y_easy <- function(subject){
#   x<-1
#   while (x<=length(tijd_per_woord_easy(subject))) {
#     tijd_per_woord_easy(subject)[x]
#     x<-x+1
#   }
# }
# y_hard <- function(subject){
#   x<-1
#   while (x<=length(tijd_per_woord_easy(subject))) {
#     tijd_per_woord_hard(subject)[x]
#     x<-x+1
#   }
# }
