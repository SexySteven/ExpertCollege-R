
topic <- 145577
Aantal <- 8
Answer <- subset(Geordend, topic_id== topic & ordere == Aantal)
Gemiddeld <- mean(Answer$score)
Gemiddeld


names <- c("TopicID", "Number")
teller <- data.frame(c(unique(outputTabel$topic_id)), 0)
colnames(teller)<- names


#Creeer tabel, vector, matrix .. whatever om topics te tellen die al geweest zijn
for (i in 1 : nrow(teller)){
  if (topic==teller$TopicID[i]){
    teller$Number[i] <- teller$Number[i] + 1
  }
} 

#Reset de teller(Number) in de data.frame naar 0
teller$Number<- 0  