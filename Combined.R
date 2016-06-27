################INITIALISATIE (klaarzetten van tabellen voor simulatie)#############################   

#Tabel voor COUNTER waarin alle Topics zijn opgenomen. Twee kolommen; TopicID en Number.
#"Number" wordt op 0 gezet
names <- c("TopicID", "count")
counter <- data.frame(c(unique(outputTabel$topic_id)), 0)
colnames(counter)<- names

#Tabel voor ORDERTABLE waarin alle users + topics zijn gerangschikt op voorkomen 1,2,..   
orderTable <- outputTabel[order(outputTabel$user_id,outputTabel$topic_id, outputTabel$sequentie),]
print(nrow(orderTable))
for (i in 1: (nrow(orderTable)-1)){
  if (orderTable$user_id[i]== orderTable$user_id[i+1] 
      & orderTable$topic_id[i]== orderTable$topic_id[i+1]){
        orderTable$ordere[i+1]<- (orderTable$ordere[i] + 1)
  }
}
orderTable$id <- NULL
orderTable$datetimestamp <- NULL
orderTable$exercise_id <- NULL
orderTable$sequentie <- NULL

#Tabel voor TEMPTABLE preparen/opruimen
inputTable <- outputTabel
inputTable$id <- NULL
inputTable$datetimestamp <- NULL
inputTable$exercise_id <- NULL

############################SIMULATIE############################################################

#u is het aantal gebruikers
for (u in 1:5){
  TempTable <- inputTable
  counter$count<- 0 

  #t is het aantal leaftopics diep
  for(t in 1:15){
    i <- 1

#COUNTER-functie    
#Topicteller per gebruiker; hierin wordt bijgehouden hoe vaak een gebruiker een topic voorgeschoteld
#krijgt. 
#Iedere keer dat een topic wordt gepresenteerd, wordt de waarde "count" in de tabel "counter"
#met 1 opgehoogd. De waarde wordt daarnaast weggeschreven in de variabele "cnt", deze wordt gebruikt
#in de ORDERTABLE-functie

    SimInput_topic_id <- 145582
    
    for (q in 1 : nrow(counter)){
      if (SimInput_topic_id==counter$TopicID[q]){
        counter$count[q] <- counter$count[q] + 1
        cnt<-counter$count[q]
      }
    }

#ORDERTABLE-functie     
#Berekent gemiddelde score die gebruikers op het desbetreffende topic hebben behaald
#cnt = aantal keer dat het topic voorbij is gekomen, als dit hoger is dan de bestaande data-set
#dan worden de laatst bekende waarden gepakt.
#Gemiddeld = de kans op goed beantwoorden.
    
    temp <- subset(orderTable, topic_id==SimInput_topic_id)
    if (max(range(temp$ordere))< cnt) cnt<- max(range(temp$ordere))
    Answer <- subset(orderTable, topic_id== topic & ordere == cnt)
    Gemiddeld <- mean(Answer$score)
    temp <- NULL
    Answer <- NULL    
    
#SeqSub is een subset van de TempTable (userdata) van alle rijen waarvoor geldt dat de sequentie
#en het topicId gelijk zijn aan de input van het scoringsalgoritme, namelijk; 
#SimInput_topic_id en SimInput_sequentie.
    
    SeqSub <- subset(TempTable,sequentie == SimInput_sequentie & topic_id == SimInput_topic_id)
    GemKans <- mean(SeqSub[["score"]])
    RestKans <- 1-GemKans


 
#Bereken het antwoord en deze kan worden doorgestuurd naar het scoringsalgoritme.
#Als GoedFout = 1, dan is het juiste antwoord gegeven. Bij een waarde 0 het verkeerde.
    
    GoedFout<- sample(c(1,0) , size = 1, replace=FALSE, prob=c(GemKans, RestKans)) 
  
#Uit de TempTable worden alle gebruikers verwijderd die een ander antwoord hadden gegeven.
#Hiermee wordt de TempTable een stuk kleiner en zijn alleen die gebruikers over die in het 
#gekozen pad zitten.
    
    SeqSubAlt <-subset(SeqSub, score != GoedFout)
    if (nrow(SeqSubAlt) >0){
      for (i in 1:length(SeqSubAlt$user_id))  {
        TempTable <- subset(TempTable, user_id != SeqSubAlt$user_id[i])
      }
    }
    TempTable <- subset(TempTable, sequentie > t)
  }
}

