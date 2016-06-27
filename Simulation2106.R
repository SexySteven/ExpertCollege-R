topic_id == SimInput_topic_id
names <- c("id", "user_id", "datetimestamp", "score", "sequentie", "topic_id", "exercise_id", "ordere")
colnames(outputTabel) <- names
outputTabel$id <- NULL
outputTabel$datetimestamp <- NULL
outputTabel$exercise_id <- NULL


#u is het aantal gebruikers
for (u in 1:10){
  TempTable <- outputTabel
  GK <- 0 
  Mtrx <- matrix()
#t is het aantal leaftopics diep
    for(t in 1:100){
      i <- 1
      SeqSub <- subset(TempTable,sequentie == t ) #toevoegen " & topic_id == SimInput_topic_id "

      GemKans <- mean(SeqSub[["score"]])
      RestKans <- 1-GemKans
      GK[t] <-GemKans
        if (t > 1){  
          A <- c(A,GK[t])
        }
        else
          A<-GK[t]

          if (GemKans == 0 | RestKans == 0) GoedFout<-GemKans
          else GoedFout<- sample(c(1,0) , size = 1, replace=FALSE, prob=c(GemKans, RestKans)) 
  
    SeqSubAlt <-subset(SeqSub, score != GoedFout)
     if (nrow(SeqSubAlt) >0){
        for (i in 1:length(SeqSubAlt$user_id))  {
        TempTable <- subset(TempTable, user_id != SeqSubAlt$user_id[i])
        }
      }
    
    TempTable <- subset(TempTable, sequentie > t)
    }
  if (u > 1){  
    C <- c(C,A)
  }
  else
    C<- A
}
Mtrx <- t(matrix(C, ncol = u))
for (i in 1:50) T<- colMeans(Mtrx)
plot(T)
Q<- as.data.frame(Mtrx)
