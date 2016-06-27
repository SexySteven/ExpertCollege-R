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
                                       
#Poging tot connectie maken                          

 library(httr)
 henk <- POST(url = "http://devx.expertcollege.com/Portal/Login.aspx", encode="form", body = list(LoginUserName="Henk", LoginPassword="Floeps"))
 cookies(henk)
 koek<-cookies(henk)$value[1]
 koek
 
 ingrid <- POST(url = "http://devx.expertcollege.com/ExerciseHandler.ashx", encode="raw", content_type_xml())
 content(ingrid)
 
 klaas <- GET (url = "http://devx.expertcollege.com/portal/launcher.aspx?course=43",set_cookies(koek))
 

 