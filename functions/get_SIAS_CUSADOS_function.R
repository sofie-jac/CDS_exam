get_SIAS_CUSADOS <- function(data){
  # create scores and set the classes right for further analysis
  scores <- data
  scores$age <- as.integer(scores$age)
  scores$gender <- as.factor(scores$gender)
  
  #change gender into factor with names
  #levels(data$gender)=c("Female","Male","Other")
  scores$gender<- revalue(scores$gender, c("1"="Female", "2"="Male", "3"="Other"))
  
  #CUSADOS
  scores <- scores %>%
    mutate(cusados = rowSums(.[4:15]))%>%
    drop_na()
  #Sias
  columnsToReverse <- c('sias_5', 'sias_9', 'sias_11')
  scores[ ,columnsToReverse] = 4 - scores[ ,columnsToReverse]
  scores <- scores %>% 
    mutate(sias = rowSums(.[17:36])) %>%
    drop_na()
  #remove un-needed variables
  scores <- select(scores, record_id, gender, age, sias, cusados)
  
  #calculate above threshold inidividuals
  scores$sias_above_clin <- ifelse(scores$sias > 42, 1, 0) #a score of 1 means diagnosis
  scores$cusados_above_clin <- ifelse(scores$cusados > 15, 1, 0) #a score of 1 means diagnosis
  scores$both_above_clin <- ifelse(scores$cusados_above_clin+scores$sias_above_clin == 2, 1, 0) #a score of 1 means diagnosis in both
  
  #Change names of variables
  scores <- rename(scores, c("age" = "Age", "gender" = "Gender", "sias" = "SIAS", "cusados" = "CUSADOS"))
  return(scores)
}

