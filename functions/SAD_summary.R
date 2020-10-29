SAD_summary <- function(scores) {
  sum(scores$sias_above_clin)
  sum(scores$cusados_above_clin)
  sum(scores$both_above_clin)
  
  #percentage above clinical thrshold
  (sum(scores$sias_above_clin)/nrow(scores))*100
  (sum(scores$cusados_above_clin)/nrow(scores))*100
  (sum(scores$both_above_clin)/nrow(scores))*100
  
  SIAS_above_clin <- list(sum(scores$sias_above_clin), (sum(scores$sias_above_clin)/nrow(scores))*100)
  CUDASOS_above_clin <- list(sum(scores$cusados_above_clin), (sum(scores$cusados_above_clin)/nrow(scores))*100)
  both_above_clin <- list(sum(scores$both_above_clin), (sum(scores$both_above_clin)/nrow(scores))*100)
  
  SAD_summary <- as.data.frame(c(SIAS_above_clin, CUDASOS_above_clin, both_above_clin))
  SAD_summary <- rename(SAD_summary, c("X30"="SIAS_n", "X14.9253731343284"="SIAS_%", "X32"="CUSADOS_n", "X15.9203980099502"="CUSADOS_%", "X13"="both_n", "X6.46766169154229"="both_%"))
  return(SAD_summary)
  }
