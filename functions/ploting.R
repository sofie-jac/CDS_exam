#plot the data with thresholds
plot_SAD_scales <- function(scores){
  scores$Gender <- as.factor(scores$Gender)
  plot <- ggplot(data = scores)+
  geom_smooth(method = 'lm', aes(SIAS, CUSADOS)) +
  geom_point(aes(SIAS, CUSADOS, color = Age, pch = Gender)) +
  geom_vline(xintercept = 43, color = "red") + #sias threshold    
  geom_hline(yintercept = 16, color = "red") + #cusados threshold
  theme_minimal()
  return(plot)
}

