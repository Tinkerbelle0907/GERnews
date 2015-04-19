# You find here a function for giving an example for further use of the information retrieved with the get_faz function
# It depicts how frequent your topic has been mentioned (per month)
# Unfortunately the frequency cannot be depicted as per day because unless you do not create a Faz online account and pay for the articles there is no way to access the real days of the articles


see_freq<- function(faz_df, title) {
  
  df_months <- as.data.frame(table(faz_df$months))
  colnames(df_months) <- c("Months", "Frequency")
  
  gg <- ggplot(df_months, aes(Months,Frequency))
  y  <- scale_y_continuous(breaks = seq(1,10,1))
  t  <- ggtitle(title)
  
  plot <- (gg + geom_point() + t + y)
  
  return(plot)
}

# Here is how to use the function:

# 1. You build the FAZ data frame with the get_faz function

#ostern_df <- get_faz(topic="Ostern", from="01.01.2013", to="10.04.2015", area="TI", email="Your@email.com")

# 2. Then you think of a title and voilà you have a graph.
#see_freq(ostern_df, title="Easter in FAZ headlines")

# However, this is just an example of what you can do with the retrieved material from all three GERnews get-functions. For special use you can do whatever you want to do as it only is an ordinary data frame. 
# Example: as.data.frame(table(ostern_df$months)) also shows the frequency of 'Easter in FAZ headlines' in a table
