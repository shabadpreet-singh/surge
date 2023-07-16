###Deferred Acceptance####

DA <- function(pref_M, pref_W) {
  
  n <- dim(pref_M)[2] # number of men = women
  
  # Initialize everyone to be free i.e. unmatched
  unmarried_men <- seq(1, n)
  unmarried_women <- seq(1, n)
  engagements <- c()
  
  # While there are still unmarried men
  while (length(unmarried_men) > 0) {
    
    # Choose an unmarried man
    man <- unmarried_men[1]
    
    # Get the man's preference list
    prefs <- pref_M[, man]
    
    # Go through each of the women on the man's preference list
    for (i in 1:n) {
      woman <- prefs[i]
      
      # Get the woman's preference list
      woman_prefs <- pref_W[, woman]
      
      # If the woman is unmarried, they become engaged
      if (woman %in% unmarried_women) {
        
        engagements <- rbind(engagements, c(man, woman))
        unmarried_men <- unmarried_men[-1]
        unmarried_women <- unmarried_women[unmarried_women != woman]
        break
      }
      
      # Otherwise, check if the woman prefers this man over her current partner
      else {
        current_man <- engagements[which(engagements[, 2] == woman), 1]
        
        if (which(woman_prefs == man) < which(woman_prefs == current_man)) {
          engagements <- engagements[-which(engagements[, 2] == woman), ]
          engagements <- rbind(engagements, c(man, woman))
          unmarried_men <- c(unmarried_men[-1], current_man)
          break
        }
      }
    }
  }
  
  return(engagements[order(engagements[, 1]), ])
}