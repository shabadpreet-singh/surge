##Score Function####
calc_score <- function(pref_M, pref_W, matching)
{
  M_Match <- matching[, 2]
  n <- dim(pref_M)[2]
  # Pref_M will be a n x n matrix, with column representing preferences of a man
  # and n representing the number of men
  # same for pref_W 
  # matching is n x 2 matrix of pairs of men and women calculated by various algorithms(like ttc, da or sd)
  # M_match is a column vector having women who is matched with men of that index
  # similarly W_Match has men matched to women
  
  W_Match <- order(M_Match)
  score <- 0
  
  for(i in 1:n)
  {
    score <- score + (n+1 - which(pref_M[ ,i] == M_Match[i]) ) #Adding Men score
    score <- score + (n+1 - which(pref_W[, i] == W_Match[i]) ) #Adding Women score
  }
  
  return(score)
}


