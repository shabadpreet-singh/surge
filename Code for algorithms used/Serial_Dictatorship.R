###Serial Dictatorship####

# returns matrix of nx2 with n being number of men and second column being the women matched to each men according to pref_m(preferences of man)
SD <- function(pref_M)
{
  n <- dim(pref_M)[2] # number of men
  match <- numeric(n) # to store matching
  
  #we are taking the priority order to be {1, 2..., n} with 1 being highest and n being lowest priority men
  for(i in 1:n)
  {
    match[i] <-pref_M[!(pref_M[,i] %in% match) , i][1]   #assigns first unmatched women to i'th man acc to his pref
  }
  
  return(matrix(c(1:n, match), ncol = 2))
}