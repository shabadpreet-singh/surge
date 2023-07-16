###dfs(Depth First Search)####

dfs <- function(pointing, parent.node, current.node, visited)
{
  if(current.node == parent.node){
    return(visited)
  }
  if(current.node %in% visited){
    return(NULL)
  }else{
    
    visited <- c(visited, current.node)
    current.node <- pointing[current.node, 2]
    
    return(Recall(pointing, parent.node, current.node, visited))
  }
}

###Temp_Matching function####
temp_match <- function(endowments, new.pref.m, unmatched_W)
{
  len <- length(new.pref.m)
  pointing <- matrix(c(1:len, rep(0, len)), ncol = 2)
  
  for(i in 1:len)
  {
    bool <- c()
    for(j in 1:len){
      bool <- c(bool, ( new.pref.m[i]  %in% endowments[[j]]) )
    }
    pointing[i, 2] <- which(bool == TRUE)
  }
  
  chain <- rep(NA, len)
  global <- c()
  for(i in 1:len)
  {
    parent.node <- i
    visited <- i
    current.node <- pointing[i, 2]
    
    if(i %in% global){next}
    
    chain[i] <- list(dfs(pointing, parent.node, current.node, visited))
    global <- c(global, chain[[i]])
  }
  
  global <- global[order(global)]
  
  #find list of men successfully matched to their first pref using igraph and store it in `men`
  final.match <- matrix(c(global, new.pref.m[global]), ncol = 2)
  
  return(final.match)
}


###TTC####
TTC <- function(pref_M, pref_W)
{
  n <- dim(pref_M)[2] # number of men
  
  unmatched_M <- seq(1:n)
  unmatched_W <- seq(1:n)
  matched <- NULL
  
  while(length(unmatched_M) > 1)
  {
    new.pref.m <- c() # finding new preferences of unmatched men by removing matched women
    new.pref.w <- c() # new preferences for women
    
    
    for(i in unmatched_M){
      new.pref.m <- cbind(new.pref.m, pref_M[!(pref_M[, i] %in% matched[, 2]), i])
    }
    for(i in unmatched_W){
      new.pref.w <- cbind(new.pref.w, pref_W[!(pref_W[, i] %in% matched[, 1]), i])
    }
    
    endowments <-rep(NA, n)
    
    for(i in 1:n){
      endowments[i] <- list(unmatched_W[ which(new.pref.w[1, ] == i) ])
    }
    
    endowments <- endowments[unmatched_M]  # as endowments for already matched doesn't exists
    
    temp <- temp_match(endowments, new.pref.m[1, ], unmatched_W)
    matched <- rbind(matched, matrix(c(unmatched_M[temp[, 1]], temp[, 2]), ncol = 2))
    
    unmatched_M <- unmatched_M[!(unmatched_M %in% matched[, 1])]
    unmatched_W <- unmatched_W[!(unmatched_W %in% matched[, 2])]
  }
  
  # if only 1 unmatched men and women remains they will be matched together no need to run loop again which saves time
  if(length(unmatched_M == 1)){
    matched <- rbind(matched, c(unmatched_M, unmatched_W))
  }
  
  return(matched[order(matched[, 1]), ])
}
