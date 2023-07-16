setwd("C:/SURGE_CODE") #Set working Directory to current folder

source("Score_Function.R")  # loads `calc_score` function
source("Serial_Dictatorship.R")  # loads `SD` function
source("Deffered_Acceptance.R") # loads `DA` function
# Example
# men <- matrix(c(1, 2, 3,
#                 2, 3, 1,
#                 1, 3, 2), nrow = 3)
# women <- matrix(c(2, 1, 3,
#                   3, 2, 1,
#                   3, 1, 2), nrow = 3)
# DA(men, women) # returns n pairs with first element as men and the other the women matched to him
# 
source("TTC.R")  # loads `TTC` function
# Example
# men <- matrix(c(1, 2, 3,
#                 2, 1, 3,
#                 3, 2, 1), ncol = 3)
# 
# women <- matrix(c(1, 2, 3,
#                   3, 1, 2,
#                   2, 3, 1), ncol = 3)
# TTC(men, women)

#CALCULATING SCORES FOR 'n' MEN AND WOMEN CASE

#Random Sampling cases for n >= 4 (where 'n' is number of people)#####
n <- 10

n <- as.integer(readline(prompt = "Enter total number of men and women : "))

 library(pracma)
 library(gtools)
 way <- perms(seq(1 : (n/2) ))

 score_ttc <- 0
 score_da <- 0
 score_sd <- 0


 for(i in 1:1e6){

   if(i %% 1e4 == 0){print(i / 1e4)}

   prefs <-  sample(1:fact(n/2), n, replace = TRUE)

   pref_men   <- t(way[prefs[1:(n/2)], ])
   pref_women <- t(way[prefs[1:(n/2)], ])

   score_sd  <- score_sd  + calc_score(pref_men, pref_women, SD(pref_men))
   score_da  <- score_da  + calc_score(pref_men, pref_women, DA(pref_men, pref_women))
   score_ttc <- score_ttc + calc_score(pref_men, pref_women, TTC(pref_men, pref_women))
 }

 score_da  <- log(score_da)  - log(1e6) #Taking log as to increase Numerical stability
 score_sd  <- log(score_sd)  - log(1e6)
 score_ttc <- log(score_ttc) - log(1e6)

# save(score_da, score_sd, score_ttc, file = "scores.RData")


#Part 2 finding pattern when da is greater than ttc####

# Uncomment the following lines if installing packages required
# install.packages("gtools")
# install.packages("pracma")

library(gtools) #gtools contain `permutation` and pracma contains `perms` function 
library(pracma)

way <- perms(1:3)
dat <- permutations(6, 6, 1:6, repeats.allowed = T)
obs <- array(0, dim = c(6^6, 6, 3))

obs[1:6^6, 1:6, 1:3] <- way[dat, ]
rm(dat, way) #removing space

ttc_better <- NULL
da_better  <- NULL
equal <- NULL

n <- dim(obs)[1]
for(k in 1:n){
  
  if(k%%1000 == 0){
    print(round(k * 1e2/46656, 2)) 
  }
  
   men <- t(obs[k, 1:3, ])
   women <- t(obs[k, 4:6, ])
   
   ttc <- calc_score(men, women, TTC(men, women))
   da  <- calc_score(men, women, DA(men, women))

   if(ttc > da){
     ttc_better <- c(ttc_better, k)
   }
   else{ 
     if(ttc == da){
       equal <- c(equal, k)
     }
     else{ 
       da_better <- c(da_better, k)}
   }
  }

save(ttc_better, da_better, equal, obs, file = "Better_index.RData") 

#############################################
load("Better_index.RData")

da_More <- numeric(length(da_better))
ttc_More <- numeric(length(ttc_better))

for(i in 1:length(da_better)){
  
  men   <- t(obs[da_better[i], 1:3, ])
  women <- t(obs[da_better[i], 4:6, ])
  
  da_More[i] <- calc_score(men, women, DA(men, women)) - calc_score(men, women, TTC(men, women))
}

for(i in 1:length(ttc_better)){
  
  men   <- t(obs[ttc_better[i], 1:3, ])
  women <- t(obs[ttc_better[i], 4:6, ])
  
  ttc_More[i] <- calc_score(men, women, TTC(men, women)) - calc_score(men, women, DA(men, women))
}
unique(ttc_More)
unique(da_More)

plot(da_More)
hist(da_More)

hist(ttc_More)
plot(ttc_More)

plot(da_More[order(da_More)], type = "l")


#The following code is experimental...
#####
# dat <- permutations(6, 6, 1:6, repeats.allowed = T)
# obs <- array(0, dim = c(6^6, 6, 3))
# 
# # rows contain different possibility
# # columns of obs are m1, m2, m3 and w1, w2, w3
# # the third dimension contain priority of one person
# obs[1:6^6, 1:6, 1:3] <- way[dat, ]
# # data collected
# rm(dat, way) #removing space
# 
# score <- 0 # To store score at each iteration
# 
# for(k in 1:dim(obs)[1]){
#   men <- t(obs[k, 1:3, ])
#   women <- t(obs[k, 4:6, ])
# 
#   score <- score + calc_score(men, women, TTC(men, women))
# }
#####

# score <- function(peeps, vec, way){
#   
#   if(peeps == 0){
#     
#     men <- t(way[ vec[1:4], ])
#     women <- t(way[ vec[5:8], ])
#     
#     return(calc_score(men, women, TTC(men, women)) )
#   }
#   
#   total <- 0
#   for(i in 1:24){
#     if(peeps == 8){print(i)}
#     vec[9-peeps] <- i
#     
#     total <- total + Recall(peeps - 1, vec, way)
#   }
#   return(total)
# }
# 
# total <- 0
# vec <- numeric(8)
# score(8, vec, way)
