#########################
### Reproducible code ###
#########################

# In this script, you can find the codes needed to reproduce the results of the paper:
  
# "The Maternal-Fetal Neurodevelopmental Groundings of Preterm Birth Risk".

# The object "data_scaled", used below, is available upon request to dellarosa.pasquale@hsr.it

## LEARNING AND TESTING PHASE ##

library(CompareCausalNetworks)
library(igraph)
library(viridis)
library(pcalg)

source("Causal_mat.R")

source(file = "MEC_to_obs.R")

source(file = "stud_perm_test.R")

n_sim <- 20000 #permutations

seed <- 2356

alpha <- 0.05 #level of the permutation test

surplus <- n_sim + 100 #additional permutation to avoid (unlikely) repetitions

ptb_risk <- c(1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0 ,0 ,1, 0, 1, 1 ,1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0)

# Estimation step

out <- Causal_mat(data_scaled = data_scaled,alpha = alpha,seed = 1345,method = "ges")

obs <- MEC_to_obs(nets = out$nets) # MEC stays for Markov equivalence class

T_ac <- sapply(X = out$nets,FUN = function(x) sum(x)) #number of active connections in the whole network

# Global test on connectivity

stud_res_MEC <- stud_perm_test(obs = T_ac,id_group = ptb_risk,alternative = "less",alpha = 0.05,seed = seed,n_sim = n_sim,surplus = surplus)

## PREDICTIVE PHASE ##

## VALIDATION PHASE ##