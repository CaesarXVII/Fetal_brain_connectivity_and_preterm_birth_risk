stud_perm_test = function(obs,id_group,alternative,alpha,seed, n_sim, surplus){
  
  T_lobs <- list()
    
  V_lobs <- list()
    
  S_lobs <- list()
  
  n <- length(obs)
  
  id_group <- factor(x = id_group,levels = c("0","1"))
  
  id_one <- which(id_group == "1")
  
  id_zero <- which(id_group == "0")
  
  n_one <- length(id_one)
  
  n_zero <- length(id_zero)
  
  P <- matrix(data = rep(0,surplus*n),nrow = n,ncol = surplus)
  
  for (i in 1:surplus) {
    
    P[,i] <- sample(x = 1:n,size = n,replace = F)
    
  }
  
  P_nodup <- P[, !duplicated(t(P))]
  
  P <- P_nodup[,1:n_sim]
  
  if(alternative == "less"){
    
    T_obs <- mean(obs[id_one]) - mean(obs[id_zero])
    
    V_obs <- sqrt((n/n_one)*var(obs[id_one]) + (n/n_zero)*var(obs[id_zero]))
    
    S_obs <- T_obs/V_obs
  
    for (k in 1:n_sim) {
      
      id_perm <- id_group[P[,k]] #permutation step
      
      T_lobs[[k]] <- mean(obs[which(id_perm == "1")]) - mean(obs[ which(id_perm == "0")])
      
      V_lobs[[k]] <- sqrt((n/n_one)*var(obs[which(id_perm == "1")]) + (n/n_zero)*var(obs[which(id_perm == "0")]))
      
      S_lobs[[k]] <- T_lobs[[k]]/V_lobs[[k]]
      
    }
    
  
  } else {
    
    
    stop("Sorry, other directions are not coded yet")
    
  }
  
  S_perm <- unlist(S_lobs)
  
  perm_p_val <- (1 + length(which(S_perm <= S_obs)))/(1 + n_sim)
  
  stat_obs <- c(T_obs, V_obs, S_obs)
  
  names(stat_obs) <- c("T_obs", "V_obs", "S_obs")
  
  out = structure(list(stat_obs = stat_obs,
                       T_lobs = T_lobs,
                       V_lobs = V_lobs,
                       S_lobs = S_lobs,
                       p_val = perm_p_val
  ))
  
  invisible(out)
  
}