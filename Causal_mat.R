Causal_mat = function(data_scaled,alpha, seed, method){
  
  nets = list()
  
  n <- length(data_scaled)
  
  n_con <- dim(data_scaled[[1]])[2]
  
  Cmat <- matrix(data = rep(0,n*(n_con)^2),nrow = n,ncol = (n_con)^2)
  
  for(i in 1:n){
    #X = data[data$Exp==i,]
    X <- data_scaled[[i]]
    #Ahat <- getParents(X[,-1], method=method, alpha=0.05)
    
    set.seed(seed) #for reproducibility
    
    Ahat <- CompareCausalNetworks::getParents(X, method=method, alpha=alpha)
    
    #sum(Ahat)
    #rownames(Ahat) = colnames(Ahat)  = names(X)[-1]
    
    rownames(Ahat) <- colnames(Ahat)  <- colnames(X)
    
    nets[[i]] <- Ahat
    
    Cmat[i,] <- c(t(Ahat)) #row vectorization to preserve causal direction
    
  }
  
  #n_con <- dim(Ahat)[1]
  
  name_roi <- colnames(X)
  
  Cmat_names <- matrix(data=rep(0,n_con^2),nrow = n_con,ncol = n_con) 
  
  for (j in 1:n_con) {
    
    for (i in 1:n_con) {
      
      
     Cmat_names[j,i] <- paste0("",name_roi[j],"_",name_roi[i],"")
      
    }
    
  }
  
  colnames(Cmat) <- c(t(Cmat_names))
  
  out = structure(list(nets = nets,
                       Cmat = Cmat,
                       edge_names=colnames(Cmat)
  ))
  
  invisible(out)
}
