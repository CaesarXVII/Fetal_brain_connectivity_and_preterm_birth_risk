MEC_to_obs = function(nets){
  
  MEC_to_und <- function(A){
    
    n <- dim(A)[1]
    
    if (n != dim(A)[2]){
      
      stop("Please provide a valid adjecency matrix")
      
    }
    
    for (i in 1:n) {
  
      for (j in 1:n) {
        
      if(A[j,i]&A[i,j] == "1"){
        
        A[i,j] <- 0
        
      }  
        
      }
          
    }
    
    return(A)
    
  }
  
  T_ac <- sapply(X = nets,FUN = function(x) sum(MEC_to_und(A = x))) #number of active connections in the whole network

  out = structure(list(T_ac = c(T_ac)
  ))
  
  invisible(out)
}
