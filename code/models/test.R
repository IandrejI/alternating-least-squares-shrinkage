library(CVXR)
ALS <- function(M, dimMax = 10, impRate = 0.001, dimIterMax = 3,
                maxSSE = 0.1){
  
  
  rownamesM <- rownames(M)
  colnamesM <- colnames(M)
  
  Q <- matrix(data = sample(1, dimMax * ncol(M), replace = T),
              nrow = dimMax, ncol = ncol(M))
  
  P <- matrix(data = NA, nrow = nrow(M), ncol = dimMax)
  
  residR <- data.frame(SSE = rep(NA, dimIterMax*dimMax),
                       Iteration = rep(NA, dimIterMax*dimMax),
                       dim = rep(NA, dimIterMax*dimMax))
  
  totalIter = 0
  dim = 1
  add_dim = TRUE
  
  while(add_dim){
    
    dimIter = 0
    cont_while = TRUE
    
    if(dim > 1){
      R <- M-Rhat} 
    else{
      R <- M}  
    
    while(cont_while){
      
      for(i in 1:nrow(R)){
        X <- Q[dim,!is.na(M[i,])]
        P[i,dim] <- solve(t(X)%*%X)%*%t(X)%*%R[i,!is.na(M[i,])]}
      for(i in 1:ncol(R)){
        X <- P[!is.na(M[,i]),dim]
        Q[dim,i] <- solve(t(X)%*%X)%*%t(X)%*%R[!is.na(M[,i]),i]}
      
      dimIter <- dimIter + 1
      totalIter <- totalIter + 1
      
      if(dim > 1){
        Rhat <- P[,1:dim]%*%Q[1:dim,]} 
      else{
        Rhat <- P[,1:dim]%*%t(Q[1:dim,])}
      
      
      residR$SSE[totalIter] <- sum((M - Rhat)^2, na.rm = TRUE)
      residR$Iteration[totalIter] <- dimIter
      residR$dim[totalIter] <- dim
      
      
      cont_while <-  (if(dimIter > 1){
        (residR$SSE[(totalIter - 1)] - residR$SSE[totalIter]) > impRate} 
        else{
          TRUE}) & dimIter < dimIterMax
      
    }
    
    if(dim < dimMax && residR$SSE[totalIter] > maxSSE){
      dim <- dim + 1}
    else{
      add_dim = FALSE}
    
  }
  
  rownames(Rhat) <- rownamesM
  colnames(Rhat) <- colnamesM
  colnames(Q) <- colnamesM
  rownames(P) <- rownamesM
  pick <- !is.na(residR$Iteration) & !is.na(residR$SSE) & !is.na(residR$dim)
  residR <- residR[pick,]
  
  results <- list(
    Rhat = Rhat,
    Q = Q[1:dim,],
    P = P[,1:dim],
    iterResults = residR[order(residR$SSE),])
  
  return(results)
  
}
ALS(trainMatrix)

M <- matrix(sample(1:5,100, T),ncol = 10, nrow = 10)
P <- matrix(rep(1,10), nrow = 10)

Q <- matrix(data = sample(1, 1 * ncol(M), replace = T),
            nrow = 1, ncol = ncol(M))


solve(t(Q)%*%Q)
X <- Q[1,]
solve(t(X)%*%X) %*% t(X)%*%M[1,]
