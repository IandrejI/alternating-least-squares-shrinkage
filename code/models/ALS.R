ALS.noBias <- function(M, dim = 10, impRate = 0, iterMax = Inf,lambda = 0,alpha = 0,
                seed = as.numeric(Sys.time()), verbose = TRUE){
  
  glmnetInstalled <- require("glmnet", character.only = TRUE)
  if(!glmnetInstalled){
    userInput <- readline(prompt = "r-package glmnet required and not found! If you want to install glmnet type y:")
    if(userInput == "y"){
      install.packages("glmnet")
    }else{
      stop("r-package glmnet required! Install glmnet OR set alpha = NULL")
    }
  }
  
  R <- M
   
  solver <- function(y,X,lambda,alpha){
    solution <- glmnet::glmnet(x = X, y = y,lambda = lambda,alpha = alpha, intercept = FALSE)
    return(as.numeric(coef(solution))[-1])}

  ## number of rows and cols in M  
  ncol <- ncol(M)
  nrow <- nrow(M)
  
  ## save row and colnames from M
  rownamesM <- rownames(M)
  colnamesM <- colnames(M)
  
  ## init. Q-Matrix (ItemMatrix) with 1
  set.seed(seed)
  Q <- matrix(data = runif(dim*ncol), nrow = ncol, ncol = dim)
  
  ## init P-Matrix (UserMatrix) with NA 
  P <- matrix(data = NA, nrow = nrow, ncol = dim)
  
  
  ## data.frame to store iteration results
  residR <- data.frame(SSE = Inf, Iteration = 0)
  
  ## init toalIter with 0 
  totalIter <- 0
  
  ## init dimension iteration with 0
  
  cont_while <- TRUE
  
  ## iterate between P and Q as long as cont_while == TRUE
  while(cont_while){
    totalIter <- totalIter + 1
    
    
    residR <- rbind(residR,data.frame(SSE = NA, Iteration = NA))
    
    ## for every row in R 
    for(i in 1:nrow(R)){
      ## item with rating
      notNA <- !is.na(R[i,])
      ## set Q as X
      X <- Q[notNA,]
      ## set R as y
      y <- R[i,notNA]
      ## solve linear Problem
      solution <- solver(y = y,X = X,lambda = lambda, alpha = alpha)
      P[i,] <- solution
    }
    ## for every col in R 
    for(i in 1:ncol(R)){
      ## user with rating
      notNA <- !is.na(R[,i])
      ## set P as X
      X <- P[notNA,]
      ## set R as y
      y <- R[notNA,i]
      ## solve linear Problem
      solution <- solver(y = y,X = X,lambda = lambda, alpha = alpha)
      Q[i,] <- solution
    }
    ## count dimIter and totalIter up by one
    
    ## calculate Rhat as matrix mult. bewteen P and Q
    Rhat <- P%*%t(Q)
    
    ## 
    residR$SSE[totalIter+1] <- sum((M - Rhat)^2, na.rm = TRUE)
    residR$Iteration[totalIter+1] <- totalIter
    if(verbose){
      print(paste("iteration:",totalIter,"| SSE:",residR$SSE[totalIter+1]))
    }
    cont_while <- residR$SSE[(totalIter)] - residR$SSE[totalIter+1] > impRate && totalIter < iterMax
    
    
  }
  
  
  rownames(Rhat) <- rownamesM
  colnames(Rhat) <- colnamesM
  colnames(Q) <- colnamesM
  rownames(P) <- rownamesM
  
  
  results <- list(
    Rhat = Rhat,
    Q = Q,
    P = P,
    iterResults = residR[nrow(residR):1,])
  
  
  return(results)
  
}

ALS.Bias <- function(M, dim = 10, impRate = 0, iterMax = Inf,lambda = 0,alpha = 0,
                seed = as.numeric(Sys.time()), verbose = TRUE){
  
  glmnetInstalled <- require("glmnet", character.only = TRUE)
  
  if(!glmnetInstalled){
    userInput <- readline(prompt = "r-package glmnet required and not found! If you want to install glmnet type y:")
    if(userInput == "y"){
      install.packages("glmnet")
    }else{
      stop("r-package glmnet required! Install glmnet OR set alpha = NULL")
    }
  }
  
  solver <- function(y,X,lambda,alpha){
    solution <- glmnet::glmnet(x = X, y = y,lambda = lambda, alpha = alpha, intercept = TRUE)
    return(as.numeric(coef(solution)))}
  
  R <- M
  meanR <- mean(R,na.rm = TRUE)
  R <- R-meanR
  
  ## number of rows and cols in M  
  ncol <- ncol(M)
  nrow <- nrow(M)
  
  ## save row and colnames from M
  rownamesM <- rownames(M)
  colnamesM <- colnames(M)
  
  ## init. Q-Matrix (ItemMatrix) with 1
  set.seed(seed)
  Q <- matrix(data = runif(dim*ncol), nrow = ncol, ncol = dim)
  ## init item intercept
  i.I <- rep(0,nrow)
  
  ## init P-Matrix (UserMatrix) with NA 
  P <- matrix(data = NA, nrow = nrow, ncol = dim)
  ## init user bias
  u.B <- rep(0,ncol)
  
  
  ## data.frame to store iteration results
  residR <- data.frame(SSE = Inf, Iteration = 0)
  
  ## init toalIter with 0 
  totalIter <- 0
  
  ## init dimension iteration with 0
  
  cont_while <- TRUE
  
  ## iterate between P and Q as long as cont_while == TRUE
  while(cont_while){
    totalIter <- totalIter + 1
    
    residR <- rbind(residR,data.frame(SSE = NA, Iteration = NA))
    
    ## for every row in R 
    for(i in 1:nrow(R)){
      ## item with rating
      notNA <- !is.na(R[i,])
      ## set Q as X
      X <- Q[notNA,]
      ## set R as y
      y <- R[i,notNA] - i.I[notNA]
      ## solve linear Problem
      solution <- solver(y = y,X = X,lambda = lambda, alpha = alpha)
      u.B[i] <- solution[1]
      P[i,] <- solution[-1]
    }
    ## for every col in R 
    for(i in 1:ncol(R)){
      ## user with rating
      notNA <- !is.na(R[,i])
      ## set P as X
      X <- P[notNA,]
      ## set R as y
      y <- R[notNA,i] - u.B[notNA]
      ## solve linear Problem
      solution <- solver(y = y,X = X,lambda = lambda, alpha = alpha)
      i.I[i] <- solution[1]
      Q[i,] <- solution[-1]
    }
    
    ## calculate Rhat as matrix mult. bewteen P and Q
    i.I_mat <- matrix(i.I, nrow = nrow, ncol = ncol, byrow = TRUE)
    u.B_mat <- matrix(u.B, nrow = nrow, ncol = ncol)
    Rhat <- P%*%t(Q) + meanR + i.I_mat + u.B_mat
    
    ## 
    residR$SSE[totalIter+1] <- sum((M - Rhat)^2, na.rm = TRUE)
    residR$Iteration[totalIter+1] <- totalIter
    
    if(verbose){
      print(paste("iteration:",totalIter,"| SSE:",residR$SSE[totalIter+1]))
    }
    
    cont_while <- residR$SSE[(totalIter)] - residR$SSE[totalIter+1] > impRate && totalIter < iterMax
    
    
  }
  
  
  rownames(Rhat) <- rownamesM
  colnames(Rhat) <- colnamesM
  colnames(Q) <- colnamesM
  rownames(P) <- rownamesM
  
  
  results <- list(
    Rhat = Rhat,
    Q = Q,
    P = P,
    iterResults = residR[nrow(residR):1,])
  
  
  return(results)
  
}

ALS <-function(M, dim = 10, impRate = 0, iterMax = Inf,lambda = 0,alpha = 0,
               seed = as.numeric(Sys.time()), bias = FALSE, verbose = TRUE){
  if(bias == TRUE){
    ALS.Bias(M, dim, impRate, iterMax, lambda, alpha, seed, verbose)
  }else{
    ALS.noBias(M, dim, impRate, iterMax, lambda, alpha, seed, verbose)
  }
  
}
tune.ALS <- function(M, k = 10, dim = 10, impRate = 0, iterMax = Inf,lambda = 0,alpha = 0,
                     seed = as.numeric(Sys.time()), bias = FALSE, verbose = FALSE, status = TRUE, return.mean = TRUE){
  notNAs <- which(!is.na(M))
  values <- M[notNAs]
  names(notNAs) <- values
  
  set.seed(seed)
  notNAs <- sample(notNAs)
  
  holdOuts <- split(notNAs, cut(seq_along(notNAs), k, labels = FALSE))
  
  results <- data.frame(fold = 1:k, RMSE = NA, MAE = NA)
  
  for(i in 1:k){
    holdout <- holdOuts[[i]]
    realValues <- as.numeric(names(holdout))
    train.M <- M
    train.M[holdout] <- NA
    
    Rhat <-  ALS(M, dim , impRate, iterMax, lambda, alpha, seed, bias, verbose)$Rhat
    
    prediction <- Rhat[holdout]
    
    e <- realValues - prediction
    results$RMSE[i] <- sqrt(mean(e^2))
    results$MAE[i] <- mean(abs(e))
    if(status){
      print(paste0(strrep("-", 45),"   ",i/k*100,"%","   ",strrep("-", 45)))
      print(paste0("Errors on hold out set ",i,": ","RMSE = ",results$RMSE[i]," MAE = ",results$MAE[i]))
      print(strrep("-", 100))
    }
    
  }
  if(return.mean){
    return(apply(results[,-1], 2, mean))
  } else{
    return(results)
  }
}


