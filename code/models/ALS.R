# packages ----------------------------------------------------------------
library(tidyverse)
library(glmnet)
library(grid)
library(gridExtra)


# functions ---------------------------------------------------------------
## ALS Function ohne Bias
ALS.noBias <- function(M, dim = 10, impRate = 0, iterMax = Inf,lambda = 0,alpha = 0,
                seed = as.numeric(Sys.time()), holdout = NULL,verbose = TRUE){
  
  ## check if glmnet is installed and install if desired
  glmnetInstalled <- require("glmnet", character.only = TRUE)
  if(!glmnetInstalled){
    userInput <- readline(prompt = "r-package glmnet required and not found! If you want to install glmnet type y:")
    if(userInput == "y"){
      install.packages("glmnet")
    }else{
      stop("r-package glmnet required! Install glmnet OR set alpha = NULL")
    }
  }
  
  #check if holdout is specified
  holdout.check <- !is.null(holdout)
  
  # if holdout, delete values
  if(holdout.check){
    M[holdout] <- NA
    realValue <- as.numeric(names(holdout))
  }
  
  # set M to R
  R <- M
  
  # Number of values not Null 
  N <- sum(!is.na(R))
  
  # window size init
  window <- 10
  
  # Solver for ALS 
  solver <- function(y,X,lambda,alpha){
    ## Set intercept to false
    solution <- glmnet::glmnet(x = X, y = y,lambda = lambda,alpha = alpha, intercept = FALSE)
    return(as.numeric(coef(solution))[-1])}

  ## number of rows and cols in M  
  ncol <- ncol(M)
  nrow <- nrow(M)
  
  ## save row and colnames from M
  rownamesM <- rownames(M)
  colnamesM <- colnames(M)
  
  ## init. Q-Matrix (ItemMatrix) with random Number between 0 and 1
  set.seed(seed)
  Q <- matrix(data = runif(dim*ncol), nrow = ncol, ncol = dim)
  
  ## init P-Matrix (UserMatrix) with NA 
  P <- matrix(data = NA, nrow = nrow, ncol = dim)
  
  
  ## data.frame to store iteration results
  residR <- data.frame(SSE.Train = Inf,RMSE.Train = Inf, Iteration = 0)
  if(holdout.check){
    residR <- data.frame(SSE.Train = Inf,SSE.Valid = NA,RMSE.Train = Inf,RMSE.Valid = NA, Iteration = 0)
  }
  
  
  ## init toalIter with 0 
  totalIter <- 0
  
  ## init dimension iteration with 0
  
  cont_while <- TRUE
  
  ## iterate between P and Q as long as cont_while == TRUE
  while(cont_while){
    totalIter <- totalIter + 1
    
    
    
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
    
    
    ## calculate Rhat as matrix mult. bewteen P and Q
    Rhat <- P%*%t(Q)
    
    ## calculate errors
    SSE.Train <- sum((R - Rhat)^2, na.rm = TRUE)
    RMSE.Train <- sqrt(SSE.Train/N)
    add <- data.frame(SSE.Train = SSE.Train,RMSE.Train = RMSE.Train, Iteration = totalIter)
    RMSE.max <- RMSE.Train
    RMSE.min <- RMSE.Train
    
    if(holdout.check){
      # prediction = Rhat at holdout-position 
      prediction <- Rhat[holdout]
      # calc. error
      e <- realValue - prediction
      SSE.Valid <- sum(e^2)
      RMSE.Valid <- sqrt(SSE.Valid/length(e))
      add <- data.frame(SSE.Train = SSE.Train,SSE.Valid = SSE.Valid,RMSE.Train = RMSE.Train,RMSE.Valid = RMSE.Valid, Iteration = totalIter)
      RMSE.max <- max(c(RMSE.Train, RMSE.Valid))
      RMSE.min <- min(c(RMSE.Train, RMSE.Valid))
      }
    residR <- rbind(residR, add)
    
    # if verbose is desired 
    if(verbose){
      ## print Iteration-Result in console 
      print(paste("iteration:",totalIter,"| SSE:",residR$SSE.Train[totalIter+1]))
      flush.console()
      ## plot iteration result for train
      plot(residR$Iteration[-1],residR$RMSE.Train[-1],type='l',lwd=2.0,col = "#fb6a4a",
           xlim=c(0,window+totalIter), ylim = c(RMSE.min*0.95,RMSE.max*1.2),
           xlab = "Iteration", ylab = "RMSE")
      title("Error")
      legend(x = "topright",legend=c("Train Error", "Validation Error"), 
             fill = c("#fb6a4a","#74c476"))
      ## if holdout, plot iteration result for test
      if(holdout.check){
        lines(residR$Iteration[-1], residR$RMSE.Valid[-1], type = 'l',lwd=2.0, col = "#74c476")
      }
    }
    ## continue it total iter == 1 or if improvment rate > impRate or numer of total itertions < iter Max
    cont_while <- totalIter < 2 || (residR$SSE.Train[(totalIter)] - residR$SSE.Train[totalIter+1])/residR$SSE.Train[(totalIter)] > impRate && totalIter < iterMax
    
  }
  
  # set names of RHat, P and Q
  rownames(Rhat) <- rownamesM
  colnames(Rhat) <- colnamesM
  rownames(Q) <- colnamesM
  rownames(P) <- rownamesM
  
  # bind as list
  results <- list(
    Rhat = Rhat,
    Q = Q,
    P = P,
    iterResults = residR[nrow(residR):1,])
  
  # return list
  return(results)
  
}

## ALS Function mit Bias
ALS.Bias <- function(M, dim = 10, impRate = 0, iterMax = Inf,lambda = 0,alpha = 0,
                seed = as.numeric(Sys.time()), holdout = NULL,verbose = TRUE){
  
  ## check if glmnet is installed and install if desired
  glmnetInstalled <- require("glmnet", character.only = TRUE)
  if(!glmnetInstalled){
    userInput <- readline(prompt = "r-package glmnet required and not found! If you want to install glmnet type y:")
    if(userInput == "y"){
      install.packages("glmnet")
    }else{
      stop("r-package glmnet required! Install glmnet OR set alpha = NULL")
    }
  }
  
  ## Solver for ALS 
  solver <- function(y,X,lambda,alpha){
    ## set intercept as true 
    solution <- glmnet::glmnet(x = X, y = y,lambda = lambda, alpha = alpha,
                               intercept = TRUE, standardize.response = FALSE)
    return(as.numeric(coef(solution)))}
  
  #check if holdout is specified
  holdout.check <- !is.null(holdout)
  
  # if holdout, delete values
  if(holdout.check){
    M[holdout] <- NA
    realValue <- as.numeric(names(holdout))
  }
  
  ## set M to R
  R <- M
  
  ## calculate mean of R
  meanR <- mean(R,na.rm = TRUE)
  ## substract mean
  R <- R-meanR
  
  ##  number of given values
  N <- sum(!is.na(R))
  
  ## window size init
  window <- 10
  
  ## number of rows and cols in M  
  ncol <- ncol(M)
  nrow <- nrow(M)
  
  ## save row and colnames from M
  rownamesM <- rownames(M)
  colnamesM <- colnames(M)
  
  ## init. Q-Matrix (ItemMatrix) with random Number between 0 and 1
  set.seed(seed)
  Q <- matrix(data = runif(dim*ncol), nrow = ncol, ncol = dim)
  ## init item intercept
  i.I <- runif(nrow)
  
  ## init P-Matrix (UserMatrix) with NA 
  P <- matrix(data = NA, nrow = nrow, ncol = dim)
  ## init user bias
  u.B <- runif(ncol)
  
  
  ## data.frame to store iteration results
  residR <- data.frame(SSE.Train = Inf,RMSE.Train = Inf, Iteration = 0)
  if(holdout.check){
    residR <- data.frame(SSE.Train = Inf,SSE.Valid = NA,RMSE.Train = Inf,RMSE.Valid = NA, Iteration = 0)
  }
  
  ## init toalIter with 0 
  totalIter <- 0
  
  
  cont_while <- TRUE
  
  ## iterate between P and Q as long as cont_while == TRUE
  while(cont_while){
    totalIter <- totalIter + 1
    
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
      ## update user bias with intercept 
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
      ## update item intercept with intercept
      i.I[i] <- solution[1]
      Q[i,] <- solution[-1]
    }
    
    ## calculate Rhat as matrix mult. bewteen P and Q and add biases
    i.I_mat <- matrix(i.I, nrow = nrow, ncol = ncol, byrow = TRUE)
    u.B_mat <- matrix(u.B, nrow = nrow, ncol = ncol)
    Rhat <- P%*%t(Q) + meanR + i.I_mat + u.B_mat
    
    ## calculate errors
    SSE.Train <- sum((R - Rhat)^2, na.rm = TRUE)
    RMSE.Train <- sqrt(SSE.Train/N)
    add <- data.frame(SSE.Train = SSE.Train,RMSE.Train = RMSE.Train, Iteration = totalIter)
    RMSE.max <- RMSE.Train
    RMSE.min <- RMSE.Train
    
    if(holdout.check){
      # prediction = Rhat at holdout-position 
      prediction <- Rhat[holdout]
      # calc. error
      e <- realValue - prediction
      SSE.Valid <- sum(e^2)
      RMSE.Valid <- sqrt(SSE.Valid/length(e))
      add <- data.frame(SSE.Train = SSE.Train,SSE.Valid = SSE.Valid,RMSE.Train = RMSE.Train,RMSE.Valid = RMSE.Valid, Iteration = totalIter)
      RMSE.max <- max(c(RMSE.Train, RMSE.Valid))
      RMSE.min <- min(c(RMSE.Train, RMSE.Valid))
    }
    residR <- rbind(residR, add)
    
    # if verbose is desired 
    if(verbose){
      ## print Iteration-Result in console 
      print(paste("iteration:",totalIter,"| SSE:",residR$SSE.Train[totalIter+1]))
      flush.console()
      ## plot iteration result for train
      plot(residR$Iteration[-1],residR$RMSE.Train[-1],type='l',lwd=2.0,col = "#fb6a4a",
           xlim=c(0,window+totalIter), ylim = c(RMSE.min*0.95,RMSE.max*1.2),
           xlab = "Iteration", ylab = "RMSE")
      title("Error")
      legend(x = "topright",legend=c("Train Error", "Validation Error"), 
             fill = c("#fb6a4a","#74c476"))
      ## if holdout, plot iteration result for test
      if(holdout.check){
        lines(residR$Iteration[-1], residR$RMSE.Valid[-1], type = 'l',lwd=2.0, col = "#74c476")
      }
    }
    ## continue it total iter == 1 or if improvment rate > impRate or numer of total itertions < iter Max
    cont_while <- totalIter < 2 || (residR$SSE.Train[(totalIter)] - residR$SSE.Train[totalIter+1])/residR$SSE.Train[(totalIter)] > impRate && totalIter < iterMax
    
  }
  
  # set names of RHat, P and Q
  rownames(Rhat) <- rownamesM
  colnames(Rhat) <- colnamesM
  rownames(Q) <- colnamesM
  rownames(P) <- rownamesM
  
  # bind as list
  results <- list(
    Rhat = Rhat,
    Q = Q,
    P = P,
    iterResults = residR[nrow(residR):1,])
  
  # return list
  return(results)
  
}

## Wrapper Function for ALS with bias or no bias
ALS <-function(M, dim = 10, impRate = 0, iterMax = Inf,lambda = 0,alpha = 0,
               seed = as.numeric(Sys.time()), bias = FALSE, holdout = NULL,verbose = TRUE){
  if(bias == TRUE){
    ALS.Bias(M, dim, impRate, iterMax, lambda, alpha, seed, holdout, verbose)
  }else{
    ALS.noBias(M, dim, impRate, iterMax, lambda, alpha, seed, holdout,verbose)
  }
}


## Function für k-fold CV
tune.ALS <- function(M, k = 10, dim = 10, impRate = 0, iterMax = Inf,lambda = 0,alpha = 0,
                     seed = as.numeric(Sys.time()), bias = FALSE,verbose = TRUE, status = TRUE, return.mean = TRUE){
  # which values are given
  notNAs <- which(!is.na(M))
  # set values as vector
  values <- M[notNAs]
  # set the values as names of notNas 
  names(notNAs) <- values
  
  # set seed
  set.seed(seed)
  # shuffle notNAs
  notNAs <- sample(notNAs)
  # split notNas in k equal-sized vectors
  holdOuts <- split(notNAs, cut(seq_along(notNAs), k, labels = FALSE))
  
  # init result data-frame
  results <- data.frame(fold = 1:k, RMSE = NA)
  
  # reapet k times
  for(i in 1:k){
    holdout <- holdOuts[[i]]
    
    ## call ALS function with specified parameters and k-th holdout
    solution <-  ALS(M, dim , impRate, iterMax, lambda, alpha, seed, bias, holdout, verbose)$iterResults
    ## save final prediction on holdoutset
    results$RMSE[i] <- solution$RMSE.Valid[1]
    ## if desired print status of cross validation  
    if(status){
      print(paste0(strrep("-", 45),"   ",i/k*100,"%","   ",strrep("-", 45)))
      print(paste0("Error on holdout ",i,": ","RMSE = ",results$RMSE[i]))
      print(strrep("-", 100))
    }
  }
  # if only the mean of the RMSE is desired return mean
  # else return results
  if(return.mean){
    return(c("Validation RMSE" = mean(results$RMSE)))
  } else{
    return(results)
  }
}

