# packages ----------------------------------------------------------------
source("code/models/ALS.R")


# 10-CV -------------------------------------------------------------------
tuning.grid <- expand.grid(impRate = seq(0.001,0.003,0.001), alpha = seq(0,1,0.1),
                          lambda = c(seq(0.25,0.9,0.25),1:5), dim = seq(10,50,10), RMSE.Valid = NA)

set.seed(385)
sample.grid <- tuning.grid[sample(1:nrow(tuning.grid), 125),]


start <- Sys.time()
for(i in 1:nrow(sample.grid)){
    impRate <- sample.grid$impRate[i]
    alpha <- sample.grid$alpha[i]
    lambda <- sample.grid$lambda[i]
    dim <- sample.grid$dim[i]
  
  
    sample.grid$RMSE.Valid[i] <- try({tune.ALS(trainMatrix, k = 5, dim = dim, seed = 8239, 
                                        impRate = impRate, alpha = alpha, iterMax = 100,
                                        lambda = lambda, verbose = TRUE)}, silent = TRUE)
  
    write.csv(sample.grid, file = "data/sampleGrid.csv")

    print(paste0("grid-progress: ",i/nrow(sample.grid)*100, "%"))
}
end <- Sys.time()

duration <- end-start


