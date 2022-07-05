# packages ----------------------------------------------------------------
source("code/models/ALS.R")


# 10-CV -------------------------------------------------------------------
tuning.grid <- expand.grid(impRate = seq(0.0001,0.001,0.0001), alpha = seq(0,1,0.05),
                          lambda = seq(0,10,0.25), dim = seq(10,50,5), RMSE.Valid = NA)

set.seed(385)
sample.grid <- tuning.grid[sample(1:nrow(tuning.grid), 3),]

for(i in 2:nrow(sample.grid)){
    impRate <- sample.grid$impRate[i]
    alpha <- sample.grid$alpha[i]
    lambda <- sample.grid$lambda[i]
    dim <- sample.grid$dim[i]
  
  
    sample.grid$RMSE.Valid[i] <- try({tune.ALS(trainMatrix, k = 10, dim = dim, seed = 8239, 
                                        impRate = impRate, alpha = alpha,
                                        lambda = lambda, verbose = FALSE)}, silent = TRUE)
  
    write.csv(sample.grid, file = "data/sampleGrid.csv")

    print(paste0("grid-progress: ",i/nrow(sample.grid)*100, "%"))
}





