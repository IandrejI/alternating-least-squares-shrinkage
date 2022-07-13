# packages ----------------------------------------------------------------
source("code/models/ALS.R")


# 10-CV -------------------------------------------------------------------
## 10 CV mit verschiedenen Regul-Parametern

# tuning.grid2 <- expand.grid(impRate = c(0.001, 0.01), alpha = seq(0,1,0.2),
#                           lambda = c(seq(0,0.09,0.01),seq(0.1,0.9,0.1),seq(1,3,0.5)), dim = seq(2,20,2)
#                           ,RMSE.Valid = NA)
# 
# set.seed(38345)
# # 150 Zufällige Modelle
# sample.grid2 <- tuning.grid2[sample(1:nrow(tuning.grid2), 150),]
# 
# 
# start2 <- Sys.time()
# for(i in 1:nrow(sample.grid2)){
#     impRate <- sample.grid2$impRate[i]
#     alpha <- sample.grid2$alpha[i]
#     lambda <- sample.grid2$lambda[i]
#     dim <- sample.grid2$dim[i]
# 
# 
#     sample.grid2$RMSE.Valid[i] <- try({tune.ALS(trainMatrix, k = 10, dim = dim, seed = 8239,
#                                         impRate = impRate, alpha = alpha, iterMax = 30,
#                                         lambda = lambda, verbose = TRUE)}, silent = TRUE)
# 
#     write.csv(sample.grid2, file = "data/sampleGrid2.csv")
# 
#     print(paste0("grid-progress: ",i/nrow(sample.grid2)*100, "%"))
# }
# end2 <- Sys.time()
# 
# duration2 <- end2-start2



eval.grid <- sample.grid2


