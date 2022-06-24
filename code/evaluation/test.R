set.seed(321489)
M <- matrix(sample(c(1:5, NA), 10000, T), nrow = 100)

ALSTest <- ALS(M, dim = 5, seed = 123786, iterMax = 999, lambda = 0.5)

ALS.Bias(M, dim = 4, seed = 123786, iterMax = 999, lambda = 0.5, verbose = FALSE)
ALS.noBias(M, dim = 5, seed = 123786, iterMax = 999)$Rhat
tune.ALS(M,dim = 1000, seed = 123786, iterMax = 999, bias = TRUE)