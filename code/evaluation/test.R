source("code/models/ALS.R")
seed <- set.seed(321489)

M <- matrix(sample(c(1:5, NA), 10000, T), nrow = 100)

notNAs <- which(!is.na(M))
values <- M[notNAs]
names(notNAs) <- values

set.seed(seed)
notNAs <- sample(notNAs)
k = 10
holdOuts <- split(notNAs, cut(seq_along(notNAs), k, labels = FALSE))
holdout <- holdOuts[[1]]



# M -----------------------------------------------------------------------


ALSTest <- ALS(M, dim = 10, seed = 123786, iterMax = 2, holdout = holdout, bias = TRUE)

ALS.Bias(M, dim = 10, seed = 123786, iterMax = 5, verbose = TRUE,
         holdout = holdout)

ALS.noBias(M, dim = 10, seed = 123786, iterMax = 5, holdout = holdout)


tune.ALS(M,10,dim = 10, seed = 123786, iterMax = 5, bias = TRUE)



# Movie Lens --------------------------------------------------------------

ALS.noBias(ratingMatrix, dim = 30, impRate = 0.0001, holdout = test, alpha = 1, lambda = 1)
ALS.Bias(ratingMatrix, dim = 30, iterMax = 10, holdout = test, alpha = 1, lambda = 1)

