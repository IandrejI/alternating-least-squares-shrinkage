##renv::restore()
# packages ----------------------------------------------------------------
library(tidyverse)


# loadData ---------------------------------------------------------------
ratings <- read_delim("data/ml-1m/ratings.dat", delim = "::",
                      col_names = c("userId", "movieId", "rating", "timestamp"))
movies <- read_delim("data/ml-1m/movies.dat", delim = "::",
                     col_names = c("movieId", "title", "genres"))
users <- read_delim("data/ml-1m/users.dat", delim = "::",
                    col_names = c("userId", "gender", "age", "occupation", "zip"))

movies5 <- ratings %>% 
  group_by(movieId) %>% 
  summarise(n = n()) %>%
  filter(n > 5)


ratings <- ratings %>% 
  filter(movieId %in% movies5$movieId)


categories <- str_split(movies$genres,"\\|", simplify = TRUE)
uniqueCat <- unique(categories[,1])
for(i in 2:ncol(categories)){
  uniqueCat <- c(uniqueCat,unique(categories[,i]))
}
uniqueCat <- sort(unique(uniqueCat)[uniqueCat != ""])
uniqueCat[1] <- "noGenre"

df <- matrix(nrow = nrow(categories), ncol = length(uniqueCat))
colnames(df) <- uniqueCat
for(i in 1:length(uniqueCat)){
  for(j in 1:nrow(categories)){
    ifelse(sum(uniqueCat[i] == categories[j,]) > 0, df[j,i] <- 1,  df[j,i] <- 0)
  }
}

movies <- cbind(movies[,-3],df)



# createDataSet -----------------------------------------------------------
movielens <- ratings %>% 
  left_join(movies, by = "movieId")


movielensPivotLonger <- movielens %>% 
  pivot_longer(cols = 6:25, names_to = "genre") %>% 
  filter(value == 1)


ratingMatrix <-  movielens %>% 
  mutate(movie = paste0(title,":",movieId)) %>% 
  select(userId, movie, rating) %>%
  pivot_wider(names_from = movie, values_from = rating) %>% 
  column_to_rownames("userId")
ratingMatrix <- as.matrix(ratingMatrix)

# Test/Train --------------------------------------------------------------
set.seed(2187)
testPos <- sample(which(!is.na(ratingMatrix)), 0.2*(length(which(!is.na(ratingMatrix)))))  
testRating <- ratingMatrix[testPos]
trainMatrix <- ratingMatrix
trainMatrix[testPos] <- NA

head(sort(testPos), 10)


# Benchmark ---------------------------------------------------------------
set.seed(1236)
seeds <- sample(1:1000000000, 1500)
benchErrors <- expand.grid(n = 1:length(seeds), RMSE = NA, MAE = NA)

for(i in 1:length(seeds)){
  set.seed(seeds[i])
  benchRating <- sample(1:5, replace = TRUE, length(testRating))
  diffRatings <- testRating - benchRating
  benchErrors$RMSE[i] <- sum((diffRatings^2))*1/length(diffRatings)
  benchErrors$MAE[i] <- sum((abs(diffRatings)))*1/length(diffRatings)
}

benchErrors %>% 
  select(RMSE,MAE) %>% 
  summarise_all(.funs = mean)
