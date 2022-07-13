##renv::restore()
# packages ----------------------------------------------------------------
source("code/models/ALS.R")
# loadData ---------------------------------------------------------------
ratings <- read_csv("data/ratings.csv")
movies <- read_delim("data/movies.csv")

ratings %>% 
  group_by(movieId) %>% 
  summarise(n = n()) %>% 
  arrange(n)


movies20 <- ratings %>% 
  group_by(movieId) %>% 
  summarise(n = n()) %>%
  filter(n >= 15)


ratings <- ratings %>% 
  filter(movieId %in% movies20$movieId)

ratings %>% 
  group_by(userId) %>% 
  summarise(n = n()) %>% 
  arrange(n)


user20 <- ratings %>% 
  group_by(userId) %>% 
  summarise(n = n()) %>%
  filter(n >= 15)


ratings <- ratings %>% 
  filter(userId %in% user20$userId)

ratings %>% 
  group_by(movieId) %>% 
  summarise(n = n()) %>% 
  arrange(n)




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
str(ratingMatrix)
ratingMatrix <- as.matrix(ratingMatrix)
dim(ratingMatrix)

# Test/Train --------------------------------------------------------------
set.seed(2187)
test <- sample(which(!is.na(ratingMatrix)), 0.2*(length(which(!is.na(ratingMatrix)))))  
names(test) <- ratingMatrix[test]
trainMatrix <- ratingMatrix
trainMatrix[test] <- NA
test.ratings <- as.numeric(names(test))

