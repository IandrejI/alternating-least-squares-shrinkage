# packages ----------------------------------------------------------------
library(tidyverse)
library(rsample)

# loadData ---------------------------------------------------------------
genomeScores <- read_csv("data/genome-scores.csv")
genomeTags <- read_csv("data/genome-tags.csv")
links <- read_csv("data/links.csv")
movies <- read_csv("data/movies.csv")
ratings <- read_csv("data/ratings.csv")
tags <- read_csv("data/tags.csv")


categories <- str_split(movies$genres,"\\|", simplify = TRUE)
uniqueCat <- sort(unique(categories[,1]))

sum(uniqueCat[3] == categories[1,])


df <- matrix(nrow = nrow(categories), ncol = length(uniqueCat))
colnames(df) <- uniqueCat
for(i in 1:length(uniqueCat)){
  for(j in 1:nrow(categories)){
    ifelse(sum(uniqueCat[i] == categories[j,]) > 0, df[j,i] <- 1,  df[j,i] <- 0)
  }
}
movies <- cbind(movies[,-3],df)
colnames(movies)[3] <- "noGenre"


# createDataSet -----------------------------------------------------------
movielens <- ratings %>% 
  left_join(movies, by = "movieId")

movies[,-1] %>% 
  summarise_if(is.numeric, .funs = c(sum,mean))


