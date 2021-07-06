library(recommenderlab)  
library(readr)
library(dplyr)

#import data
ratings_matrix <- read_csv("R/Shiny/ratingsmatrix.csv")
BigMovie <- read_csv("R/Shiny/BigMovie.csv")

genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

#Popular movies Function 
popular <- function(ratings,n){
  ratings <- as.matrix(ratings)
  ratings <- as(ratings,"realRatingMatrix")
  test <- ratings[1,]
  
  modelpop <- Recommender(data=ratings, method ="POPULAR")
  recomm <- predict(object = modelpop, newdata=test, n=n)
  c <- as(recomm, "list")
  print(subset(BigMovie, movieId %in% as.list(as.data.frame(c)[,1]))[2:4])
}


#IBCF movies Function 
ibcf <- function(ratings,test, n){
  
  modelibcf <- Recommender(data=ratings, method ="IBCF")
  recomm <- predict(object = modelibcf, newdata=test, n=n)
  c <- as(recomm, "list")
  print(subset(BigMovie, movieId %in% as.list(as.data.frame(c)[,1]))[2:4])
}


#Main
Suggest <- function(){
  print(genre_list)
  m <- readline("Select Any Genre you like:")
  c <- which(genre_list==m)
  r <- subset(BigMovie, BigMovie[,c+4]==1)$movieId
  ratings <- ratings_matrix[as.character(r)]
  print("These are the top 5 popular movies of the genre you selected")
  popular(ratings,5)
  m <- readline("Select one of the movie you liked the most:")
  c <- subset(BigMovie, title %in% m)$movieId[1]
  m <- ratings_matrix[as.character(c)]
  m <- which(m==max(m,na.rm = T))[1]
  
  ratings <- ratings_matrix[as.character(r)]
  ratings <- as.matrix(ratings)
  ratings <- as(ratings,"realRatingMatrix")
  ratings <- binarize(ratings,minRating=3)
  test <- ratings[m,]
  
  n <- readline("Number of Recommendations:")
  ibcf(ratings,test,n)
}

Suggest_by_year <- function(){
  print("We have movies from the year 1919 to 2015")
  c <- readline("Select any one year : ")
  r <- subset(BigMovie, year==c)$movieId
  ratings <- ratings_matrix[as.character(r)]
  print("These are the top 5 popular movies of the year you selected")
  popular(ratings,5)
  m <- readline("Select one of the movie you liked the most:")
  c <- subset(BigMovie, title %in% m)$movieId[1]
  m <- ratings_matrix[as.character(c)]
  m <- which(m==max(m,na.rm = T))[1]
  
  ratings <- ratings_matrix[as.character(r)]
  ratings <- as.matrix(ratings)
  ratings <- as(ratings,"realRatingMatrix")
  ratings <- binarize(ratings,minRating=3)
  test <- ratings[m,]
  
  n <- readline("Number of Recommendations:")
  ibcf(ratings,test,n)
}
Suggest_by_year()
