#Libraries:
library(readr)
library(data.table)


#Import dataset
movies <- read_csv("R/Shiny/movies.csv")
movies <- movies[,1:4]
ratings <- read_csv("R/Shiny/ratings.csv")


#Differentiating Genre
genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres2) <- c(1:10)

genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western") # we have 18 genres in total

genre_matrix <- matrix(0,10330,18) #empty matrix, 10330=no of movies+1, 18=no of genres
genre_matrix[1,] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list

for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}

genre_matrix <- genre_matrix[-1,]
genre_matrix <- as.data.frame(genre_matrix)
genre_matrix$movieId <- movies$movieId

BigMovie <- merge(movies, genre_matrix, by="movieId")
BigMovie[,1] <- as.numeric(BigMovie[,1])
BigMovie[,3] <- as.numeric(BigMovie[,3])
for(i in 5:ncol(BigMovie)){
  BigMovie[,i] <- as.numeric(BigMovie[,i])
}


#Rating Matrix
binaryratings <- ratings
binaryratings <- dcast(binaryratings, userId~movieId, value.var = "rating", na.rm=FALSE)
binaryratings <- binaryratings[,-1]
colnames(binaryratings) <- as.character(unique(ratings$movieId))

#Remving uncommon movies
`%!in%` <- purrr::compose(`!`, `%in%`)
r <- colnames(ratings_matrix)
m <- BigMovie$movieId
c <- m %!in% r
c <- which(c==T)
BigMovie <- BigMovie[-c,]

write.csv(BigMovie,"BigMovie.csv", row.names = F)
write.csv(binaryratings,"ratingsmatrix.csv", row.names = F)
