install.packages("recommenderlab")
library(recommenderlab)
install.packages("ggplot2")
install.packages("reshape2")
install.packages("data.table")
library(recommenderlab)
library(data.table)
library(reshape2)
library(ggplot2)
Movies_data = read.csv(file = "movies.csv", header = T)
Ratings_data = read.csv(file = "ratings.csv", header = T)
str(Movies_data)
str(Ratings_data)
head(Movies_data, 7)
head(Ratings_data, 7)
summary(Movies_data)
summary(Ratings_data)

#Making Genres as matrix
?data.frame
library(data.table)
Movie_genre = data.frame(Movies_data$genres)
head(Movie_genre, 7)
Movie_genre2 = data.frame(tstrsplit(Movie_genre[,1],'[|]'))
Movie_genre2
head(Movie_genre2,5)
colnames(Movie_genre2) = c(1:10)
colnames(Movie_genre2)
Genre_types = c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
Genre_types
Genre_matrix = matrix(0,10330,18)
Genre_matrix[1,] = Genre_types
Genre_matrix
nrow(Movie_genre2)
ncol(Movie_genre2)
for (index in 1:nrow(Movie_genre2)){
  for (col in 1:ncol(Movie_genre2)){
    genre_col = which(Genre_matrix[1,] == Movie_genre2[index,col])
    Genre_matrix[index+1,genre_col] = 1
  }
}

#removing 1st row as it is a list of genres
Genre_matrix_update = data.frame(Genre_matrix[-1,])
Genre_matrix_update
str(Genre_matrix_update)
#converting characters to integers
for (col in 1 : ncol(Genre_matrix_update)) {
  Genre_matrix_update[,col] = as.integer(Genre_matrix_update[,col])
}
Genre_matrix_update
colnames(Genre_matrix_update)= Genre_types
Genre_matrix_update
str(Genre_matrix_update)

#searching matrix
Search_matrix = cbind(Movies_data[,1:2,], Genre_matrix_update)
head(Search_matrix, 8)

#regarding rating matrix
Rating_matrix = dcast(Ratings_data, userId~movieId , value.var = "rating", na.rm = FALSE)
rating_matrix = as.matrix(Rating_matrix[,-1])
#now convert into recommender lab sparse matrix
rating_matrix = as(rating_matrix,"realRatingMatrix")
rating_matrix

#the terminology of recommender system using
recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

#using recommenderlabs
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)

#using lapply function to know what are the parameters of recommender lab
lapply(recommendation_model, "[[", "description")

#similarity matrix to quantify the similarities between users
Similarity_matrix = similarity(rating_matrix[1:4,], method = "cosine", which ="users")
str(Similarity_matrix)
Sim_matrix = as.matrix(Similarity_matrix)
str(Similarity_matrix)
image(Sim_matrix, main = "Similarity of users")

str(rating_matrix)
rating_matrix

#similarity matrix for the items
Similarity_movies <- similarity(rating_matrix[, 1:4], method =
                                 "cosine", which = "items")
Sim_for_movies = as.matrix(Similarity_movies)
Sim_for_movies
image(Sim_for_movies, main = "Similarity between movies")

#for the unique ratings
Values_rating = as.vector(rating_matrix@data)
str(Values_rating)
unique(Values_rating)

#creating a table for these unique ratings to create a count 
Table_of_ratings = table(Values_rating)
Table_of_ratings

#now we try to estimate no,of views of movies
Movie_views = colCounts(rating_matrix)
Movie_views
Table_views = data.frame(movie = names(Movie_views), views = Movie_views) 
str(Table_views)
Table_views = Table_views[order(Table_views$views ,decreasing = TRUE) , ]
Table_views
Table_views$title = c(1:10325)

#matching no.of views to the respective movie 
for (index in 1:10325){
  Table_views[index,3] <- as.character(subset(Movies_data,
                                    Movies_data$movieId == Table_views[index,1])$title)
}
    
str(Table_views)

#plotting graphs for top films
ggplot(Table_views[1:7,], aes(x=title , y = views)) +
  geom_bar(stat = "identity", fill = "yellow") +
ggtitle(" List of top viewed films") + geom_text(aes (label = views), size = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#heatmap 
image(rating_matrix[1:20, 1:25], axes = FALSE, 
      main = "Heatmap of the 1st 20 rows of rating matrix")

#filtering the data to get useful data
movie_ratings = rating_matrix[rowCounts(rating_matrix)>50, colCounts(rating_matrix)>50]
movie_ratings

#heatmap for the users i.e the relevant data above
minimum_users = quantile(rowCounts(movie_ratings), 0.98)
str(minimum_users)
summary(minimum_users)
minimum_movies =  quantile(colCounts(movie_ratings), 0.98)
str(minimum_movies)
summary(minimum_movies)
image(movie_ratings[rowCounts(movie_ratings)>minimum_users,
                    colCounts(movie_ratings)>minimum_movies ], 
      main = "Heatmap of relevant users that is top users and top movies")

#now we deal with average ratings per user i.e we take mean of all the rows existing
average_ratings = rowMeans(movie_ratings)
qplot(average_ratings, fill = I("green"), col = I("red")) + 
  ggtitle("Distribution of average rating per user")


#now we normalize the ratings to bring them to a uniform value
normalized_data = normalize(movie_ratings)
str(normalized_data)
summary(normalized_data)
sum(rowMeans(normalized_data)>0.00001)

#heatmap of that normalization
image(normalized_data[rowCounts(normalized_data)>minimum_users,
                    colCounts(normalized_data)>minimum_movies ], 
      main = "Heatmap of normalized ratings")

#binarizing the data
binarized_minimum_users = quantile(rowCounts(movie_ratings), 0.95)
binarized_minimum_movies = quantile(colCounts(movie_ratings), 0.95)
#taking good ratings as >= 2.5
good_rating_as = binarize(movie_ratings, minRating = 2.5)
#getting heat map 
image(good_rating_as[rowCounts(movie_ratings) > binarized_minimum_users, 
                     colCounts(movie_ratings) > binarized_minimum_movies], 
      main = "Heat map of binarized data of ratings i.e top users and movies")
#preparing the model
#Item Based Collaborative Filtering System

sampling_data = sample(x = c(TRUE, FALSE), size = nrow(movie_ratings),
                       prob = c(0.7, 0.3), replace = TRUE)
training_data = movie_ratings[sampling_data, ]
training_data
testing_data = movie_ratings[!sampling_data, ]
testing_data

#the terminology of recommender system using
recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

#applying the model, actual application
recommending_model = Recommender(data =training_data, method ='IBCF', 
                                 parameter = list(k=30) )
str(recommending_model)
recommending_model
class(recommending_model)
