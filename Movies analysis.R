##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes
library(dplyr)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

oldw <- getOption("warn") #remove warnings for graphing plots (I put these back in after plotting the graphs)
options(warn = -1)

##########################################################
# Plot rating distributions
##########################################################

#Group films by rating, and calculate number of films with that rating (in thousands)
edx %>%
group_by(rating) %>%
summarise(number=n()/1000) %>%
ggplot(aes(x=rating, y=number))+
scale_y_continuous(breaks = c(seq(0, 28000, 500))) +
geom_bar(stat="identity") +
labs(x="Rating", y="Number of films (thousands)")
         
#Group ratings by user, and show distribution of users by number of ratings


edx %>%
  group_by(userId) %>%
  summarise(number_reviews=n()) %>%
  ggplot(aes(number_reviews))+
  geom_histogram(binwidth= 10)+
  scale_x_continuous(limits=c(10,250)) +
  labs(x="Number of ratings", y="Number of people giving that many ratings")


##########################################################
# Analyse different methods
##########################################################

options(warn= oldw ) #put warnings back in
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_i <- createDataPartition(y = edx$rating, times = 1, p = 0.3, list = FALSE)
train_set <- edx[-test_i,]
test_set <- edx[test_i,]
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

nrow(train_set) #print number of entries of the training set
nrow(test_set) #print number of entries of the test set

mu <- mean(train_set$rating) #find mean of training set ratings

#Predicted purely based on average rating of entire training set
predicted_ratings <- test_set %>% 
  mutate(pred = mu ) %>%
  pull(pred) #extract the predictions as a list

rmse_mu <- RMSE(predicted_ratings, test_set$rating) #test how good this prediction is
rmse_mu

#Just include average rating of each film 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% #group by movie
  summarize(b_i = mean(rating - mu)) #add a term b_i which adjusts rating based on movie

#produce predictions based on adjusting depending on movie
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>% 
  mutate(pred = mu + b_i) %>%
  pull(pred) 

rmse_movs <- RMSE(predicted_ratings, test_set$rating) #test how good this prediction is
rmse_movs

#Include a term b_u which adjusts rating based on user giving rating 
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%       #group by user
  summarize(b_u = mean(rating - mu - b_i)) #add a term b_u which adjusts rating based on user

#Predict ratings based on film and user
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

rmse_user <- RMSE(predicted_ratings, test_set$rating) #test how good this prediction is
rmse_user

#Include genres
genre_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u)) #add term to adjust for popularity of that combination of genres

predicted_ratings <- test_set %>%  
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

rmse_genres <- RMSE(predicted_ratings, test_set$rating) #test how good this prediction is
rmse_genres

#Include year
year_set <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(rating_year= floor(timestamp/(3600*24*365))+1970) %>% #define the rating year
  group_by(rating_year) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u-b_g)) #add a term b_y which adjusts rating based on year

#predict ratings of test set
predicted_ratings <- test_set %>% 
  mutate(rating_year= floor(timestamp/(3600*24*365))+1970) %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(year_set, by='rating_year') %>%
  mutate(pred=ifelse(is.na(b_y), mu + b_i + b_u + b_g, mu + b_i + b_u + b_g + b_y)) %>%
  pull(pred)

RMSE(predicted_ratings, test_set$rating) #test how good this prediction is

#Instead include time of day
hour_set <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(rating_hour= round((timestamp/3600) %% 24)) %>% #define hour of day
  group_by(rating_hour) %>%
  summarize(b_h = mean(rating - mu - b_i - b_u-b_g, na.rm=TRUE)) #add a term b_h which adjusts rating based on hour

#predict ratings of test set
predicted_ratings <- test_set %>% 
  mutate(rating_hour= round((timestamp/3600) %% 24)) %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(hour_set, by='rating_hour') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_h) %>%
  pull(pred)

RMSE(predicted_ratings,test_set$rating) #test how good this prediction is

#Number of months since first rating of film
months_film_set <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(movieId) %>%
  mutate(months_since_film=floor((timestamp - min(timestamp))/(30*24*3600))) %>%
  ungroup() %>%
  group_by(months_since_film) %>%
  summarize(b_f = mean(rating - mu - b_i -b_u, na.rm=TRUE)) #add a term b_f which adjusts based on time since film's first rating

#predict ratings of test set
predicted_ratings <- test_set %>% 
  group_by(movieId) %>%
  mutate(months_since_film=floor((timestamp - min(timestamp))/(30*24*3600))) %>%
  ungroup() %>%
  group_by(userId) %>%
  mutate(sqrt_months_since_rating=floor(sqrt(abs((timestamp - min(timestamp))/(30*24*3600))))) %>%
  ungroup() %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(months_film_set, by='months_since_film') %>%
  mutate(pred = mu + b_i + b_u + b_f) %>%
  pull(pred)
rmse_movie_time <- RMSE(predicted_ratings,test_set$rating, na.rm=TRUE) #test how good this prediction is
rmse_movie_time

#Number of months since user's first rating
months_rating_set <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(movieId) %>%
  mutate(months_since_film=floor((timestamp - min(timestamp))/(30*24*3600))) %>%
  ungroup() %>%
  left_join(months_film_set, by='months_since_film') %>%
  group_by(userId) %>%
  mutate(sqrt_months_since_rating=floor(sqrt(abs((timestamp - min(timestamp))/(30*24*3600))))) %>%
  ungroup() %>%
  group_by(sqrt_months_since_rating) %>%
  summarize(b_d = mean(rating - mu - b_i - b_u, na.rm=TRUE)) #add a term b_d which adjusts based on time since user's first rating

#predict ratings of test set
predicted_ratings <- test_set %>% 
  group_by(movieId) %>%
  mutate(months_since_film=floor((timestamp - min(timestamp))/(30*24*3600))) %>%
  ungroup() %>%
  group_by(userId) %>%
  mutate(sqrt_months_since_rating=floor(sqrt(abs((timestamp - min(timestamp))/(30*24*3600))))) %>%
  ungroup() %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(months_film_set, by='months_since_film') %>%
  left_join(months_rating_set, by='sqrt_months_since_rating') %>%
  mutate(pred = mu + b_i + b_u + b_d + b_f) %>%
  pull(pred)

rmse_user_time <-RMSE(predicted_ratings,test_set$rating, na.rm=TRUE) #test how good this prediction is
rmse_user_time

##########################################################
# Regularisation while choosing tuning parameter
##########################################################


#split training data up to tune lambda
test_i <- createDataPartition(y = train_set$rating, times = 1, p = 0.5, list = FALSE)
train_set_1 <- train_set[-test_i,]
train_set_2 <- train_set[test_i,]
train_set_2 <- train_set_2 %>% 
  semi_join(train_set_1, by = "movieId") %>%
  semi_join(train_set_1, by = "userId")

#Set a wide range for lambdas
lambdas <- seq(0, 10, 0.25)
#Find out the RMSE for each value of lambda
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set_1$rating)
  
  #Term to correct for more or less popular movies
  b_i <- train_set_1 %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l)) #add b_i which adjusts for popularity of movie
  
  #Term to correct for more or less harsh users
  b_u <- train_set_1 %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l)) #add b_u which adjusts for harshness of users
  
  #Adjust rating depending on number of months since film's first rating
  months_film_set <- train_set_1 %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(movieId) %>%
    mutate(months_since_film=floor((timestamp - min(timestamp))/(30*24*3600))) %>%
    ungroup() %>%
    group_by(months_since_film) %>%
    summarize(b_f = sum(rating - mu - b_i -b_u)/(n()+l)) #add b_f which adjusts for time since film's first rating
  
  #Adjust rating depending on number of months since user's first rating
  months_rating_set <- train_set_1 %>%
    group_by(movieId) %>%
    mutate(months_since_film=floor((timestamp - min(timestamp))/(30*24*3600))) %>%
    ungroup() %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(months_film_set, by='months_since_film') %>%
    group_by(userId) %>%
    mutate(sqrt_months_since_rating=floor(sqrt(abs((timestamp - min(timestamp))/(30*24*3600))))) %>%
    ungroup() %>%
    group_by(sqrt_months_since_rating) %>%
    summarize(b_d = sum(rating - mu - b_i - b_u - b_f)/(n()+l)) #add b_d which adjusts for time since user's first rating
  
  #Predict ratings for train_set_2
  predicted_ratings <- train_set_2 %>% 
    group_by(movieId) %>%
    mutate(months_since_film=floor((timestamp - min(timestamp))/(30*24*3600))) %>%
    ungroup() %>%
    group_by(userId) %>%
    mutate(sqrt_months_since_rating=floor(sqrt(abs((timestamp - min(timestamp))/(30*24*3600))))) %>%
    ungroup() %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(months_rating_set, by='sqrt_months_since_rating') %>%
    mutate(pred = mu + b_i + b_u + b_d ) %>%
    pull(pred)
  
  RMSE(predicted_ratings, train_set_2$rating, na.rm=TRUE) #test how good this prediction is
})

lambda <- lambdas[which.min(rmses)] #find the value of lambda which minimises the RMSE
lambda
rmse_wide_range  <- data.frame(lambdas, rmses)
ggplot(data=rmse_wide_range, aes(x=lambdas, y=rmses))+ #plot the error in the prediction against the value of lambda
  geom_point() +
  labs(x=expression(paste(lambda)), y= "RMSE")


#Set a narrower range for lambdas
lambdas <- seq(4, 6, 0.05)
#Find out the RMSE for each value of lambda
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set_1$rating)
  
  #Term to correct for more or less popular movies
  b_i <- train_set_1 %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l)) #add b_i which adjusts for popularity of movie
  
  #Term to correct for more or less harsh users
  b_u <- train_set_1 %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l)) #add b_u which adjusts for harshness of users
  
  #Adjust rating depending on number of months since film's first rating
  months_film_set <- train_set_1 %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(movieId) %>%
    mutate(months_since_film=floor((timestamp - min(timestamp))/(30*24*3600))) %>%
    ungroup() %>%
    group_by(months_since_film) %>%
    summarize(b_f = sum(rating - mu - b_i -b_u)/(n()+l)) #add b_f which adjusts for time since film's first rating
  
  #Adjust rating depending on number of months since user's first rating
  months_rating_set <- train_set_1 %>%
    group_by(movieId) %>%
    mutate(months_since_film=floor((timestamp - min(timestamp))/(30*24*3600))) %>%
    ungroup() %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(months_film_set, by='months_since_film') %>%
    group_by(userId) %>%
    mutate(sqrt_months_since_rating=floor(sqrt(abs((timestamp - min(timestamp))/(30*24*3600))))) %>%
    ungroup() %>%
    group_by(sqrt_months_since_rating) %>%
    summarize(b_d = sum(rating - mu - b_i - b_u - b_f)/(n()+l)) #add b_d which adjusts for time since user's first rating
  
  #Predict ratings for train_set_2
  predicted_ratings <- train_set_2 %>% 
    group_by(movieId) %>%
    mutate(months_since_film=floor((timestamp - min(timestamp))/(30*24*3600))) %>%
    ungroup() %>%
    group_by(userId) %>%
    mutate(sqrt_months_since_rating=floor(sqrt(abs((timestamp - min(timestamp))/(30*24*3600))))) %>%
    ungroup() %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(months_rating_set, by='sqrt_months_since_rating') %>%
    mutate(pred = mu + b_i + b_u + b_d ) %>%
    pull(pred) #pull out the predictions into a list
  
  RMSE(predicted_ratings, train_set_2$rating, na.rm=TRUE) #test how good this prediction is
})

lambda <- lambdas[which.min(rmses)] #find the value of lambda which minimises the RMSE
lambda
rmse_narrow_range  <- data.frame(lambdas, rmses)
ggplot(data=rmse_narrow_range, aes(x=lambdas, y=rmses))+
  geom_point() +
  labs(x=expression(paste(lambda)), y= "RMSE") #plot the error in the prediction against the value of lambda

#take mean of training set
mu <- mean(train_set$rating)

#correct for popularity of movie
b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

#correct for harshness of user
b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

#Adjust for number of months since film's first rating
months_film_set <- train_set %>%
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  group_by(movieId) %>%
  mutate(months_since_film=floor((timestamp - min(timestamp))/(30*24*3600))) %>%
  ungroup() %>%
  group_by(months_since_film) %>%
  summarize(b_f = sum(rating - mu - b_i -b_u)/(n()+lambda))

#Number of months since user's first rating
months_rating_set <- train_set %>%
  group_by(movieId) %>%
  mutate(months_since_film=floor((timestamp - min(timestamp))/(30*24*3600))) %>%
  ungroup() %>%
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(months_film_set, by='months_since_film') %>%
  group_by(userId) %>%
  mutate(sqrt_months_since_rating=floor(sqrt(abs((timestamp - min(timestamp))/(30*24*3600))))) %>%
  ungroup() %>%
  group_by(sqrt_months_since_rating) %>%
  summarize(b_d = sum(rating - mu - b_i - b_u - b_f)/(n()+lambda))

#Predict ratings
predicted_ratings <- test_set %>% 
  group_by(movieId) %>%
  mutate(months_since_film=floor((timestamp - min(timestamp))/(30*24*3600))) %>%
  ungroup() %>%
  group_by(userId) %>%
  mutate(sqrt_months_since_rating=floor(sqrt(abs((timestamp - min(timestamp))/(30*24*3600))))) %>%
  ungroup() %>%
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(months_rating_set, by='sqrt_months_since_rating') %>%
  mutate(pred = mu + b_i + b_u + b_d ) %>%
  mutate(pred=ifelse(pred>5,5,pred)) %>% #Making sure all ratings stay within range
  mutate(pred=ifelse(pred<0,0,pred)) %>%
  pull(pred)

rmse_reg <- RMSE(predicted_ratings, test_set$rating, na.rm=TRUE) #test how good this prediction is
rmse_reg

names_rmse <- c("Mean", "Movie", "User", "Time since movie", "Time since user's first review","Regularisation")
results_rmse <- c(rmse_mu, rmse_movs, rmse_user, rmse_movie_time, rmse_user_time, rmse_reg)
df <- data.frame(names_rmse, results_rmse)
ggplot(data=df, aes(x= reorder(names_rmse,  results_rmse), y=results_rmse)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(x="Method", y="RMSE", subtitle="RMSE against additional method used")

ggplot(data=df, aes(x= reorder(names_rmse,  results_rmse), y=results_rmse)) +
  geom_bar(stat="identity") +
  coord_flip(ylim=c(0.86,1.06)) +
  labs(x="Method", y="RMSE", subtitle="RMSE against additional method used (zoomed in)")

##########################################################
# Validate final method
##########################################################

#Regularisation with lambda=4.8
mu <- mean(edx$rating)
l <- 4.8
b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))


#Number of months since first rating of film
months_film_set <- edx %>%
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  group_by(movieId) %>%
  mutate(months_since_film=floor((timestamp - min(timestamp))/(30*24*3600))) %>%
  ungroup() %>%
  group_by(months_since_film) %>%
  summarize(b_f = sum(rating - mu - b_i -b_u)/(n()+l))

#Number of months since user's first rating
months_rating_set <- edx %>%
  group_by(movieId) %>%
  mutate(months_since_film=floor((timestamp - min(timestamp))/(30*24*3600))) %>%
  ungroup() %>%
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(months_film_set, by='months_since_film') %>%
  group_by(userId) %>%
  mutate(sqrt_months_since_rating=floor(sqrt(abs((timestamp - min(timestamp))/(30*24*3600))))) %>%
  ungroup() %>%
  group_by(sqrt_months_since_rating) %>%
  summarize(b_d = sum(rating - mu - b_i - b_u - b_f)/(n()+l))

#Predict ratings
predicted_ratings <- validation %>% 
  group_by(userId) %>%
  mutate(sqrt_months_since_rating=floor(sqrt(abs((timestamp - min(timestamp))/(30*24*3600))))) %>%
  ungroup() %>%
  group_by(movieId) %>%
  mutate(months_since_film=floor((timestamp - min(timestamp))/(30*24*3600))) %>%
  ungroup() %>%
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(months_rating_set, by='sqrt_months_since_rating') %>%
  left_join(months_film_set, by='months_since_film') %>%
  mutate(pred = mu + b_i + b_u + b_d + b_f) %>%
  mutate(pred=ifelse(pred>5,5,pred)) %>%
  mutate(pred=ifelse(pred<0,0,pred)) %>%
  pull(pred)

RMSE(predicted_ratings, validation$rating, na.rm=TRUE) #test how good this prediction is

