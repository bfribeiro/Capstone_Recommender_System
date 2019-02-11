#############################################################
#							    
#                        MODELLING                      
#							    
############################################################

# Since the title of the movie is not relevant fot he modelling, we'll drop it.
edx <- edx %>% 
  select(-title)

# Split the dataset into train and test sets
set.seed(755)
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

# Make sure to don’t include users and movies in the test set that do not appear in the training set
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Remove not necessary objects
rm(test_index)

# Release the memory
gc()

# Define the naseline rating, here it will be the average
mu <- train_set %>% 
  summarize(mu=mean(rating)) %>% 
  pull()

# Test the average against the ratings to define a starting point and store de RMSEs for comparison

rmse <- data.frame("Method"="Average", "RMSE"=RMSE(mu, test_set$rating))
knitr::kable(rmse)

############################################################################################

# Estimating the movie bias

movie_avg <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i=mean(rating-mu))

# Predictions
y_hat <- test_set %>% 
  left_join(movie_avg, by="movieId") %>% 
  mutate(prediction=mu+b_i) %>% 
  .$prediction

# Store the new RMSE
rmse <- rmse %>% 
  rbind(data.frame("Method"="Average + Movie Bias", "RMSE"=RMSE(y_hat, test_set$rating)))

###########################################################################################

# Estimating the user bias
usr_avg <- train_set %>% 
  left_join(movie_avg, by="movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u=mean(rating-mu-b_i))

# Predictions
y_hat <- test_set %>% 
  left_join(movie_avg, by="movieId") %>% 
  left_join(usr_avg, by="userId") %>% 
  mutate(prediction=mu+b_i+b_u) %>% 
  .$prediction


# Note that the range of y_hat is below 0 and above 5.
# Since there are no values below 0.5 in the dataset, everything below 0 will become 0.5 and
# everything above 5 will become 5.

y_hat[y_hat<0] <- 0.5
y_hat[y_hat>5] <- 5

# Store the new RMSE
rmse <- rmse %>% 
  rbind(data.frame("Method"="Average + User and Movie Biases", "RMSE"=RMSE(y_hat, test_set$rating)))



# Since genres, the difference between time it was watched and time it was produced so as the era
# seem to have some effect in the rating, this values will also be estimated

###########################################################################################

# Estimating the Era bias

era_avg <- train_set %>% 
  left_join(movie_avg, by="movieId") %>% 
  left_join(usr_avg, by="userId") %>% 
  group_by(era) %>% 
  summarize(b_e=mean(rating-mu-b_i-b_u))

# Predictions
y_hat <- test_set %>% 
  left_join(movie_avg, by="movieId") %>% 
  left_join(usr_avg, by="userId") %>% 
  left_join(era_avg, by="era") %>% 
  mutate(prediction=mu+b_i+b_u+b_e) %>% 
  .$prediction

# "Correct" the range
y_hat[y_hat<0] <- 0.5
y_hat[y_hat>5] <- 5

# Store the new RMSE
rmse <- rmse %>% 
  rbind(data.frame("Method"="Average + Era, User and Movie Biases", "RMSE"=RMSE(y_hat, test_set$rating)))

###########################################################################################

# Estimating the genres bias. For simplicity, here, it'll only be considered the first (principal) genre 
# of the movie.

genres_avg <- train_set %>% 
  separate(genres, "genres", sep="\\|", extra="drop") %>% 
  left_join(movie_avg, by="movieId") %>% 
  left_join(usr_avg, by="userId") %>% 
  left_join(era_avg, by="era") %>% 
  group_by(genres) %>% 
  summarize(b_g=mean(rating-mu-b_i-b_u-b_e))

# Predictions
y_hat <- test_set %>% 
  separate(genres, "genres", sep="\\|", extra="drop") %>% 
  left_join(movie_avg, by="movieId") %>% 
  left_join(usr_avg, by="userId") %>% 
  left_join(era_avg, by="era") %>% 
  left_join(genres_avg, by="genres") %>% 
  mutate(prediction=mu+b_i+b_u+b_e+b_g) %>% 
  .$prediction

y_hat[y_hat<0] <- 0.5
y_hat[y_hat>5] <- 5

# Store the new RMSE
rmse <- rmse %>% 
  rbind(data.frame("Method"="Average + Genres, Era, User and Movie Biases", "RMSE"=RMSE(y_hat, test_set$rating)))

###########################################################################################

# Estimating a "time" bias. It's the difference between the movie premiere and when it was watched.

time_diff_avg <- train_set %>% 
  separate(genres, "genres", sep="\\|", extra="drop") %>% 
  mutate(timeDiff=abs(year(timestamp)-year)) %>% 
  left_join(movie_avg, by="movieId") %>% 
  left_join(usr_avg, by="userId") %>% 
  left_join(era_avg, by="era") %>% 
  left_join(genres_avg, by="genres") %>% 
  group_by(timeDiff) %>% 
  summarize(b_t=mean(rating-mu-b_i-b_u-b_e-b_g))

# Predictions
y_hat <- test_set %>% 
  separate(genres, "genres", sep="\\|", extra="drop") %>% 
  mutate(timeDiff=abs(year(timestamp)-year)) %>% 
  left_join(movie_avg, by="movieId") %>% 
  left_join(usr_avg, by="userId") %>% 
  left_join(era_avg, by="era") %>% 
  left_join(genres_avg, by="genres") %>% 
  left_join(time_diff_avg, by="timeDiff") %>% 
  mutate(prediction=mu+b_i+b_u+b_e+b_g+b_t) %>% 
  .$prediction

# "Correct" the range
y_hat[y_hat<0] <- 0.5
y_hat[y_hat>5] <- 5


# Store the new RMSE
rmse <- rmse %>% 
  rbind(data.frame("Method"="Average + Time, Genres, Era, User and Movie Biases", "RMSE"=RMSE(y_hat, test_set$rating)))

#############################################################
#                      Regularization                    
############################################################

# To define the value to pick for regularization, we'll perform a cross-validation to verify which value fits better
# to minimize the model error.

# Define the values to test
lambdas <- seq(0, 10 , 0.2)

# Build the function to cross-validate.
# This function will take a few minutes running
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_e <- train_set %>% 
    left_join(b_i, by="movieId") %>% 
    left_join(b_u, by="userId") %>% 
    group_by(era) %>% 
    summarize(b_e=sum(rating-mu-b_i-b_u)/(n()+l))
  
  b_g <- train_set %>% 
    separate(genres, "genres", sep="\\|", extra="drop") %>% 
    left_join(b_i, by="movieId") %>% 
    left_join(b_u, by="userId") %>% 
    left_join(b_e, by="era") %>% 
    group_by(genres) %>% 
    summarize(b_g=sum(rating-mu-b_i-b_u-b_e)/(n()+l))
  
  b_t <- train_set %>% 
    separate(genres, "genres", sep="\\|", extra="drop") %>% 
    mutate(timeDiff=abs(year(timestamp)-year)) %>% 
    left_join(b_i, by="movieId") %>% 
    left_join(b_u, by="userId") %>% 
    left_join(b_e, by="era") %>% 
    left_join(b_g, by="genres") %>% 
    group_by(timeDiff) %>% 
    summarize(b_t=sum(rating-mu-b_i-b_u-b_e-b_g)/(n()+l))
  
  y_hat <- test_set %>% 
    separate(genres, "genres", sep="\\|", extra="drop") %>% 
    mutate(timeDiff=abs(year(timestamp)-year)) %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_e, by= "era") %>% 
    left_join(b_g, by= "genres") %>% 
    left_join(b_t, by= "timeDiff") %>% 
    mutate(prediction=mu+b_i+b_u+b_e+b_g+b_t) %>% 
    .$prediction
  
  return(RMSE(y_hat, test_set$rating))
})

# Pick the lambda value with the minor rmse
lambdas[which.min(rmses)]

rmse <- rmse %>% 
  rbind(data.frame("Method"="Regularized, Average + Time, Genres, Era, User and Movie Biases", "RMSE"=RMSE(y_hat, test_set$rating)))

knitr::kable(rmse)

write_csv(rmse, 'suppor_data/train_rmses.csv')