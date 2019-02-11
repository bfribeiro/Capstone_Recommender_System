#############################################################
#							    
#                        VALIDATION                      
#							    
############################################################

# Clean the environment and load the datasets again
rm(list=ls())
gc()

edx <- read_csv('data/edx.csv')
validation <- read_csv('data/validation.csv')

edx <- edx %>% 
  select(-title)

validation <- validation %>% 
  select(-title)

# Define lambra as 5
l <- 5

# Only the average
mu <- mean(edx$rating)

rmse <- data.frame('Method'='Average', RMSE=RMSE(mu, validation$rating))


# Movie bias

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))

# Predictions

y_hat <- validation %>% 
  left_join(b_i, by="movieId") %>% 
  mutate(prediction=mu+b_i) %>% 
  .$prediction

# Store the RMSE
rmse <- rmse %>% 
  rbind(data.frame("Method"="Average + Movie Bias", "RMSE"=RMSE(y_hat, validation$rating)))


# User bias
b_u <- edx  %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))

# Predictions
y_hat <- validation %>% 
  left_join(b_i, by="movieId") %>% 
  left_join(b_u, by="userId") %>% 
  mutate(prediction=mu+b_i+b_u) %>% 
  .$prediction

# "Correect" the range
y_hat[y_hat<0] <- 0.5
y_hat[y_hat>5] <- 5

# Store the RMSE
rmse <- rmse %>% 
  rbind(data.frame("Method"="Average + User and Movie Biases", "RMSE"=RMSE(y_hat, validation$rating)))

# Era bias
b_e <- edx%>% 
  left_join(b_i, by="movieId") %>% 
  left_join(b_u, by="userId") %>% 
  group_by(era) %>% 
  summarize(b_e=sum(rating-mu-b_i-b_u)/(n()+l))

# Predictions
y_hat <- validation %>% 
  left_join(b_i, by="movieId") %>% 
  left_join(b_u, by="userId") %>% 
  left_join(b_e, by="era") %>% 
  mutate(prediction=mu+b_i+b_u+b_e) %>% 
  .$prediction

# "Correect" the range
y_hat[y_hat<0] <- 0.5
y_hat[y_hat>5] <- 5

# Store the RMSE
rmse <- rmse %>% 
  rbind(data.frame("Method"="Average + Era, User and Movie Biases", "RMSE"=RMSE(y_hat, validation$rating)))

# Genre bias
b_g <- edx %>% 
  separate(genres, "genres", sep="\\|", extra="drop") %>% 
  left_join(b_i, by="movieId") %>% 
  left_join(b_u, by="userId") %>% 
  left_join(b_e, by="era") %>% 
  group_by(genres) %>% 
  summarize(b_g=sum(rating-mu-b_i-b_u-b_e)/(n()+l))

# Predictions
y_hat <- validation %>% 
  separate(genres, "genres", sep="\\|", extra="drop") %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_e, by= "era") %>% 
  left_join(b_g, by= "genres") %>% 
  mutate(prediction=mu+b_i+b_u+b_e+b_g) %>% 
  .$prediction

# "Correct" the range
y_hat[y_hat<0] <- 0.5
y_hat[y_hat>5] <- 5

# Store the new RMSE
rmse <- rmse %>% 
  rbind(data.frame("Method"="Average + Genres, Era, User and Movie Biases", "RMSE"=RMSE(y_hat, validation$rating)))


# Time bias
b_t <- edx %>% 
  separate(genres, "genres", sep="\\|", extra="drop") %>% 
  mutate(timeDiff=abs(year(timestamp)-year)) %>% 
  left_join(b_i, by="movieId") %>% 
  left_join(b_u, by="userId") %>% 
  left_join(b_e, by="era") %>% 
  left_join(b_g, by="genres") %>% 
  group_by(timeDiff) %>% 
  summarize(b_t=sum(rating-mu-b_i-b_u-b_e-b_g)/(n()+l))

#Prediction
y_hat <- validation %>% 
  separate(genres, "genres", sep="\\|", extra="drop") %>% 
  mutate(timeDiff=abs(year(timestamp)-year)) %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_e, by= "era") %>% 
  left_join(b_g, by= "genres") %>% 
  left_join(b_t, by= "timeDiff") %>% 
  mutate(prediction=mu+b_i+b_u+b_e+b_g+b_t) %>% 
  .$prediction

# "Correct" the range
y_hat[y_hat<0] <- 0.5
y_hat[y_hat>5] <- 5


# Store the new RMSE
rmse <- rmse %>% 
  rbind(data.frame("Method"="Average + Time, Genres, Era, User and Movie Biases", "RMSE"=RMSE(y_hat, validation$rating)))


# Comparison between training and validation RMSES

rmse <- rmse %>% 
  rbind(data.frame("Method"="Regularized, Average + Time, Genres, Era, User and Movie Biases", "RMSE"=NA))

train_rmses <- read_csv('support_data/train_rmses.csv')

rmses <- cbind(train_rmses, rmse[,2])
colnames(rmses) <- c("Method", "Training RMSE", "Validation RMSE")

write_csv(rmses, 'support_data/train_val_rmses.csv')

