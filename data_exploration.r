#############################################################
#							    
#                     DATA EXPLORATION                      
#							    
#############################################################

# General info
summary(edx) 
dim(edx) 

edx %>% 
  select(movieId, userId) %>% 
  summarize('unique movies'=n_distinct(movieId), 'unique users'=n_distinct(userId))

# Ratings distribution
edx %>% 
  group_by(rating) %>% 
  summarize(N=n()) %>% 
  arrange(desc(N))

# Plot ratings distibution
edx %>% 
  group_by(rating) %>% 
  summarize(N=n()) %>% 
  ggplot(aes(x=rating, y=N))+
  geom_histogram(stat = "identity")+
  labs(title="Ratings Distribution", x="Ratings", y="N")


# Views by genres
edx %>% 
  separate_rows(genres, sep="\\|") %>% 
  group_by(genres) %>% 
  summarize(N=n()) %>% 
  arrange(desc(N))

  
# Plot # of views by genre
edx %>%
  separate_rows(genres, sep="\\|") %>% 
  group_by(genres) %>% 
  summarize(N=n()) %>% 
  ggplot(aes(x=genres, y=N))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title="View by Genre", x="Years", y="N")
  
  
# Number of times movies were watched
edx %>% 
  group_by(title) %>% 
  summarize(N=n()) %>% 
  arrange(desc(N))

# Views by era of the cinema

edx %>% 
  group_by(era) %>% 
  summarize(N=n()) %>% 
  arrange(desc(N))
  
edx %>% 
  group_by(era) %>% 
  summarize(N=n()) %>% 
  ggplot(aes(x=era, y=N))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title="Views by Cinema Era", x="Years", y="N")

# Most watched movies of each era
most_watched <- c()
period <- c("Before 30's", "30's", "40's", "50's", "60's", "70's", "80's", "90's", "2000's")
for (i in 1:length(period)){
  
  most_watched <- rbind(most_watched,
        edx %>% 
          filter(era==period[i]) %>% 
          group_by(title) %>% 
          summarize(N=n()) %>% 
          top_n(1)
        )}
most_watched <- cbind(most_watched,era=period)

# Extract the average rating to make some observation of the rating behaviour
mu <- mean(edx$rating)

# Average ratings considering the "distance" between movie premiere and being watched
edx %>% 
  mutate(Difference=abs(year(timestamp)-year)) %>% 
  select(Difference, rating) %>% 
  ggplot(aes(x=Difference, y=(rating-mu)))+
  geom_bar(stat="summary", fun.y="mean")+
  scale_x_continuous(breaks = seq(0,93,10))+
  labs(x="Difference in years", y="Average Rating")

# Average ratings considering eras
edx %>% 
  group_by(era) %>% 
  ggplot(aes(x=era, y=(rating-mu)))+
  geom_bar(position = "dodge", stat="summary", fun.y="mean")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x="Era", y="Average Rating")

# Average ratings considering genres
edx %>% 
  separate_rows(genres, sep="\\|") %>% 
  ggplot(aes(x=genres, y=(rating-mu)))+
  geom_bar(position = "dodge", stat="summary", fun.y="mean")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(breaks=seq(-0.5, 0.5, 0.25))+
  labs(x="Genres", y="Average Rating")

