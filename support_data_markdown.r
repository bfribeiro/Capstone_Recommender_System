#Ratings distribution

ratings_distribution <- edx %>% 
  group_by(rating) %>% 
  summarize(N=n()) %>% 
  arrange(desc(N))

write_csv(ratings_distribution, 'support_data/ratings_distribution.csv')

#Views by Genres


# Views by genres
genres_views <- edx %>% 
  separate_rows(genres, sep="\\|") %>% 
  group_by(genres) %>% 
  summarize(N=n()) %>% 
  arrange(desc(N))

write_csv(genres_views, 'support_data/genres_views.csv')


#Views by era

era_views <- edx %>% 
  group_by(era) %>% 
  summarize(N=n()) %>% 
  arrange(desc(N))

write_csv(era_views, 'support_data/era_views.csv')

#Difference between years (production and watched) and ratings
year_diff_avg_rating <- edx %>% 
  mutate(Difference=abs(year(timestamp)-year)) %>% 
  select(Difference, rating)

write_csv(year_diff_avg_rating, 'support_data/year_diff_avg_rating.csv')

# Era average rating
era_avg_rating <- edx %>% 
  group_by(era) %>% 
  select(era, rating)
  
write_csv(era_avg_rating, 'support_data/era_avg_rating.csv')

# Genres average rating

genres_avg_rating <- edx %>% 
  separate_rows(genres, sep="\\|") %>% 
  select(genres, rating)

write_csv(genres_avg_rating, 'support_data/genres_avg_rating.csv')


p1 <- ratings_year_diff %>% 
  ggplot(aes(x=Difference, y=(rating-mu)))+
  geom_bar(stat="summary", fun.y="mean")+
  scale_x_continuous(breaks = seq(0,93,10))+
  labs(x="Difference in years", y="Average Rating")

p2 <- era_avg_rating %>% 
  ggplot(aes(x=era, y=(rating-mu)))+
  geom_bar(position = "dodge", stat="summary", fun.y="mean")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x="Era", y="Average Rating")


p3 <- genres_avg_rating %>% 
  ggplot(aes(x=genres, y=(rating-mu)))+
  geom_bar(position = "dodge", stat="summary", fun.y="mean")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(breaks=seq(-0.5, 0.5, 0.25))+
  labs(x="Genres", y="Average Rating")

grid.arrange(p1,p2,p3, nrow=2, ncol=2)
