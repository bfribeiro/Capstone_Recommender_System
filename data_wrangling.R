#############################################################
#							                                              #
#                      DATA WRANGLING                       #  
#							                                              #
#############################################################

# Load the necessary libraries to work
library(tidyverse)
library(caret)
library(lubridate)

# Load the datasets into R space
edx <- read_csv("data/raw_edx.csv")
validation <- read.csv("data/raw_validation.csv")

dim(edx)
dim(validation)

summary(edx)
summary(validation)

str(edx)
str(validation)

head(edx)

# Need to separate the year from the title and correct the timestamp to a Date format. To make easier to work 
# in both datasets, we'll  merge the two datasets to work on both of them at the same time. To not forget the 
# dimensions of both sets, we added a "temporary flag" into both of them: 0 for the edx and 1 for the validation.

flag <- c(rep(0, nrow(edx)), rep(1, nrow(validation)))

complete <- rbind(edx, validation)
complete <- complete %>% 
  mutate(flag=flag)

dim(complete)

# Create and year vector to store the years from the title and then add before "genres"

year <- str_extract_all(complete$title, "\\([0-9]{4}\\)") %>% 
  str_extract_all("[0-9]{4}") %>% 
  unlist() %>% 
  as.numeric()

complete <- complete %>%
  add_column(year, .before="genres")

# Now we remove the year from the "title"
complete <- complete %>% 
  mutate(title=str_remove_all(title, " \\(|[0-9]{4}\\)"))

# Convert the timestamp to date

complete <- complete %>% 
  mutate(timestamp=as.POSIXct(timestamp, origin="1970-01-1")) %>% 
  mutate(timestamp=as.Date(timestamp)) # Since the time doesn't matter, we keep only the date.


# Since different periods of cinema have different "appeals", we will create a new variable, called "era".
# I referes to the period of the cinema: 30's, 40's, 50's... etc.

complete <- complete %>% # This will turn the years into 10, 20, 30, 40...and 2000 if above 2000
  mutate(period=ifelse(year<2000, floor((year-1900)/10)*10 , 2000)) 

complete <- add_column(complete, era=NA)

complete$era[complete$period<30] <- "Before 30's"
complete$era[complete$period==30] <- "30's"
complete$era[complete$period==40] <- "40's"
complete$era[complete$period==50] <- "50's"
complete$era[complete$period==60] <- "60's"
complete$era[complete$period==70] <- "70's"
complete$era[complete$period==80] <- "80's"
complete$era[complete$period==90] <- "90's"
complete$era[complete$period>90] <- "2000's"

# Let's reorder the dataset just to make it easier to read and remove the period column

complete <- complete %>% 
  select(userId, movieId, timestamp, title, year, era, genres, rating, flag,  -period)

# Split the dataset again

edx <- complete %>% 
  filter(flag==0) %>% 
  select(-flag)

validation <- complete %>% 
  filter(flag==1) %>% 
  select(-flag)

# Save the new datasets

write_csv(edx, "data/edx.csv")
write_csv(validation, "data/validation.csv")

# No need to keep the complete and validation datasets in the enviroment. Clean the "garbage" and return some memory to OS
rm(complete, validation, flag, year)
gc()
