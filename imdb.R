library(tidyverse)
library(dplyr)
library(plotly)

movies <- read_csv('./data/data_raw/tmdb-5000-movie-dataset/tmdb_5000_movies.csv')
#credits <- read_csv('./data/data_raw/tmdb-5000-movie-dataset/tmdb_5000_credits.csv')
colnames(movies)[colnames(movies) == 'vote_average'] <- 'rating'
colnames(movies)

# remove invalid record
movies <- movies %>%  
  filter(!is.na(release_date))

colnames(movies)
#colnames(credits)

head(movies, 1)

dim(movies)
# rating, length
runtime_rating_plot <- ggplot(data = movies, aes(x = runtime, y = rating))
runtime_rating_plot +
  geom_point()

# remove very long movies
normal_length_movies <- movies %>% filter(runtime <= 240 & runtime > 0) 
ggplot(normal_length_movies, aes(runtime, rating)) +
  geom_point()

# rating, runtime over time
ggplot(normal_length_movies, aes(runtime, rating)) +
  geom_point(aes(colour = release_date)) +
  scale_color_gradient(low = 'yellow', high = 'red')

ggplot(normal_length_movies, aes(x = release_date, y = rating)) +
  geom_point(aes(colour = runtime)) +
  scale_color_gradient(low = 'yellow', high = 'red')

#zero rating
normal_length_movies %>% ggplot(aes(rating, vote_count)) +
  geom_bar(stat = "identity")

normal_length_movies <- normal_length_movies %>% 
  filter(rating > 2.5) %>% 
  filter(rating < 9)

plot <- ggplot(normal_length_movies, aes(x = release_date, y = runtime)) +
  geom_point(aes(colour = rating)) +
  scale_color_gradient(low = 'yellow', high = 'red')

ggplotly(plot)

# plotly
