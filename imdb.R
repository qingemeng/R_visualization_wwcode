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
  geom_point(colour = 'blue')

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
  scale_color_gradient(low = 'yellow', high = 'red') +
  xlab('release date') +
  ylab('length') + 
  ggtitle("runtime vs rating over years")

ggplotly(plot)

# genre
head(movies$genres)

devtools::install_github("sailthru/tidyjson")
library('tidyjson')
# convert and gether json
movies2 <- movies %>% 
  as.tbl_json(json.column = "genres") %>% 
  gather_array() %>% 
  spread_values(genre=jstring('name'))

g_genre <- ggplot(movies2, aes(genre, rating))
g_genre + geom_boxplot(aes(fill = genre))

# get frequecy by gener
genre_frequencies <- movies2 %>%
  group_by(genre) %>% 
  summarize(count=n())

ggplot(genre_frequencies, aes(genre,count)) +
  geom_bar(stat = 'identity')

#get rating of action movies over year
action_movies <- subset(movies2, genre = 'Action') 
g_action_movies_plot <- ggplot(action_movies, aes(release_date, rating))
g_action_movies_plot + 
  geom_point() +
  geom_smooth()

movies3 <- filter(movies2, genre == 'Action' | genre == 'Drama' | genre == 'Animation')
p_movies3 <- ggplot(movies3, aes(release_date, rating))
p_movies3 + 
  geom_point(aes(color = genre)) +
  facet_grid(genre~.)
