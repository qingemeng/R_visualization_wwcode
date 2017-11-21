library(tidyverse)
library(dplyr)
library(plotly)
library(tidyjson)

movies <- read_csv('./data/data_raw/tmdb-5000-movie-dataset/tmdb_5000_movies.csv')
colnames(movies)
colnames(movies)[colnames(movies) == 'vote_average'] <- 'rating'
colnames(movies)

# remove invalid record
movies <- movies %>%  
  filter(!is.na(release_date))

head(movies, 1)
dim(movies)

#TODO: rating, runtime


# remove very long movies
tidy_movies <- movies %>% filter(runtime <= 240 & runtime > 0) 
ggplot(tidy_movies, aes(runtime, rating)) +
  geom_point(colour = 'blue')

#0/10 points rating
tidy_movies %>% ggplot(aes(rating, vote_count)) +
  geom_bar(stat = "identity")

#TODO: filter out movies with rating =< 2.5 & >=9


# rating, release date color scale over runtime
ggplot(tidy_movies, aes(x = release_date, y = rating)) +
  geom_point(aes(colour = runtime)) +
  scale_color_gradient(low = 'yellow', high = 'red') +
  xlab('release date') +
  ggtitle("rating, release date scale over runtime")

# TODO: y: runtime vs x: release date, color scale over rating

# ggplotly(plot)

# genre
head(tidy_movies$genres,1)

# convert and gather json
movies_by_genre <- tidy_movies %>% 
  as.tbl_json(json.column = "genres") %>% 
  gather_array() %>% 
  spread_values(genre=jstring('name'))
head(movies_by_genre$genre,3)

# TODO boxplot for rating vs genre


# get frequecy by gener
genre_frequencies <- movies_by_genre %>%
  group_by(genre) %>% 
  summarize(count=n())

#TODO bar plot for count vs genre
ggplot(genre_frequencies, aes(genre,count)) +
  geom_bar(stat = 'identity')

#get rating of action movies over year
action_movies <- subset(movies_by_genre, genre = 'Action') 
g_action_movies_plot <- ggplot(action_movies, aes(release_date, rating))
g_action_movies_plot + 
  geom_point() +
  geom_smooth(method = 'auto')
movies_by_genre_sub <- filter(movies_by_genre, genre == 'Action' | genre == 'Science Fiction' | genre == 'Animation')
ggplot(movies_by_genre_sub, aes(release_date, budget)) +
  geom_point(aes(color = genre)) +
  geom_smooth() +
  facet_grid(genre~.)

# To do: plot the relation of popularity and rating
# To do: plot the title of the most popular movie
# To do: plot the relation of popularity and rating of action and drama movies
#        and show in facet