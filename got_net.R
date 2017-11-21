library(tidyverse)
library(visNetwork)

got_nodes <- read_csv('data/data_raw/GoT/GoT_nodes.csv')
got_edges <- read_csv('data/data_raw/GoT/GoT_edges.csv')

# process edges and nodes
got_edges <- got_edges %>% rename(from = source, to = target)
got_nodes <- got_nodes %>% add_column(image = '', shape = 'image')

# add icons
got_nodes[got_nodes$name == 'Jon Snow', ]$image <- 
  'https://pbs.twimg.com/profile_images/3456602315/aad436e6fab77ef4098c7a5b86cac8e3.jpeg' 
got_nodes[got_nodes$name == 'Daenerys Targaryen', ]$image <- 
  'https://pbs.twimg.com/profile_images/523131665145536512/pgdsesL3.jpeg' 

visNetwork(got_nodes,
           got_edges,
           width = '100%') %>%
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visIgraphLayout() %>%
  visOptions(highlightNearest = TRUE)
