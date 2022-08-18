### Network Construction ### 

## Purpose : 
## 1. Create a retweet network between all members of the tweet corpus 
## 2. Find out the basic characteristics of the retweet network 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
rm(list = ls())

## Load in all libraries ## 

library(dplyr)
library(data.table)
library(igraph)
library(ggplot2)

## Load in the data ## 

# all_tweets <- readRDS('tweets/all_tweets.rds')

## 1. Create a re-tweet network between all members of the tweet corpus ## 

# Create an edge list 

edges_retweets <- all_tweets %>% 
  select(author_id, retweet_author_id) %>% 
  filter(!is.na(retweet_author_id)) %>% 
  group_by(author_id, retweet_author_id) %>% 
  summarise(count = n())

# Create retweet network

retweet_network <- graph_from_data_frame(edges_retweets[, c(1,2)], directed = T)
E(retweet_network)$weight <- edges_retweets$count
summary(retweet_network)

saveRDS(edges_retweets, 'edge_lists/edges_retweets.rds')

## 2. Basic Characteristics of network 

# Summary characteristics 

summary(retweet_network) 
no_nodes <- gorder(retweet_network) # 19055
no_edges <- gsize(retweet_network) # 41701


weighted_indeg <- graph.strength(retweet_network, mode = 'in')
weighted_outdeg <- graph.strength(retweet_network, mode = 'out')
nonweighted_indeg <- degree(retweet_network, mode = 'in')
nonweighted_outdeg <- degree(retweet_network, mode = 'out')

max_retweets_received <- max(weighted_indeg) 
min_retweets_received <- min(weighted_indeg) 

max_retweets_received # 4601
min_retweets_received # 0

max_retweets_sent <- max(weighted_outdeg) 
min_retweets_sent <- min(weighted_outdeg) 

max_retweets_sent # 5541
min_retweets_sent # 0

max_retweets_connections <- max(nonweighted_indeg)
min_retweets_connections <- min(nonweighted_outdeg)

max_retweets_connections # 1304
min_retweets_connections # 0


network_stats <- t(data.frame(no_nodes, 
                              no_edges, 
                              max_retweets_received, 
                              max_retweets_sent, 
                              max_retweets_connections))

write.csv(network_stats, 'csv/network_stats.csv')

# Degree distribution 

color = ('Mean line' = 'red')

agg_retweets_received <- edges_retweets |> 
  group_by(retweet_author_id) |> 
  summarize(sum = sum(count)) |> 
  arrange(-sum) |> 
  filter(sum > 10) |>
  mutate(log_sum = log(sum)) |>
  ggplot() + 
  geom_density(aes(log_sum), 
               adjust = 0.5, 
               color = '#469B8F') + 
  geom_vline(aes(xintercept = log(mean(sum)), 
             color = 'Mean line'), 
             linetype = 'dashed') +
  xlab('Log of Aggregated retweets received') + 
  scale_color_manual(values = color) +
  theme_bw() + 
  theme(legend.title = element_blank())

agg_retweets_received

ggsave('plots/Retweet Network/agg_retweets_received.png', 
       agg_retweets_received, 
       width = 4, 
       height = 4)




# Number of tweets containing user id

ids <- unique(c(unique(edges_retweets$author_id), 
                unique(edges_retweets$retweet_author_id)))

all_tweets |> 
  filter(author_id %in% ids) |> 
  summarise(n = n()) # 813083 tweets



