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

## 1. Create a retweet network between all members of the tweet corpus ## 

# Create an edge list 

edges_retweets <- all_tweets %>% 
  select(author_id, retweet_author_id) %>% 
  filter(!is.na(retweet_author_id)) %>% 
  group_by(author_id, retweet_author_id) %>% 
  summarise(count = n())

retweet_network <- graph_from_data_frame(edges_retweets[, c(1,2)], directed = T)
E(retweet_network)$weight <- edge_df$count
summary(retweet_network)

saveRDS(retweet_network, 'networks/retweet_network.rds')
saveRDS(edges_retweets, 'edge_lists/edges_retweets.rds')

## 2. Basic Characteristics of network 

# Re-tweet distribution

table(edges_retweets$count)
hist(log(edge_df$count))

# Total number of tweets considered for further analysis 

ids <- unique(c(unique(edges_retweets$author_id), 
                unique(edges_retweets$retweet_author_id)))
length(ids)

all_tweets |> 
  filter(author_id %in% ids) |> 
  summarise(n = n()) # 813083 tweets

