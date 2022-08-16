### Data Extraction ### 

## Purpsose : 
## 1. Collect a sample of tweets containing hashtags in support of Farmers Protests. 
## 2. Subsample '#Farmers Protest' from a random sample of users in 1. 
## 3. Produce basic descriptive plots 

## Create Working Directory ##

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
rm(list = ls())

## Load in all libraries ##

library(academictwitteR)
library(dplyr)
library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(purrr)
library(tidyr)
library(data.table)

## 1. Initial sample of tweets in support of protests ## 

# Create a Function for Daily Extraction of tweets 

collect_tweets_daily <- function(query = supportquery, 
                                 start_date = '2020-09-01', 
                                 end_date = '2021-10-01',
                                 total_limit = 250000, 
                                 file = 'support_query/final_', 
                                 lang = NULL, 
                                 is_retweet = F){
  
  dates <- seq(as.Date(start_date), as.Date(end_date), by = 'day')
  n <- floor(total_limit/length(dates))
  stopifnot(n >= 1) 
  
  print(paste0('Total tweets collected per day : ', n))
  
  tweets = list(NA)
  
  for (i in 1:(length(dates)-1)){
    
    start = paste0(dates[i], 'T00:00:00Z')
    end = paste0(dates[i + 1], 'T00:00:00Z')
    
    print(start)
    print(end)
    
    
    get_all_tweets(
      query = query,
      is_retweet = is_retweet,
      start_tweets = start,
      end_tweets = end,
      lang = lang, 
      data_path = paste0(file, substr(start , 1, 10), '/'),
      bind_tweets = F,
      page_n = 500,
      n = n)
  }
}

# Execute the query 

supportquery <- c('#Istandwithfarmers', '#IStandWithFarmers', 
                  '#TakeBackFarmLaws')

collect_tweets_daily(total_limit = 2000000, 
                     lang = 'en', 
                     file = 'support_query/final_', 
                     is_retweet = NULL)

# Create a function for binding the tweets 

bind_data <- function(file = 'init_', 
                      start_date = '2020-09-01', 
                      end_date = '2021-12-01', 
                      by = 'month', 
                      users = F){
  
  if(by == 'month'){
    dates <- seq(as.Date(start_date), as.Date(end_date), by = 'month')
  } else if (by == 'day'){
    dates <- seq(as.Date(start_date), as.Date(end_date), by = 'day')
  }
  
  tweets <- list(NA)
  
  for (i in 1 : 1:length(dates)){
    
    date = dates[i]
    print(date)
    
    tweets[[i]] <- bind_tweets(data_path = paste0(file, date, '/'), 
                               user = users)
  }
  
  tweets <- dplyr::bind_rows(tweets, .id = 'labels')
  
  return(tweets)
}

# Bind the tweet - level and user - level data 

support_tweets <- bind_data(file = 'support_query/final_',
                    start_date = '2020-09-01', 
                    end_date = '2021-09-30', 
                    by = 'day')


support_usernames <- bind_data(file = 'support_query/final_',
                       start_date = '2020-09-01', 
                       end_date = '2021-09-30', 
                       by = 'day',
                       users = T)

## 2. Extract tweets containing hashtag '#FarmersProtest' from 25% users in 1 ##

# User IDs 

unique_users <- unique(usernames$username)

# Randomly Sample 25% users 

users_ind <- sample(1:length(unique_users), 
                    floor(0.25*length(unique_users)), 
                    replace = F) # Randomly sample 10% of users 

length(users_ind) # 26501 users

users <- unique_users[users_ind]

# Split the users vector into batches of 30, otherwise the query gets too long 

n <- length(users)
k <- 30 

vec_to_split <- 1 : n 
users_split <- list(NA)
j = 1


for (i in seq(0,n-k,k)) {
  split[j] <- vec[(i+1):(i+k)]
  j = j + 1
}

# Iterate over the batches to get tweets 

lapply(1:length(users_split), function(x){
  
  print(x)
  batch <- users_split[[x]]
  
  get_all_tweets(
    query = '#FarmersProtest',
    users = users_all[batch],
    start_tweets = '2020-09-01T00:00:00Z',
    end_tweets = '2021-10-01T00:00:00Z',
    lang = 'en', 
    data_path = paste0('users_tweets/batch_', x),
    bind_tweets = F,
    n = 1000, 
    verbose = T)
})

# Bind user tweets 

users_tweets <- bind_tweets(data_path = 'users_tweets/')

table(users_tweets$lang)
users_tweets$created_at <- as.Date(users_tweets$created_at)
range(users_tweets$created_at)

# Bind all tweets together 

all_tweets <- bind_rows(support_tweets, users_tweets)


### 3. Save all files into machine ###

saveRDS(support_tweets, 'tweets/support_tweets.rds')
saveRDS(usernames, 'tweets/support_usernames.rds')
saveRDS(users_tweets, 'tweets/users_tweets.rds')
saveRDS(usernames, 'tweets/user_tweets_usernames.rds')
saveRDS(all_tweets, 'tweets/all_tweets.rds')


## 4. Describe data ##


# Random sample of 5 tweets for display 

original_support_tweets <- support_tweets |> 
  as_tibble() |>
  filter(lengths(referenced_tweets) == 0) 

nrow(original_support_tweets) # 35656

ran <- sample(1:nrow(original_support_tweets), 5)

display <- gsub('\n', ' ', original_support_tweets$text[ran])

write.csv(display, 'csv/support_text.csv')

                
# Number of tweets 

nrow(support_tweets) # 264771
nrow(users_tweets) # 813551
nrow(all_tweets) # 1078322 tweets in total 


# Number of unique users 

length(unique(all_tweets$author_id)) # 95157 unique users 

# Unique tweets 

distinct_all_tweets <- all_tweets |> 
  distinct(id, .keep_all = T)

nrow(distinct_all_tweets) # 1065112
length(unique(all_tweets$author_id)) # 95157

# Find out referenced tweet IDs for tweets that have been re-tweeted 
  
distinct_all_tweets <- distinct_all_tweets |> # Un-nest re-tweets
  as_tibble() |>
  filter(lengths(referenced_tweets) > 0) |>
  unnest(referenced_tweets, 
         names_repair = 'universal', 
         keep_empty = T, 
         names_sep = '_')

distinct_all_tweets <- distinct_all_tweets |> 
  left_join(distinct_all_tweets[, c('referenced_tweets_id', 'id', 'text', 'author_id')], 
            by = c('referenced_tweets_id' = 'id'))

colnames(distinct_all_tweets)[colnames(distinct_all_tweets) == 'author_id.y'] = 'retweet_author_id'
colnames(distinct_all_tweets)[colnames(distinct_all_tweets) == 'author_id.x'] = 'author_id'
colnames(distinct_all_tweets)[colnames(distinct_all_tweets) == 'text.y'] = 'retweeted_text'

saveRDS(distinct_all_tweets, 'tweets/all_tweets.rds')

# Unique tweets (excluding re-tweets)

all_tweets |> 
  filter(referenced_tweets_type != 'retweeted') |> 
  summarize(n = n()) # 79718 non re-tweets





