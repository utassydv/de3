library(tidyverse)
library(rtweet)
library(ggplot2)
library(glue)
library(data.table) 
library(scales)
library(lubridate)

#################
#Keys for twitter API
appname <- f1_analysis
key <- #TODO: set these credentials
  secret_key <- 
  access_token <- 
  access_secret <- 
  
  #################
# Create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret_key,
  access_token = access_token,
  access_secret = access_secret)

#################
# Get twitter data
bahrein_gp <- search_tweets(q = "bahreingp -filter:retweets", n=18000, type='mixed', lang="en", until='2020-11-30', retryonratelimit = TRUE) 

#Remove unnecessary columns
keep_cols <- c("created_at", "text", "source", "is_retweet", "retweet_count","quote_count")
bahrein_gp_simple <- bahrein_gp %>% select(keep_cols)

#################
#Build in cleaning
builtin_clean <- plain_tweets(bahrein_gp_simple)



#################
# Manual cleaning part

# Removing links
bahrein_gp_simple_clean <- bahrein_gp_simple
bahrein_gp_simple_clean$stripped_text <- gsub("http.*","",  bahrein_gp_simple$text)
bahrein_gp_simple_clean$stripped_text <- gsub("https.*","", bahrein_gp_simple_clean$stripped_text)

# Emoji removal
bahrein_gp_simple_clean$plain_tweet <- enc2native(bahrein_gp_simple_clean$stripped_text) # Covnert emojis to native encoding
bahrein_gp_simple_clean$plain_tweet <- gsub("<.*.>", "", bahrein_gp_simple_clean$plain_tweet)

# Remove leading whitespaces from the beginning
bahrein_gp_simple_clean$plain_tweet <- trimws(bahrein_gp_simple_clean$plain_tweet)

# Get rid of text an stripped text columns
bahrein_gp_simple_clean <- bahrein_gp_simple_clean %>% 
  select(-text, -stripped_text)

# We also need to get rid of empty tweets!
bahrein_gp_simple_clean <- bahrein_gp_simple_clean %>% 
  filter(nchar(plain_tweet)!=0)



############################################
#Writing out clean data for a back-up
write.csv(bahrein_gp_simple_clean, '/Users/utassydv/Documents/workspaces/CEU/my_repos/de3/hw3/data/clean/bahrein_simple_clean.csv')
write.csv(builtin_clean, '/Users/utassydv/Documents/workspaces/CEU/my_repos/de3/hw3/data/clean/bahreing_built_in_clean.csv')



############################################
#Reading in as a backup
bahrein_gp_builtin <- read_csv('/Users/utassydv/Documents/workspaces/CEU/my_repos/de3/hw3/data/clean/bahreing_built_in_clean.csv')
bahrein_gp_manual <- read_csv('/Users/utassydv/Documents/workspaces/CEU/my_repos/de3/hw3/data/clean/bahrein_simple_clean.csv')
bahrein_gp_manual <-  bahrein_gp_manual %>% 
  rename(
    text = plain_tweet
  )




##################################################################################################################################
##################################################################################################################################
#AWS Amazon Comprehend

# latest stable version
install.packages("aws.comprehend", repos = c(cloudyr = "http://cloudyr.github.io/drat", getOption("repos")))

keyTable <- read.csv("accessKeys.csv", header = T) # accessKeys.csv == the CSV downloaded from AWS containing your Acces & Secret keys
AWS_ACCESS_KEY_ID <- as.character(keyTable$Access.key.ID)
AWS_SECRET_ACCESS_KEY <- as.character(keyTable$Secret.access.key)

#activate
Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = "eu-west-1") 

library("aws.comprehend")

###############################
#Getting the sentiment of all the tweets

#######
#with built in clean
sentiment <- function(row, df) {
  # Extraxt a specific row
  record <- df[row,]
  # Do the sentiment with Amazon's Comprehemd
  sentiment <- detect_sentiment(as.character(record$text))
  # Merge the sentiment result to the original data
  merged <- merge(sentiment, record)
  #print(merged)
  print(row)
  return (merged)
}

row_seq <- seq(1,nrow(bahrein_gp_builtin)) # Define argument for function, nrow(dataframe)

sentiment_built_in <- lapply(row_seq, sentiment, df=bahrein_gp_builtin)
sentiment_built_in <- do.call(rbind.data.frame, sentiment_built_in)

sentiment_built_in<-sentiment_built_in[day(sentiment_built_in$created_at)=="29",]

trends <- sentiment_built_in %>% 
  group_by(Sentiment) %>% 
  summarise(count = n())

write.csv(sentiment_built_in, '/Users/utassydv/Documents/workspaces/CEU/my_repos/de3/hw3/data/clean/bahrein_gp_sentiments_built_in.csv')

#######
#with manual clean

row_seq <- seq(1,nrow(bahrein_gp_manual)) # Define argument for function, nrow(dataframe)

sentiment_manual <- lapply(row_seq, sentiment, df=bahrein_gp_manual)
sentiment_manual <- do.call(rbind.data.frame, sentiment_manual)

sentiment_manual<-sentiment_manual[day(sentiment_manual$created_at)=="29",]

trends <- sentiment_manual %>% 
  group_by(Sentiment) %>% 
  summarise(count = n())

write.csv(sentiment_manual, '/Users/utassydv/Documents/workspaces/CEU/my_repos/de3/hw3/data/clean/bahrein_gp_sentiments_manual.csv')