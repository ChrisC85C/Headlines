install.packages("tidyverse")
install.packages("tm")
install.packages("NLP")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud2")
install.packages("caret")

library(tidyverse)
library(NLP)
library(tm)
library(SnowballC)
library(wordcloud2)
library(caret)

headlines <- read.csv("C:/Users/User/Documents/RStudioProjects/Scrape_The_Guardian/Data/Headlines_2000_2023.csv")
headlines <- headlines %>% 
  select(-X)

# Get all of the headlines from 2023
headlines_2023 <- headlines[headlines$Year=='2023',]
headlines_2023 <- headlines_2023$Headline

# Create a corpus of the headlines
corpus <- Corpus(VectorSource(headlines_2023))
corpus$content

# Remove numbers ###works
corpus_noNumbers <- tm_map(corpus, removeNumbers)
corpus_noNumbers$content

# Remove punctuation
corpus_noPunctuation <- tm_map(corpus_noNumbers, removePunctuation)
corpus_noPunctuation$content

# Strip whitespace
corpus_noWhitespace <- tm_map(corpus_noPunctuation, stripWhitespace)
corpus_noWhitespace$content

# Convert to lowercase
corpus_noCAPS <- tm_map(corpus_noWhitespace, content_transformer(tolower))
corpus_noCAPS$content

# Remove stop words
corpus_noStopWords <- tm_map(corpus_noCAPS, removeWords, stopwords("english"))
corpus_noStopWords$content




#### 1st filter ########################## EXTRACT
headlines_vector <- unlist(corpus_noStopWords$content)
headlines_vector
ukr_headlines <- grep("ukr", headlines_vector, value = TRUE)

ukr_headlines

library(syuzhet)

get_sentiment_scores <- function(ukr_headlines) {
  sentiment_UKR <- get_sentiment(ukr_headlines, method = "syuzhet")
  return(sentiment_UKR)
}

sentiment_scores <- sapply(ukr_headlines, get_sentiment_scores)

sentiment_df <- data.frame(headline=ukr_headlines, sentiment_score=sentiment_scores)

# Classify each headline as positive, negative, or neutral
sentiment_df <- sentiment_df %>%
  mutate(sentiment_category = case_when(
    sentiment_score > 0  ~ "positive",
    sentiment_score == 0 ~ "neutral",
    sentiment_score < 0  ~ "negative"
  ))

# View the results
print(sentiment_df)


hist(sentiment_df$sentiment_score, 
     main = "Histogram Sentiment Analysis of Headlines referring to Ukraine", 
     xlab = "Method used is Syuzhet",
     col = "#0d4eb8")  # Set y-axis limits to make sure the average value is visible
      
      # Calculate average sentiment score
      average_sentiment_score <- mean(sentiment_df$sentiment_score)
      
      # Add a vertical line at the average sentiment score
     abline(v = average_sentiment_score, col="#c40a4b", lty = 1, lwd=4)
     legend("topright", legend = c("Positive", "Neutral", "Negative"),
            col = c("#0d4eb8", "#ffffff", "#c40a4b"), lty = 1, lwd=4)



     
###################### 2019 to 2022 ###################################################
library(tm)
library(syuzhet)

# Create a function to analyze sentiment
analyze_sentiment_year <- function(year) {
# Get all of the headlines from the specified year
       headlines_year <- headlines[headlines$Year == year, ]
       headlines_year <- headlines_year$Headline
       
       # Create a corpus of the headlines
       corpus <- Corpus(VectorSource(headlines_year))
       
       # Preprocess the corpus
       corpus_noNumbers <- tm_map(corpus, removeNumbers)
       corpus_noPunctuation <- tm_map(corpus_noNumbers, removePunctuation)
       corpus_noWhitespace <- tm_map(corpus_noPunctuation, stripWhitespace)
       corpus_noCAPS <- tm_map(corpus_noWhitespace, content_transformer(tolower))
       corpus_noStopWords <- tm_map(corpus_noCAPS, removeWords, stopwords("english"))
       
       # Extract headlines containing "Ukr" or "ukrain"
       headlines_vector <- unlist(corpus_noStopWords$content)
       ukr_headlines <- grep("ukr|ukrain", headlines_vector, value = TRUE)
       
       # Get sentiment scores for Ukrainian headlines
       sentiment_scores <- sapply(ukr_headlines, get_sentiment, method = "syuzhet")
       
       # Create a data frame of sentiment scores and headlines
       sentiment_df <- data.frame(headline = ukr_headlines, sentiment_score = sentiment_scores)
       
       # Classify each headline as positive, negative, or neutral
       sentiment_df <- sentiment_df %>%
         mutate(sentiment_category = case_when(
           sentiment_score > 0 ~ "positive",
           sentiment_score == 0 ~ "neutral",
           sentiment_score < 0 ~ "negative"
         ))
       
       # Analyze sentiment and print results
       if (length(sentiment_df$headline) > 0) {
         # Create a histogram of sentiment scores
         hist(sentiment_df$sentiment_score,
              main = paste0("Histogram Sentiment Analysis of Headlines referring to Ukraine in ", year),
              xlab = "Method used is Syuzhet",
              col = "#0d4eb8")
         
         # Calculate average sentiment score
         average_sentiment_score <- mean(sentiment_df$sentiment_score)
         
         # Add a vertical line at the average sentiment score
         abline(v = average_sentiment_score, col="#c40a4b", lty = 1, lwd=4)
         
         # Print the sentiment data frame
         print(sentiment_df)
       } else {
         print(paste0("No headlines containing \"Ukr\" or \"ukrain\" found for year ", year))
       }
     }
     
     # Analyze sentiment for each year
     for (year in seq(2018, 2023)) {
       analyze_sentiment_year(year) }

