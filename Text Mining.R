library(data.table)
data = fread("Desktop/Textbooks/tweets_about_sprint.csv",
             strip.white = T, sep=",", header = T, na.strings = c("", " ", "NA", "nan", "NaN", "nannan"))

#creates a index seq. = seqeuence of integer numbers based on number of rows
data$tweet_id <- seq.int(nrow(data))
head(data , n=5)

library(tidytext)
library(dplyr)

library(tidyverse)

#create df with a vector of 4 items. each item is character time. items seperated by a comma
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text

library(tidyverse)
text_df <- data_frame(line = 1:4, text = text)
head(text_df)

#tibble is useful wnat to tokensize tibbles to text analysis.
#within a tibble want to unnest tokens, unnest is function for tokenization

text_df %>%
  unnest_tokens(word, text)#this means that in df text_df tokenize column 'text' by each word

#The function unnest_tokens above splits each row such that there is one token (word) in each row of the new data frame; Also notice:
#Other columns, such as the line number each word came from, are retained.
#Punctuation has been stripped.
#By default, unnest_tokens() converts the tokens to lowercase, which makes them easier to compare or combine with other datasets. 
#Use the to_lower = FALSE argument to avoid automatic lowercasing).

##### Start processsing our original Twitter data
tidy_text <- data %>%
  unnest_tokens(word, tweet) #tweet is the name of the column that had the actual text 
tidy_text[1:20] # this broke down to each word in the tweet.

tidy_text[1:40] #we had 26 actual words in the first tweet

#remove stop words from data using anti_join function 
#we remove stop words because they don't have specifc information that is useful for text analytics
#if we have a stop word will remove the word from the row
data(stop_words) #means stop_words dataset 
tidy_text <- tidy_text %>%  #overwriting previous tidy-text
  anti_join(stop_words) #anti-hoin means remove

count()

#can use dplyr's count to find the most common words
tidy_text %>%
  count(word, sort = TRUE)



#will get tibble size 38,767 which is how many words we have in our dataset *** want to do it before
#stop_words line of code (47-49) to see how many stop words we have.

#Use ggplot2 package to create visualization of the most common words
tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 1000) %>%   #filter n based on threshold. only display words mentioned over 1000 times
  #if want fewer words make the n value greater 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

tweet_words <- data %>%
  unnest_tokens(word, tweet) %>%
  count(user, word, sort = TRUE) %>% # It will count the number of words per user
  ungroup()
head(tweet_words)

total_words <- tweet_words %>% 
  group_by(user) %>% 
  summarize(total = sum(n)) # Counts the total number of words used by each user
head(total_words)

tweet_words <- left_join(tweet_words, total_words) 
head(tweet_words)

tweet_words <- tweet_words %>%
  bind_tf_idf(word, user, n)
head(tweet_words)

#The idea of tf-idf is to find the important words for the content of each document 
#(all of the tweets per user) by decreasing the weight for commonly used words and
#increasing the weight for words that are not used very much in a collection or corpus
#of documents, in this case, all of the tweets about Sprint. 
#Calculating tf-idf attempts to find the words that are important (i.e., common) in a text,
#but not too common.



#sentiment analysis or opinion mining
sentiments
#is actually a dictionary - looks at a phrase and sees what the negative/positive word ratio is
#lexicon - collection of positive/negative words 

sentiments %>% filter(word == 'shit')

#is want to get all the positive words
sentiments %>% filter(sentiment == 'positive')

#if dataset is coming froma a very specific domain(like finance) lexicon may not be useful
#Lexicon is made up of 4 lexicons afinn, bing, nrc, 
install.packages('textdata')
library(textdata)
get_sentiments('nrc')

nrcjoy <-get_sentiments ('nrc') %>% filter(sentiment =='joy')

#stemming
library(SnowballC) #package for stemming

tidy_text <-tidy_text %>%
  mutate(word = wordStem(word))


tidy_text %>%
  count(word, sort = TRUE)
tidytext %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

sentiment <- tidy_text %>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, sort = TRUE)
head(sentiment)
#### trump is considered postiive, lexicon is mentioning the adj not the president

#sentiment analysis on a specific tweet
sentiment <- tidy_text %>%
  filter(tweet_id ==2) %>% #only including tweets that have an id of 2
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, sort = TRUE)

head(sentiment)

#filter words by user
sentiment <- tidy_text %>%
  filter(user == "KeepMyCoat") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) 
head(sentiment)

#word clouds vizualization for data
install.packages('wordcloud')

library(wordcloud)

tidy_text %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


#want to recreate word cloud with most postive and negative words
install.packages('reshape2')
library(reshape2)

tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

#acast to the end is geared towards the visuzalization. don't change anything
#here for the assignment except for the maxwords

#topic modeling: idenfitying main topics in text data

install.packages('tm')
install.packages('topicmodels')
install.packages('slam')



library(tm)
library(topicmodels)
library(slam)

datat <- data[1:1000]

corpus <- Corpus(VectorSource(datat$tweet), readerControl=list(language="en"))
#specific form of text dat

tweet_dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 3, removeNumbers = TRUE, removePunctuation = TRUE))
tweet_dtm


#to determine the best value for k use ldatuning package
install.packages('ldatuning')
library(ldatuning)

result <- FindTopicsNumber(
  tweet_dtm,
  topics = seq(from = 2, to = 22, by = 4),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

#need to see where these plots even out/flatten out- these plots are visuzlaitions of 4 different ways of determining the best value of k
##method square triangle and circle says 10 is a good value for k



lda <- LDA(tweet_dtm, k = 10)
#lda name of the function of lda algorithm two inputs(df, number of topics we want to recover from our data)

#tidy up the df by using code down below
lda_td <- tidy(lda)
lda_td

#for visualizations do code down below
top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()





