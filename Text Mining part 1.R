library(data.table)
data <- fread("Desktop/Textbooks/psychcentral_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?")) 
data 

summary(data)
str(data)
colnames(data)

#colnames(health)[colnames(health)=='row'] <-'health_id'


#tokenize column q_content and remove stop words
library(dplyr)
library(tidytext)

#processing and prepping the data for text analysis
cleandata <- health %>%
  unnest_tokens(word, q_content)
cleandata[1:20]

#########4
#remove stop words
data(stop_words) 
cleandata <- cleandata %>%
  anti_join(stop_words)

#4.1 top 5 tokens returned
cleandata %>%
  count(word, sort = TRUE)

#4.2 use plot that shows frequency of the tokens that appeared for at least 2000
library(ggplot2)
cleandata %>%
  count(word, sort = TRUE) %>%
  filter(n > 2000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#4.4 Use package snowballC to stem q_content
library(SnowballC)
cleandata <- health %>%
  unnest_tokens(word, q_content) %>%
  mutate(word = wordStem(word)) 

#4.4.1 remove stopwords and find top 5 tokens after stemming
data(stop_words) 
cleandata <- cleandata %>%
  anti_join(stop_words)

cleandata %>%
  count(word, sort = TRUE)
#4.4.2 use plot that shows frequency of the tokens that appeared for at least 4000
cleandata %>%
  count(word, sort = TRUE) %>%
  filter(n > 4000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#4.4.3 use wordcloud with 200 most used tokens
library(wordcloud)
cleandata %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))

#4.4.4 create a color coded word cloud based on sentiment use most frequent 100 tokens for positve and negative words
library(reshape2)
cleandata %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)


#5.1 top 5 tokens returned
data(stop_words) 
cleandata <- cleandata %>%
  anti_join(stop_words)

cleandata <- health %>%
  unnest_tokens(word, answers)
cleandata[1:20]

cleandata %>%
  count(word, sort = TRUE)

#5.2 use plot that shows frequency of the tokens that appeared for at least 2000
library(ggplot2)
cleandata %>%
  count(word, sort = TRUE) %>%
  filter(n > 4000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#5.3 Use package snowballC to stem q_content
library(SnowballC)
cleandata <- health %>%
  unnest_tokens(word, answers) %>%
  mutate(word = wordStem(word)) 

#5.3.1 remove stopwords and find top 5 tokens after stemming
data(stop_words) 
cleandata <- cleandata %>%
  anti_join(stop_words)

cleandata %>%
  count(word, sort = TRUE)
#6.2.2 use plot that shows frequency of the tokens that appeared for at least 4000
cleandata %>%
  count(word, sort = TRUE) %>%
  filter(n > 6000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#6.2.3 use wordcloud with 200 most used tokens
library(wordcloud)
tidy_text %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))



#7 topic modeling on q_content
install.packages('rtext')
library(rtext)
library(tm)
library(wordcloud)
library(topicmodels)
library(slam)
data <- data[1:1000,] # We perform LDA on the rows 1 through 1000 in the data.
corpus <- Corpus(VectorSource(data$q_content), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words
lda <- LDA(dtm.new, k = 10) # k is the number of topics to be found.
lda


library(ldatuning)
lda_td <-tidy(lda)
lda_td


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





