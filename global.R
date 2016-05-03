
load("tweets_with_topic.rda")

library(streamR)
library(tm)
library(wordcloud)
library(memoise)
library(ROAuth)

candidates <- list("Hillary Clinton" = "hillary", "Bernie Sanders" = "bernie", "Ted Cruz" = "ted", "Donald Trump" = "trump")


getTermMatrix <- memoise(function(candy) {
 
tweet_all$text <- sapply(tweet_all$text, function(row) iconv(row, "latin1", "ASCII", sub=""))
TweetCorpus <- paste(unlist(tweet_all$text), collapse =" ") #to get all of the tweets together
TweetCorpus <- Corpus(VectorSource(TweetCorpus))
TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)
TweetCorpus <- tm_map(TweetCorpus, removePunctuation)
TweetCorpus <- tm_map(TweetCorpus, removeWords, c(stopwords('english'), "amp", "https", "http", "httpst", "htt"))
TweetCorpus <- tm_map(TweetCorpus, content_transformer(tolower),lazy=TRUE)
TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)

myDTM = TermDocumentMatrix(TweetCorpus, control = list(minWordLength = 1))
m = as.matrix(myDTM)

sort(rowSums(m), decreasing = TRUE)
})

