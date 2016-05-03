load("Bernie.rda")
load("Donald.rda")
load("Hillary.rda")
load("Ted.rda")
load("tweets_with_topic.rda")
load("use_me_all_tweets.rda")


###############################################
### SENTIMENT ANALYSIS						###
###############################################

library(steamR)

# loading lexicon of positive and negative words (from Neal Caren)
lexicon <- read.csv("lexicon.csv", stringsAsFactors=F)
pos.words <- lexicon$word[lexicon$polarity=="positive"]
neg.words <- lexicon$word[lexicon$polarity=="negative"]

# function to clean the text
clean_tweets <- function(text){
  lapply(c("tm", "Rstem", "stringr"), require, c=T, q=T)
  utf8text <- iconv(text, to='UTF-8-MAC', sub = "byte")
  # remove punctuation and convert to lower case
  words <- removePunctuation(utf8text)
  words <- tolower(words)
  # spliting in words
  words <- str_split(words, " ")
  return(words)
}

##############################################################  
######                   Bernie First              ##########
############################################################## 
Bernie$text[1]
Bernie$text[7]

text <- clean_tweets(Bernie$text)

text[[1]]
text[[7]]

# a function to classify individual tweets
classify <- function(words, pos.words, neg.words){
  # count number of positive and negative word matches
  pos.matches <- sum(words %in% pos.words)
  neg.matches <- sum(words %in% neg.words)
  return(pos.matches - neg.matches)
}

# this is how we would apply it
classify(text[[1]], pos.words, neg.words)
classify(text[[7]], pos.words, neg.words)

# but we want to aggregate over many tweets...
classifier <- function(text, pos.words, neg.words){
  # classifier
  scores <- unlist(lapply(text, classify, pos.words, neg.words))
  n <- length(scores)
  Bernie$sentiment <- ifelse(scores > 0, 1, ifelse(scores < 0, -1, 0))
  save(Bernie,file = "BernieS.rda")
  positive <- as.integer(length(which(scores>0))/n*100)
  negative <- as.integer(length(which(scores<0))/n*100)
  neutral <- 100 - positive - negative
  cat(n, "tweets:", positive, "% positive,",
      negative, "% negative,", neutral, "% neutral")
}

# applying classifier function
classifier(text, pos.words, neg.words)


##############################################################  
######                  Hillary Later               ##########
############################################################## 
Hillary$text[1]
Hillary$text[7]

Htext <- clean_tweets(Hillary$text)

Htext[[1]]
Htext[[7]]

# a function to classify individual tweets
classify <- function(words, pos.words, neg.words){
  # count number of positive and negative word matches
  pos.matches <- sum(words %in% pos.words)
  neg.matches <- sum(words %in% neg.words)
  return(pos.matches - neg.matches)
}

# this is how we would apply it
classify(Htext[[1]], pos.words, neg.words)
classify(Htext[[7]], pos.words, neg.words)

# but we want to aggregate over many tweets...
classifier <- function(text, pos.words, neg.words){
  # classifier
  scores <- unlist(lapply(Htext, classify, pos.words, neg.words))
  n <- length(scores)
  Hillary$sentiment <- ifelse(scores > 0, 1, ifelse(scores < 0, -1, 0))
  save(Hillary,file = "HillaryS.rda")
  positive <- as.integer(length(which(scores>0))/n*100)
  negative <- as.integer(length(which(scores<0))/n*100)
  neutral <- 100 - positive - negative
  cat(n, "tweets:", positive, "% positive,",
      negative, "% negative,", neutral, "% neutral")
}

# applying classifier function
classifier(Htext, pos.words, neg.words)

##############################################################  
######                   Teddy Bear!!!             ##########
############################################################## 
Ted$text[1]
Ted$text[7]

Ttext <- clean_tweets(Ted$text)

Ttext[[1]]
Ttext[[7]]

# a function to classify individual tweets
classify <- function(words, pos.words, neg.words){
  # count number of positive and negative word matches
  pos.matches <- sum(words %in% pos.words)
  neg.matches <- sum(words %in% neg.words)
  return(pos.matches - neg.matches)
}

# this is how we would apply it
classify(Ttext[[1]], pos.words, neg.words)
classify(Ttext[[7]], pos.words, neg.words)

# but we want to aggregate over many tweets...
classifier <- function(Ttext, pos.words, neg.words){
  # classifier
  scores <- unlist(lapply(Ttext, classify, pos.words, neg.words))
  n <- length(scores)
  positive <- as.integer(length(which(scores>0))/n*100)
  negative <- as.integer(length(which(scores<0))/n*100)
  neutral <- 100 - positive - negative
  cat(n, "tweets:", positive, "% positive,",
      negative, "% negative,", neutral, "% neutral")
}

# applying classifier function
classifier(Ttext, pos.words, neg.words)

Ted$sentiment <- ifelse(scores > 0, 1, ifelse(scores < 0, -1, 0))
save(Ted,file = "TedS.rda")

##############################################################  
######                   Dump Dump                  ##########
############################################################## 
Donald$text[1]
Donald$text[7]

Dtext <- clean_tweets(Donald$text)

Dtext[[1]]
Dtext[[7]]

# a function to classify individual tweets
classify <- function(words, pos.words, neg.words){
  # count number of positive and negative word matches
  pos.matches <- sum(words %in% pos.words)
  neg.matches <- sum(words %in% neg.words)
  return(pos.matches - neg.matches)
}

# this is how we would apply it
classify(Dtext[[1]], pos.words, neg.words)
classify(Dtext[[7]], pos.words, neg.words)

# but we want to aggregate over many tweets...
classifier <- function(Dtext, pos.words, neg.words){
  # classifier
  scores <- unlist(lapply(Dtext, classify, pos.words, neg.words))
  n <- length(scores)
  positive <- as.integer(length(which(scores>0))/n*100)
  negative <- as.integer(length(which(scores<0))/n*100)
  neutral <- 100 - positive - negative
  cat(n, "tweets:", positive, "% positive,",
      negative, "% negative,", neutral, "% neutral")
}

# applying classifier function
classifier(Dtext, pos.words, neg.words)

Donald$sentiment <- ifelse(scores > 0, 1, ifelse(scores < 0, -1, 0))
save(Donald,file = "DonaldS.rda")

# ALL topic
tweet_all$text[1]
tweet_all$text[7]

ALLtext <- clean_tweets(tweet_all$text)

ALLtext[[1]]
ALLtext[[7]]

# a function to classify individual tweets
classify <- function(words, pos.words, neg.words){
  # count number of positive and negative word matches
  pos.matches <- sum(words %in% pos.words)
  neg.matches <- sum(words %in% neg.words)
  return(pos.matches - neg.matches)
}

# this is how we would apply it
classify(ALLtext[[1]], pos.words, neg.words)
classify(ALLtext[[7]], pos.words, neg.words)

# but we want to aggregate over many tweets...
classifier <- function(ALLtext, pos.words, neg.words){
  # classifier
  scores <- unlist(lapply(ALLtext, classify, pos.words, neg.words))
  n <- length(scores)
  positive <- as.integer(length(which(scores>0))/n*100)
  negative <- as.integer(length(which(scores<0))/n*100)
  neutral <- 100 - positive - negative
  cat(n, "tweets:", positive, "% positive,",
      negative, "% negative,", neutral, "% neutral")
}

# applying classifier function
classifier(ALLtext, pos.words, neg.words)
scores <- unlist(lapply(ALLtext, classify, pos.words, neg.words))
tweet_all$sentiment <- ifelse(scores > 0, 1, ifelse(scores < 0, -1, 0))
save(tweet_allfile = "tweet_allS.rda")

##############################################################  
######                  All with TOPICS             ##########
############################################################## 
tweet_all$text[1]
tweet_all$text[7]

ALLtext <- clean_tweets(tweet_all$text)

ALLtext[[1]]
ALLtext[[7]]

# a function to classify individual tweets
classify <- function(words, pos.words, neg.words){
  # count number of positive and negative word matches
  pos.matches <- sum(words %in% pos.words)
  neg.matches <- sum(words %in% neg.words)
  return(pos.matches - neg.matches)
}

# this is how we would apply it
classify(ALLtext[[1]], pos.words, neg.words)
classify(ALLtext[[7]], pos.words, neg.words)

# but we want to aggregate over many tweets...
classifier <- function(ALLtext, pos.words, neg.words){
  # classifier
  scores <- unlist(lapply(ALLtext, classify, pos.words, neg.words))
  n <- length(scores)
  positive <- as.integer(length(which(scores>0))/n*100)
  negative <- as.integer(length(which(scores<0))/n*100)
  neutral <- 100 - positive - negative
  cat(n, "tweets:", positive, "% positive,",
      negative, "% negative,", neutral, "% neutral")
}

# applying classifier function
classifier(ALLtext, pos.words, neg.words)
scores <- unlist(lapply(ALLtext, classify, pos.words, neg.words))
tweet_all$sentiment <- ifelse(scores > 0, 1, ifelse(scores < 0, -1, 0))
save(tweet_all,file = "tweet_all_with_topic_S.rda")

##############################################################  
######                   ALL ALL ALL                ##########
############################################################## 
all_tweets$text[1]
all_tweets$text[7]

alltext <- clean_tweets(all_tweets$text)

alltext[[1]]
alltext[[7]]

# a function to classify individual tweets
classify <- function(words, pos.words, neg.words){
  # count number of positive and negative word matches
  pos.matches <- sum(words %in% pos.words)
  neg.matches <- sum(words %in% neg.words)
  return(pos.matches - neg.matches)
}

# this is how we would apply it
classify(alltext[[1]], pos.words, neg.words)
classify(alltext[[7]], pos.words, neg.words)

# but we want to aggregate over many tweets...
classifier <- function(alltext, pos.words, neg.words){
  # classifier
  scores <- unlist(lapply(alltext, classify, pos.words, neg.words))
  n <- length(scores)
  positive <- as.integer(length(which(scores>0))/n*100)
  negative <- as.integer(length(which(scores<0))/n*100)
  neutral <- 100 - positive - negative
  cat(n, "tweets:", positive, "% positive,",
      negative, "% negative,", neutral, "% neutral")
}

# applying classifier function
classifier(alltext, pos.words, neg.words)
scores <- unlist(lapply(alltext, classify, pos.words, neg.words))
all_tweets$sentiment <- ifelse(scores > 0, 1, ifelse(scores < 0, -1, 0))
save(all_tweets, file = "tweet_ALL_S.rda")
