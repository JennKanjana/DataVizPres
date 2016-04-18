#server.R
library(shiny)
library(wordcloud)
library(RCurl)
library(streamR)
library(ROAuth)
library(tm)


# Define server logic required to draw a barplot
shinyServer(function(input, output) {
  
  candidates <- c("Hillary Clinton", "Bernie Sanders",
                  "Ted Cruz", "Donald Trump")
  

  library(stringi)

  
  BS$text <- sapply(BS$text, function(row) iconv(row, "latin1", "ASCII", sub=""))
  DT$text <- sapply(DT$text, function(row) iconv(row, "latin1", "ASCII", sub=""))
  HC$text <- sapply(HC$text, function(row) iconv(row, "latin1", "ASCII", sub=""))
  TC$text <- sapply(TC$text, function(row) iconv(row, "latin1", "ASCII", sub=""))
  
  
  TweetCorpus <- paste(unlist(HC$text), collapse =" ") 
  TweetCorpus["Hillary Clinton"] <- paste(unlist(HC$text), collapse =" ")
  TweetCorpus["Bernie Sanders"] <- paste(unlist(BS$text), collapse =" ") 
  TweetCorpus["Ted Cruz"] <- paste(unlist(TC$text), collapse =" ") 
  TweetCorpus["Donald Trump"] <- paste(unlist(DT$text), collapse =" ")
  
  TweetCorpus <- Corpus(VectorSource(TweetCorpus))
  TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)
  TweetCorpus <- tm_map(TweetCorpus, removePunctuation)
  TweetCorpus <- tm_map(TweetCorpus, removeWords, stopwords('english'))
  TweetCorpus <- tm_map(TweetCorpus, content_transformer(tolower),lazy=TRUE)
  TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)

  
  output$wordcloud <- renderPlot({
    wordcloud(TweetCorpus[input$candidates], max.words = 150, random.order = FALSE,scale=c(8,1),colors=brewer.pal(8, "Spectral"))
  })
  
})


library(shiny)

#ui.R
shinyUI(fluidPage(
  titlePanel("Tweets WordCloud for Candidates"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose specific candidate, visualize the WordCloud tweets"),
      
      checkboxGroupInput("candidates", label = h3("Candidates"), 
                         choices = c("Hillary Clinton", "Bernie Sanders",
                                     "Ted Cruz", "Donald Trump"),
                         selected = "Hillary Clinton")
      
    ), 
    
    mainPanel(
     
      plotOutput("wordcloud")
    )
  )
))

