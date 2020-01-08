
#################################################################################
#####  WordCloud function for text variables ####################################
#################################################################################

wordcloudfunc <- function(data,textvar){
  
  res.cloud <- list()
  
  docs <- Corpus(VectorSource(enc2utf8(as.character(na.omit(data[,paste(textvar)])))))
  
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) #function to replace a pattern to white space using regex
  docs <- tm_map(docs, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)") #match rt or via
  docs <- tm_map(docs, toSpace, "@\\w+") #match @
  docs <- tm_map(docs, toSpace, "[ \t]{2,}") #match tabs
  docs <- tm_map(docs, toSpace, "[ |\n]{1,}") #match new lines
  docs <- tm_map(docs, toSpace, "^ ") #match white space at begenning
  docs <- tm_map(docs, toSpace, " $") #match white space at the end
  docs <- tm_map(docs, PlainTextDocument)
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, toSpace, "http[[:alnum:]]*") #remove url from tweets
  docs <- tm_map(docs,removeWords,stopwords("english"))
  docs <- tm_map(docs, content_transformer(tolower))
  
  tdm.tfidf <- TermDocumentMatrix(docs, control = list(weighting =
                                                   function(x)
                                                     weightTfIdf(x, normalize = TRUE)))
                                                              
  m.tfidf <- as.matrix(tdm.tfidf)
  v.tfidf <- sort(rowSums(m.tfidf),decreasing=TRUE)
  
  tdm <- TermDocumentMatrix(docs)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
                                  
  
  d <- data.frame(Variable=paste(textvar),word.freq = names(v),freq=v,word.tfidf=names(v.tfidf),weight.tf.idf=v.tfidf)
  #head(d, 10)
  
  res.cloud$df <- d
  
  set.seed(1234)
  #wordcloud(words = d$word, freq = d$freq, min.freq = 1,
  #          max.words=200, random.order=FALSE, rot.per=0.35, 
  #          colors=brewer.pal(8, "Dark2"))
  
  #findFreqTerms(dtm, lowfreq = 4)
  
  #barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
  #        col ="lightblue", main ="Most frequent words",
  #        ylab = "Word frequencies")
  
  return(res.cloud)
}
