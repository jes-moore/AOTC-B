library(tm)
library(XML)
library(tm.plugin.webmining)
library(twitteR)
library(ROAuth) 
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

consumer_key <- 'BFcaBwqbYQ8PUMyqz9u7iAtMU'
consumer_secret <- 'zM8FPK8A0RiPfUTYSW7DtFOkOPl5Nd6hGACxRZCsjHIb9EB0WU'
access_token <- '3254129071-Z4Thwq9RyMArjwyUkVJJerVmnBVWgYgU6F6zQ5W'
access_secret <- '3CKJqz8lhyFnz1WTIiLu7FnsZTlvPPrFKbNanPmQXmEZr'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

copper_tweets <- searchTwitter("#Copper", n=1000) #get tweets
copper_text <- sapply(copper_tweets, function(x) x$getText()) #save text
copper_corpus<- Corpus(VectorSource(copper_text)) #create corpus

#clean up
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower)) 
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()))
wordcloud(r_stats_text_corpus)

#alternative steps if you're running into problems 
r_stats<- searchTwitter("#Rstats", n=1500)
#save text
r_stats_text <- sapply(r_stats, function(x) x$getText())
#create corpus
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))

#if you get the below error
#In mclapply(content(x), FUN, ...) :
#  all scheduled cores encountered errors in user code
#add mc.cores=1 into each function

#run this step if you get the error:
#(please break it!)' in 'utf8towcs'
r_stats_text_corpus <- tm_map(r_stats_text_corpus,
                              content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                              mc.cores=1
)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower), mc.cores=1)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation, mc.cores=1)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()), mc.cores=1)
wordcloud(r_stats_text_corpus)