
library(base)
# package to download tweets
library(rtweet)
# plotting and pipes - tidyverse not tidytext
library(ggplot2)
#library(dplyr)
# plotting packages
library(igraph)
library(ggraph)
# Packages for sentiment Analysis
library(sentimentr)
library(SentimentAnalysis)




#### Loading tweets for a certain period for the topic Election



pres_tweets3<- search_tweets(q = "Election", n = 15000,lang = "en",include_rts = FALSE, retryonratelimit = TRUE)


#### First 15 tweets


pres_tweets3$text[1:15]


## Preprocessing:

#### Remove http and https from tweets


pres_tweets3$stripped_text <- gsub("http.*","",  pres_tweets3$text)
pres_tweets3$stripped_text <- gsub("https.*","", pres_tweets3$stripped_text)


#### Convert tweets to lower case


pres_tweets3$stripped_text=tolower(pres_tweets3$stripped_text)


#### Analyze the tweets. This includes removing stopwords and stemming the words



pres_tweets3$sent <- analyzeSentiment(pres_tweets3$stripped_text, language = "english",aggregate = NULL, removeStopwords = TRUE, stemming = TRUE)




#### Get a binary response that is if tweet is either positive or negative


pres_tweets3$posneg<-convertToBinaryResponse(pres_tweets3$sent)$SentimentGI


##### Create the label position

pres_tweets3$sent$label_sentiment <- pres_tweets3$sent$SentimentGI +0.015


#### Plot the total number of positives and negatives over all tweets


plot(pres_tweets3$posneg, xlab="Negatives and Positives", ylab="Count", main="Total positives and negatives for 18000 tweets on Election", col = c("red", "blue"))


#### Adding plot label positions for points


pres_tweets3$sent$label_sentiment <- pres_tweets3$sent$SentimentGI +0.015


#### Plot for 100 tweets


plot(pres_tweets3$sent$SentimentGI[1:100], y =NULL, type = "o", xlab = "Tweet index", ylab = "Negative to Positive",main = " Positive Negative plots for 100 tweets fro the topic Election")
text(x=pres_tweets3$sent$label_sentiment[1:100],y=
       NULL,labels=pres_tweets3$posneg[1:100])



#### Plot for 18000 tweets


plot(pres_tweets3$sent$SentimentGI, y =NULL, type = "o", xlab = "Tweet index", ylab = "Negative to Positive",main = " Positive Negative plots for 18000 tweets on Election")
text(x=pres_tweets3$sent$label_sentiment,y=
       NULL,labels=pres_tweets3$posneg)


### Plotting sentiment values across tweets as a histogram



hist(pres_tweets3$sent$SentimentGI, xlab ="Sentiment Value" , ylab = "Tweet Count", main = "Sentiment value across tweets",col=("mediumpurple1"))

#### Split text of all tweets by sentences


split_text3 <- get_sentences(pres_tweets3$text)



#### Get emotion for overall tweets using the sentences we got above


(emo3 <- emotion(split_text3))


#### Drop unused emotions


emotion(split_text3, drop.unused.emotions = TRUE)


#### Plot emotions over all tweets. We notice a difference from the previous set of tweets



plot(emo3)



#### Plotting with unused emotions being dropped (We notice there is not a major difference)



plot(emo3, drop.unused.emotions = FALSE)



#### Plot of all emotions together



plot(emo3, facet = FALSE)



#### Plot of all emotions, negated emotions are in a seperate graph


plot(emo3, facet = 'negated')

### Aggregating the sentiment of tweets with that of other topics

#### Downloading tweets of election and president for comparison

election_comp1=search_tweets(q='election',n=1000,lang='en',include_rts = FALSE)
pres_comp1=search_tweets(q='president',n=1000,lang='en',include_rts = FALSE)


#### Preprocessing for topic 1


election_comp1$stripped_text=gsub('[[:punct:]]','',election_comp1$text)
election_comp1$stripped_text=gsub('[[:cntrl:]]','',election_comp1$stripped_text)
election_comp1$stripped_text=gsub('\\d+','',election_comp1$stripped_text)
election_comp1$stripped_text=tolower(election_comp1$stripped_text)


#### Preprocessing for topic 2


pres_comp1$stripped_text=gsub('[[:punct:]]','',pres_comp1$text)
pres_comp1$stripped_text=gsub('[[:cntrl:]]','',pres_comp1$stripped_text)
pres_comp1$stripped_text=gsub('\\d+','',pres_comp1$stripped_text)
pres_comp1$stripped_text=tolower(pres_comp1$stripped_text)


#### Get the sentiment for tweet 1


electionc_sent1=sentiment_by(election_comp1$stripped_text)
electionc_sent1


#### Getting the summary of the sentiments of the first topic


summary(electionc_sent1)


presc_sent1=sentiment_by(pres_comp1$stripped_text)
presc_sent1


#### Getting the summary of the emotions of the second topic


summary(presc_sent1)


#### Plot sentiments for the topic Election


qplot(electionc_sent1$ave_sentiment, xlab = "Sentiment range" , ylab = "Index", geom="histogram",binwidth=0.1,main="Election Sentiment Histogram")




#### Plot sentiment for the topic President


qplot(presc_sent1$ave_sentiment, xlab = "Sentiment range", ylab = "Index" ,  geom="histogram",binwidth=0.1,main="President Sentiment Histogram")


#### Side by side plotfor comparison of the sentiments


par(mfrow=c(1,1))
qplot(electionc_sent1$ave_sentiment, xlab = "Sentiment range" , ylab = "Index", geom="histogram",binwidth=0.1,main="Election Sentiment Histogram")
qplot(presc_sent1$ave_sentiment, xlab = "Sentiment range", ylab = "Index" ,  geom="histogram",binwidth=0.1,main="President Sentiment Histogram")



## A sentiment comparison between Airbus and Boeing 


#### Loading the tweets from Twitter


tweets_airbus=search_tweets(q='airbus',n=1000,lang='en',include_rts = FALSE)
tweets_boeing=search_tweets(q='boeing',n=1000,lang='en',include_rts = FALSE)



#### Preprocessing the tweets

tweets_airbus$stripped_text=gsub('[[:punct:]]','',tweets_airbus$text)
tweets_airbus$stripped_text=gsub('[[:cntrl:]]','',tweets_airbus$stripped_text)
tweets_airbus$stripped_text=gsub('\\d+','',tweets_airbus$stripped_text)
tweets_airbus$stripped_text=tolower(tweets_airbus$stripped_text)
tweets_boeing$stripped_text=gsub('[[:punct:]]','',tweets_boeing$text)
tweets_boeing$stripped_text=gsub('[[:cntrl:]]','',tweets_boeing$stripped_text)
tweets_boeing$stripped_text=gsub('\\d+','',tweets_boeing$stripped_text)
tweets_boeing$stripped_text=tolower(tweets_boeing$stripped_text)



#### Analysis of the tweets



sentiment_boeing=sentiment_by(tweets_boeing$stripped_text)
sentiment_boeing
sentiment_airbus=sentiment_by(tweets_airbus$stripped_text)
sentiment_airbus



#### Plotting the analysis

qplot(sentiment_boeing$ave_sentiment,   geom="histogram",binwidth=0.1,main="boeing Sentiment Histogram")
qplot(sentiment_airbus$ave_sentiment,   geom="histogram",binwidth=0.1,main="airbus Sentiment Histogram")


