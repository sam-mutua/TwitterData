library(rtweet)

library(httpuv)

library(ggplot2)

library(dplyr)

library(tidytext)

library(wordcloud)

library(scales) # percent function

library(lubridate)

# importing the data

Data <- read.csv("sentiment.csv")


# removing the unnecessary words and symbols

Data$tweet <-  gsub("https\\S*", "", Data$tweet)

Data$tweet <-  gsub("@\\S*", "", Data$tweet) 

Data$tweet  <-  gsub("amp", "", Data$tweet) 

Data$tweet  <-  gsub("[\r\n]", "", Data$tweet)

Data$tweet  <-  gsub("[[:punct:]]", "", Data$tweet)

tweets <- Data %>%
  select(tweet) %>%
  unnest_tokens(word, tweet)

my_stop_words <- tibble(
  word = c(
    "dont",
    "hon",
    "just",
    "time", 
    "this", 
    "coming", 
    "kenneth",
    "kuomba", 
    "county", 
    "show",
    "must",
    "can", 
    "via",
    "time",
    "the"
  ),
  lexicon = "twitter"
)

all_stop_words <- stop_words %>% bind_rows(my_stop_words)
tweets <- tweets %>%
  anti_join(all_stop_words)

Plot <- tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) 


pp <- wordcloud(Data$tweet, min.freq=200, scale=c(1.0, .1), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


library(syuzhet)

# Converting tweets to ASCII to trackle strange characters

tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")

# removing retweets, in case needed 

tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)

# removing mentions, in case needed

tweets <-gsub("@\\w+","",tweets)

ew_sentiment<-get_nrc_sentiment((tweets))

sentimentscores<-data.frame(colSums(ew_sentiment[,]))

names(sentimentscores) <- "Score"

sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)

rownames(sentimentscores) <- NULL

p4 <- ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity", show.legend = FALSE)+
  theme(legend.position="none",
        plot.title = element_text(colour = "blue", face = "bold"))+
  labs(x = "Sentiments", y = "Scores", title = "Total sentiment based on scores")+
  theme_minimal() 

p4


# total likes

sum(Data$num_of_likes)

# total retweets

sum(Data$num_of_retweet)


# source

Source <- Data %>% # gives you a bar chart of the most frequent words found in the tweets
  count(source, sort = TRUE) %>%
  top_n(5) %>%
  mutate(source = reorder(source, n)) 

ggplot(Source, aes(x=source, y = n, fill = source)) + geom_bar(show.legend = F, stat = "identity")+coord_flip()

(dims = dim(Source))

# doughnut

pie = ggplot(Source, aes(x = 2, y = n, fill = source)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y", start = 0) + 
  theme_void() + # these theme removes the lines around chart and grey background
  geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]),
                label = percent(n/5742)), # frequencies (size labels)
            color = "black", size = 4, hjust = 1.0) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(colour = "blue", face = "bold"),
        plot.caption = element_text(colour = 'green', face = "italic")) +
  labs(fill = "Source of Tweet", title = "Tweet by Source", caption = "Plot by @SamKMutua") +
  xlim(0.5, 2.5)
pie


# converting the date 

Data$Date_Column <- mdy(Data$Date_Column)


# number of tweets per day

Tw <- Data %>% 
  group_by(Date_Column) %>% 
  summarize(Count = n()) %>% 
  mutate(Date_Column = reorder(Date_Column, -Count)) 

day = ggplot(Tw, aes(x = Date_Column, y = Count, fill = Date_Column)) + geom_bar(stat = "identity", show.legend = FALSE) + theme(axis.text.x=element_text(angle=45, hjust=1),plot.title = element_text(colour = "red", face = "bold"), plot.subtitle = element_text(colour = "green", face = "bold.italic"), plot.caption = element_text(colour = "blue", face = "italic")) + labs(x = "Date", y = "Frequency", title = "Tweet by Day", subtitle = "Majority of the tweets were on 22.08.2022 the day the \n Azimio Coalition filed the petition", caption = "Plot by @SamKMutua")
day

gridExtra::grid.arrange(day, pie, p4, ncol = 2)
