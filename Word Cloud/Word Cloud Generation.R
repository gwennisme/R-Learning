### R-Learning - Generate Word Clouds ###
### Author: Gwendoline Tan (@gwennisme) ###
### Data Source: Makeover Monday Week 35 ###
### Wordcloud Learning Reference ###
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

# Load the libraries required
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(dplyr)

##### Retrieve data for processing #####

# Load the text file
raw_data <- read.csv(file="Wearables-DFE.csv", header=TRUE)

# Concatenate all the text
processed_data <- raw_data %>% group_by(Body.Location) %>% summarise(FullText = paste(Text, collapse = " "))

# Get arms data only
arms_data <- processed_data %>% filter(Body.Location == "Arms") %>% select(FullText)

##### Load/Clean the data #####

# Load the data as a corpus & inspect the loaded corpus
docs <- Corpus(VectorSource(arms_data[1]))
inspect(docs)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove common stopwords (english)
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Remove extra white space
docs <- tm_map(docs, stripWhitespace)

##### Build a term-document matrix #####

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word=names(v), freq=v)

##### Build a word cloud #####

wordcloud(words=d$word, freq=d$freq, min.freq = 1, random.order = FALSE, colors=brewer.pal(8, "Dark2"), scale=c(2.5, 0.45), rot.per = 0.35)
