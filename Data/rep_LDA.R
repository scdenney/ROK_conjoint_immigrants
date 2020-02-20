##### Code to replicate Latent Dirichlet allocation (LDA) analysis
## See Supplementary Information (Table SI.3) to:
## “Who Should be Admitted? Conjoint Analysis of South Korean Attitudes toward Immigrants”
## Paper by Steven Denney (University of Toronto) and Christophre Green (Leiden University)

## code by Steven Denney -- steven.denney@utoronto.ca


## load lda package and other required or relevant packages
library(lda)
library(tm)
library(topicmodels)
library(dplyr)
library(tidytext)
library(ggplot2)

## load data, see "rep_data3_LDA.csv"
# why <- read.csv()

## make a corpus

#tdm <- TermDocumentMatrix(Corpus(DataframeSource(why$X1)))
#myCorpus <- Corpus(VectorSource(why$X1))
#dtm <- TermDocumentMatrix(myCorpus)

#create corpus from vector
docs <- Corpus(VectorSource(why$X1))

#start preprocessing
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
#docs <- tm_map(docs, tolower)
myStopwords <- c(stopwords("english"),
                 c("because", "they", "and", "will", "think")) # consider others
docs <- tm_map(docs, removeWords, myStopwords)
docs <- tm_map(docs, stripWhitespace)

#Create document-term matrix
dtm <- DocumentTermMatrix(docs)

raw.sum=apply(dtm,1,FUN=sum) #sum by raw each raw of the table
dtm=dtm[raw.sum!=0,]

#convert rownames to filenames
rownames(dtm) <- filenames ##error

#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))

#length should be total number of terms
length(freq)

#create sort order (descending)
ord <- order(freq,decreasing=TRUE)

#List all terms in decreasing order of freq and write to disk
freq[ord]

#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 6

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,15))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerm.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))

#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])

#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])

#write to file
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))


## tip of the hat to following tutorial: https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/
