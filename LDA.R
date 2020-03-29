# load text mining library
library(tm)
library(mlbench)
library(class)
library(SparseM)
library(dismo)
library(topicmodels)
library(BH)
library(caTools)
library(glmnet)
library(e1071)
library(ipred)
library(randomForest)
library(tau)
library(tm)
library(topicmodels)


setwd("C:/Users/andym/r-workspace/MLProject/TwitterData/TimeStamp/Feb/Negative");


# load files into corpus
# get listing of .txt files in directory
filenames <- list.files(getwd())
names<-(filenames)
#read files into a character vector
files <- lapply(filenames,readLines)

# create corpus from vector
docs <- Corpus(VectorSource(files))

docs <-tm_map(docs,content_transformer(tolower))

# Create the toSpace content transformer
toSpace<- content_transformer(function(x, pattern){return (gsub(pattern, " ", x))});
docs<- tm_map(docs, toSpace, "<.*?>");
docs <- tm_map(docs, toSpace, ":");
docs<- tm_map(docs, toSpace, ",");
docs<- tm_map(docs, toSpace, "_");
docs<-tm_map(docs, toSpace, "-");
docs<-tm_map(docs, toSpace, "'");
docs<-tm_map(docs, toSpace, "`");

docs<- tm_map(docs, removePunctuation);
docs<- tm_map(docs, removeNumbers);
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace);

# # Negative sentiment Twitter123 stop words: CTRL+Shift+C to uncomment block
# myStopwords <- c("can", "say","one","way","use", "test",
#                  "also","however","tell","will", "think",
#                  "much","need","take","tend","even", "like", "particular", "rather", "said", "must", " s " , " t ", "agile", "test", "part", "write",
#                  "get", "well", "make", "ask", "come", "end", "first", "two", "may", "might", "see", "code", "project", "test", "done",
#                  "something", "thing", "point", "post", "look", "'ve", "'re", "parentid" , "id", "title", "postlink", "body", "thing","think", "just"
#                  , "thing","case", "cases", "really", "non", "etc", "using", "set", "level", "going", "lot", "back", "since", "let", "don","functional",
#                  "nonfunctional", "thus","else", "software development projects","software development project", "aren", "others", "things","requirement",
#                  "tags", "score", "answercount", "commentcount", "favoritecount", "ownerdisplayname", "ownerid", "viewcount", "http", "name", "osm", "create",
#                  "var", "points", "line", "find", "used", "now", "trying", "looking", "many", "thanks", "another", "following", "per", "within", "avail",
#                  "try", "names", "along", "please", "without", "tried", "still", "either", "anyone", "usually", "looks", "seem","owneruserid", "line", "lines",
#                  "https", "-", "—", "—", "‒", "–", "—", "―", "it's", "i'm", "i`m", "don't", "don’t", "it’s", "...", "can't", "can’t", "i’m", "donâ", "canâ", "want",
#                  "want", "know", "made", "gonna", "thatâ", "ever", "getting", "wanna", "ainâ", "didnâ", "thereâ", "sure", "gave", "havenâ", "isnâ", "theâ",
#                  "came", "whatâ", "andâ", "doesnâ", "show", "already", "says", "wonâ", "iâ", "till", "€™", "yall", "tryna", "sheâ", "taking", "˜‚", "looked",
#                  "tweet", "TRUE", "decided", "gone", "couldnâ", "especially", "pick", "seen", "stuff")

# # Positive sentiment Twitter123 stop words: CTRL+Shift+C to uncomment block
# myStopwords <- c("can", "say","one","way","use", "test",
#                     "also","however","tell","will", "think",
#                     "much","need","take","tend","even", "like", "particular", "rather", "said", "must", " s " , " t ", "agile", "test", "part", "write",
#                     "get", "well", "make", "ask", "come", "end", "first", "two", "may", "might", "see", "code", "project", "test", "done",
#                     "something", "thing", "point", "post", "look", "'ve", "'re", "parentid" , "id", "title", "postlink", "body", "thing","think", "just"
#                     , "thing","case", "cases", "really", "non", "etc", "using", "set", "level", "going", "lot", "back", "since", "let", "don","functional",
#                     "nonfunctional", "thus","else", "software development projects","software development project", "aren", "others", "things","requirement",
#                     "tags", "score", "answercount", "commentcount", "favoritecount", "ownerdisplayname", "ownerid", "viewcount", "http", "name", "osm", "create",
#                     "var", "points", "line", "find", "used", "now", "trying", "looking", "many", "thanks", "another", "following", "per", "within", "avail",
#                     "try", "names", "along", "please", "without", "tried", "still", "either", "anyone", "usually", "looks", "seem","owneruserid", "line", "lines",
#                     "https", "-", "—", "—", "‒", "–", "—", "―", "it's", "i'm", "i`m", "don't", "don’t", "it’s", "...", "can't", "can’t", "i’m", "donâ", "canâ", "want",
#                     "want", "know", "made", "gonna", "thatâ", "ever", "getting", "wanna", "ainâ", "didnâ", "thereâ", "sure", "gave", "havenâ", "isnâ", "theâ",
#                     "came", "whatâ", "andâ", "doesnâ", "show", "already", "says", "wonâ", "iâ", "till", "€™", "yall", "tryna", "sheâ", "taking", "˜‚", "looked",
#                     "tweet", "TRUE", "decided", "gone", "couldnâ", "especially", "pick", "seen", "stuff", "always", "lots", "went", "minute", "gets", "get",
#                     "giving", "taking")

# Negative sentiment stop words: CTRL+Shift+C to uncomment block
myStopwords <- c("can", "say","one","way","use", "test",
                 "also","however","tell","will", "think",
                 "much","need","take","tend","even", "like", "particular", "rather", "said", "must", " s " , " t ", "agile", "test", "part", "write",
                 "get", "well", "make", "ask", "come", "end", "first", "two", "may", "might", "see", "code", "project", "test", "done",
                 "something", "thing", "point", "post", "look", "'ve", "'re", "parentid" , "id", "title", "postlink", "body", "thing","think", "just"
                 , "thing","case", "cases", "really", "non", "etc", "using", "set", "level", "going", "lot", "back", "since", "let", "don","functional",
                 "nonfunctional", "thus","else", "software development projects","software development project", "aren", "others", "things","requirement",
                 "tags", "score", "answercount", "commentcount", "favoritecount", "ownerdisplayname", "ownerid", "viewcount", "http", "name", "osm", "create",
                 "var", "points", "line", "find", "used", "now", "trying", "looking", "many", "thanks", "another", "following", "per", "within", "avail",
                 "try", "names", "along", "please", "without", "tried", "still", "either", "anyone", "usually", "looks", "seem","owneruserid", "line", "lines",
                 "https", "-", "—", "—", "‒", "–", "—", "―", "it's", "i'm", "i`m", "don't", "don’t", "it’s", "...", "can't", "can’t", "i’m", "donâ", "canâ", "want",
                 "want", "know", "made", "gonna", "thatâ", "ever", "getting", "wanna", "ainâ", "didnâ", "thereâ", "sure", "gave", "havenâ", "isnâ", "theâ",
                 "came", "whatâ", "andâ", "doesnâ", "show", "already", "says", "wonâ", "iâ", "till", "€™", "yall", "tryna", "sheâ", "taking", "˜‚", "looked",
                 "tweet", "TRUE", "decided", "gone", "couldnâ", "especially", "pick", "seen", "stuff", "stop")

# # Positive sentiment stop words: CTRL+Shift+C to uncomment block
# myStopwords <- c("can", "say","one","way","use", "test",
#                  "also","however","tell","will", "think",
#                  "much","need","take","tend","even", "like", "particular", "rather", "said", "must", " s " , " t ", "agile", "test", "part", "write",
#                  "get", "well", "make", "ask", "come", "end", "first", "two", "may", "might", "see", "code", "project", "test", "done",
#                  "something", "thing", "point", "post", "look", "'ve", "'re", "parentid" , "id", "title", "postlink", "body", "thing","think", "just"
#                  , "thing","case", "cases", "really", "non", "etc", "using", "set", "level", "going", "lot", "back", "since", "let", "don","functional",
#                  "nonfunctional", "thus","else", "software development projects","software development project", "aren", "others", "things","requirement",
#                  "tags", "score", "answercount", "commentcount", "favoritecount", "ownerdisplayname", "ownerid", "viewcount", "http", "name", "osm", "create",
#                  "var", "points", "line", "find", "used", "now", "trying", "looking", "many", "thanks", "another", "following", "per", "within", "avail",
#                  "try", "names", "along", "please", "without", "tried", "still", "either", "anyone", "usually", "looks", "seem","owneruserid", "line", "lines",
#                  "https", "-", "—", "—", "‒", "–", "—", "―", "it's", "i'm", "i`m", "don't", "don’t", "it’s", "...", "can't", "can’t", "i’m", "donâ", "canâ", "want",
#                  "want", "know", "made", "gonna", "thatâ", "ever", "getting", "wanna", "ainâ", "didnâ", "thereâ", "sure", "gave", "havenâ", "isnâ", "theâ",
#                  "came", "whatâ", "andâ", "doesnâ", "show", "already", "says", "wonâ", "iâ", "till", "€™", "yall", "tryna", "sheâ", "taking", "˜‚", "looked",
#                  "tweet", "TRUE", "decided", "gone", "couldnâ", "especially", "pick", "seen", "stuff", "always", "lots", "went", "minute", "gets", "get",
#                  "giving", "taking")



# remove custom stopwords
docs <- tm_map(docs, removeWords, myStopwords)


# Create document-term matrix
dtm <- DocumentTermMatrix(docs)

# convert rownames to filenames
rownames(dtm) <- names

# remove empty entries in the document term matrix
rowTotals <- slam::row_sums(dtm)
dtm <- dtm[rowTotals > 0,]
# collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
# length should be total number of terms
length(freq)
# create sort order (descending)
ord <- order(freq,decreasing=TRUE)
# List all terms in decreasing order of freq and write to disk
freq[ord]
write.csv(freq[ord],"word_freq.csv")

# load topic models library
library(topicmodels)
# Set parameters for Gibbs sampling
k <- 3
burnin <- 4000
iter <- 3000
thin <- 500
keep <-50
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE



# Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

# Write out results
# docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))
ldaOut.topics
# top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,6))

write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

# probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))

theta <- posterior(ldaOut)$topics

# Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])


# Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])


# write to file
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))
