setwd("H:/RProjects/Capestone project")

library("tidyr")
library("dplyr")
library("tidytext")
library("tm")
library("openNLP")
library("RWeka")

load("rdata4//samples.Rdata")

#finalSampleData <- c(twitterRawData,blogsRawData,newsRawData)
#sampleData <- tibble(text = finalSampleData)

data(stop_words)

tidySampleData <- sampleData %>% unnest_tokens(word, text) 
#%>% anti_join(stop_words)
#Removing whitespaces 

tidySampleData<-tidySampleData[-grep("^_+$", tidySampleData$word),]
tidySampleData$word <- gsub("\\_+","",tidySampleData$word)
tidySampleData$word <- gsub("\\'+","",tidySampleData$word)
tidySampleData<-tidySampleData[-grep("\\b\\d+\\b", tidySampleData$word),]


tidySampleData$word <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tidySampleData$word) # remove retweet entities
tidySampleData$word <- gsub("@\\w+", "", tidySampleData$word) # remove at people
tidySampleData$word <- gsub("[ \t]{2,}", "", tidySampleData$word) # remove unnecessary tabs
tidySampleData$word <- gsub("^\\s+|\\s+$", "", tidySampleData$word) # remove unnecessary spaces
tidySampleData$word <- gsub('https://','',tidySampleData$word) # removes https://
tidySampleData$word <- gsub('http://','',tidySampleData$word) # removes http://
tidySampleData$word <- gsub('[^[:graph:]]', ' ',tidySampleData$word) ## removes graphic characters 
tidySampleData$word <- gsub('[[:punct:]]', '', tidySampleData$word) # removes punctuation 
tidySampleData$word <- gsub('[[:cntrl:]]', '', tidySampleData$word) # removes control characters
tidySampleData$word <- tolower(tidySampleData$word) # makes all letters lowercase
tidySampleData$word <- tidySampleData$word[!is.na(tidySampleData$word)] # remove NAs


#Removing Numbers
head(unigrafDataFrame)
unigrafDataFrame <- data.frame (tidySampleData %>% count(word, sort = TRUE) )
unigrafDataFrame[order(unigrafDataFrame$n, decreasing=TRUE), ]
names(unigrafDataFrame) <- c("word","freq")


save(unigrafDataFrame,file="rdata4//unigrafDataFrame.Rdata")
rm(ls=list())
gc()
#Bigrams

#load("rdata//twitterRawData.Rdata")
#twittertidyBigramSampleData <- twitterRawData %>% unnest_tokens(bigram, text,token = "ngrams", n = 2) 
#twittertidyBigramSampleData_separated <- twittertidyBigramSampleData %>% separate(bigram, c("word1", "word2"), sep = " ")
#twitterbigrams_filtered <- twittertidyBigramSampleData_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)

#load("rdata//blogsRawData.Rdata")

#blogstidyBigramSampleData <- blogsRawData %>% unnest_tokens(bigram, text,token = "ngrams", n = 2) 
#blogstidyBigramSampleData_separated <- blogstidyBigramSampleData %>% separate(bigram, c("word1", "word2"), sep = " ")
#blogsbigrams_filtered <- blogstidyBigramSampleData_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)


#load("rdata//newsRawData.Rdata")
#head(newsRawData)
#newstidyBigramSampleData <- newsRawData %>% unnest_tokens(bigram, text,token = "ngrams", n = 2) 
#newstidyBigramSampleData_separated <- newstidyBigramSampleData %>% separate(bigram, c("word1", "word2"), sep = " ")
#newsbigrams_filtered <- newstidyBigramSampleData_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)

#bigrams_filtered <- rbind(twittertidyBigramSampleData_separated,blogstidyBigramSampleData_separated,newstidyBigramSampleData_separated)

tidyBigramSampleData <- sampleData %>% unnest_tokens(bigram, text,token = "ngrams", n = 2) 
bigrams_filtered <- tidyBigramSampleData %>% separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered<-bigrams_filtered[-grep("^_+$", bigrams_filtered$word1),]
bigrams_filtered<-bigrams_filtered[-grep("^_+$", bigrams_filtered$word2),]

bigrams_filtered$word1 <- gsub("\\_+","",bigrams_filtered$word1)
bigrams_filtered$word2 <- gsub("\\_+","",bigrams_filtered$word2)

bigrams_filtered$word1 <- gsub("\\'+","",bigrams_filtered$word1)
bigrams_filtered$word2 <- gsub("\\'+","",bigrams_filtered$word2)

bigrams_filtered<-bigrams_filtered[-grep("\\b\\d+\\b", bigrams_filtered$word1),]
bigrams_filtered<-bigrams_filtered[-grep("\\b\\d+\\b", bigrams_filtered$word2),]




bigrams_filtered$word1 <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", bigrams_filtered$word1) # remove retweet entities
bigrams_filtered$word1 <- gsub("@\\w+", "", bigrams_filtered$word1) # remove at people
bigrams_filtered$word1 <- gsub("[ \t]{2,}", "", bigrams_filtered$word1) # remove unnecessary tabs
bigrams_filtered$word1 <- gsub("^\\s+|\\s+$", "", bigrams_filtered$word1) # remove unnecessary spaces
bigrams_filtered$word1 <- gsub('https://','',bigrams_filtered$word1) # removes https://
bigrams_filtered$word1 <- gsub('http://','',bigrams_filtered$word1) # removes http://
bigrams_filtered$word1 <- gsub('[^[:graph:]]', ' ',bigrams_filtered$word1) ## removes graphic characters 
bigrams_filtered$word1 <- gsub('[[:punct:]]', '', bigrams_filtered$word1) # removes punctuation 
bigrams_filtered$word1 <- gsub('[[:cntrl:]]', '', bigrams_filtered$word1) # removes control characters
bigrams_filtered$word1 <- tolower(bigrams_filtered$word1) # makes all letters lowercase
bigrams_filtered$word1 <- bigrams_filtered$word1[!is.na(bigrams_filtered$word1)] # remove NAs


bigrams_filtered$word2 <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", bigrams_filtered$word2) # remove retweet entities
bigrams_filtered$word2 <- gsub("@\\w+", "", bigrams_filtered$word2) # remove at people
bigrams_filtered$word2 <- gsub("[ \t]{2,}", "", bigrams_filtered$word2) # remove unnecessary tabs
bigrams_filtered$word2 <- gsub("^\\s+|\\s+$", "", bigrams_filtered$word2) # remove unnecessary spaces
bigrams_filtered$word2 <- gsub('https://','',bigrams_filtered$word2) # removes https://
bigrams_filtered$word2 <- gsub('http://','',bigrams_filtered$word2) # removes http://
bigrams_filtered$word2 <- gsub('[^[:graph:]]', ' ',bigrams_filtered$word2) ## removes graphic characters 
bigrams_filtered$word2 <- gsub('[[:punct:]]', '', bigrams_filtered$word2) # removes punctuation 
bigrams_filtered$word2 <- gsub('[[:cntrl:]]', '', bigrams_filtered$word2) # removes control characters
bigrams_filtered$word2 <- tolower(bigrams_filtered$word2) # makes all letters lowercase
bigrams_filtered$word2 <- bigrams_filtered$word2[!is.na(bigrams_filtered$word2)] # remove NAs


bigramDataFrame <-data.frame (bigrams_filtered %>% count(word1,word2,sort=TRUE))

names(bigramDataFrame) <- c("word1", "word2", "freq");

bigramDataFrame <- merge(bigramDataFrame,unigrafDataFrame,by.x="word1",by.y="word")


bigramDataFrame <- bigramDataFrame%>% mutate (prob=freq.x/freq.y)

bigramDataFrame <-bigramDataFrame[,c(1,2,3,5)]
names(bigramDataFrame) <- c("word1", "word2", "freq", "prob")
bigramDataFrame <- bigramDataFrame[order(bigramDataFrame$prob, decreasing=TRUE), ];

save(bigramDataFrame,file="rdata4//bigramDataFrame.Rdata")

rm(ls=list())
gc()

#Trigrams

#load("rdata//twitterRawData.Rdata")

#twittertidyTrigramSampleData <- twitterRawData %>% unnest_tokens(bigram, text,token = "ngrams", n = 3) 
#twittertidyTrigramSampleData_separated <- twittertidyTrigramSampleData %>% separate(bigram, c("word1", "word2","word3"), sep = " ")
#twitterTrigrams_filtered <- twittertidyTrigramSampleData_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)%>% filter(!word3 %in% stop_words$word)
#remove(twittertidyTrigramSampleData)
#save(twittertidyTrigramSampleData_separated,file="rdata3//twittertidyTrigramSampleData_separated.Rdata")

#load("rdata//blogsRawData.Rdata")
#blogstidyTrigramSampleData <- blogsRawData %>% unnest_tokens(bigram, text,token = "ngrams", n = 3) 
#blogstidyTrigramSampleData_separated <- blogstidyTrigramSampleData %>% separate(bigram, c("word1", "word2","word3"), sep = " ")
#blogsTrigrams_filtered <- blogstidyTrigramSampleData_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)%>% filter(!word3 %in% stop_words$word)
#remove(blogstidyTrigramSampleData)
#save(blogstidyTrigramSampleData_separated,file="rdata3//blogstidyTrigramSampleData_separated.Rdata")


#load("rdata//newsRawData.Rdata")

#newstidyTrigramSampleData <- newsRawData %>% unnest_tokens(bigram, text,token = "ngrams", n = 3) 
#newstidyTrigramSampleData_separated <- newstidyTrigramSampleData %>% separate(bigram, c("word1", "word2","word3"), sep = " ")
#newsTrigrams_filtered <- newstidyTrigramSampleData_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)%>% filter(!word3 %in% stop_words$word)
#remove(newstidyTrigramSampleData)

#load("rdata3//twittertidyTrigramSampleData_separated.Rdata")
tidyTrigramSampleData <- sampleData %>% unnest_tokens(bigram, text,token = "ngrams", n = 3) 
trigrams_filtered <- tidyTrigramSampleData %>% separate(bigram, c("word1", "word2","word3"), sep = " ")



trigrams_filtered<-trigrams_filtered[-grep("^_+$", trigrams_filtered$word1),]
trigrams_filtered<-trigrams_filtered[-grep("^_+$", trigrams_filtered$word2),]
trigrams_filtered<-trigrams_filtered[-grep("^_+$", trigrams_filtered$word3),]
trigrams_filtered$word1 <- gsub("\\_+","",trigrams_filtered$word1)
trigrams_filtered$word2 <- gsub("\\_+","",trigrams_filtered$word2)
trigrams_filtered$word3 <- gsub("\\_+","",trigrams_filtered$word3)
trigrams_filtered$word1 <- gsub("\\'+","",trigrams_filtered$word1)
trigrams_filtered$word2 <- gsub("\\'+","",trigrams_filtered$word2)
trigrams_filtered$word3 <- gsub("\\'+","",trigrams_filtered$word3)

trigrams_filtered<-trigrams_filtered[-grep("\\b\\d+\\b", trigrams_filtered$word1),]
trigrams_filtered<-trigrams_filtered[-grep("\\b\\d+\\b", trigrams_filtered$word2),]
trigrams_filtered<-trigrams_filtered[-grep("\\b\\d+\\b", trigrams_filtered$word3),]

trigrams_filtered$word1 <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", trigrams_filtered$word1) # remove retweet entities
trigrams_filtered$word1 <- gsub("@\\w+", "", trigrams_filtered$word1) # remove at people
trigrams_filtered$word1 <- gsub("[ \t]{2,}", "", trigrams_filtered$word1) # remove unnecessary tabs
trigrams_filtered$word1 <- gsub("^\\s+|\\s+$", "", trigrams_filtered$word1) # remove unnecessary spaces
trigrams_filtered$word1 <- gsub('https://','',trigrams_filtered$word1) # removes https://
trigrams_filtered$word1 <- gsub('http://','',trigrams_filtered$word1) # removes http://
trigrams_filtered$word1 <- gsub('[^[:graph:]]', ' ',trigrams_filtered$word1) ## removes graphic characters 
trigrams_filtered$word1 <- gsub('[[:punct:]]', '', trigrams_filtered$word1) # removes punctuation 
trigrams_filtered$word1 <- gsub('[[:cntrl:]]', '', trigrams_filtered$word1) # removes control characters
trigrams_filtered$word1 <- tolower(trigrams_filtered$word1) # makes all letters lowercase
trigrams_filtered$word1 <- trigrams_filtered$word1[!is.na(trigrams_filtered$word1)] # remove NAs

trigrams_filtered$word2 <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", trigrams_filtered$word2) # remove retweet entities
trigrams_filtered$word2 <- gsub("@\\w+", "", trigrams_filtered$word2) # remove at people
trigrams_filtered$word2 <- gsub("[ \t]{2,}", "", trigrams_filtered$word2) # remove unnecessary tabs
trigrams_filtered$word2 <- gsub("^\\s+|\\s+$", "", trigrams_filtered$word2) # remove unnecessary spaces
trigrams_filtered$word2 <- gsub('https://','',trigrams_filtered$word2) # removes https://
trigrams_filtered$word2 <- gsub('http://','',trigrams_filtered$word2) # removes http://
trigrams_filtered$word2 <- gsub('[^[:graph:]]', ' ',trigrams_filtered$word2) ## removes graphic characters 
trigrams_filtered$word2 <- gsub('[[:punct:]]', '', trigrams_filtered$word2) # removes punctuation 
trigrams_filtered$word2 <- gsub('[[:cntrl:]]', '', trigrams_filtered$word2) # removes control characters
trigrams_filtered$word2 <- tolower(trigrams_filtered$word2) # makes all letters lowercase
trigrams_filtered$word2 <- trigrams_filtered$word2[!is.na(trigrams_filtered$word2)] # remove NAs

trigrams_filtered$word3 <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", trigrams_filtered$word3) # remove retweet entities
trigrams_filtered$word3 <- gsub("@\\w+", "", trigrams_filtered$word3) # remove at people
trigrams_filtered$word3 <- gsub("[ \t]{2,}", "", trigrams_filtered$word3) # remove unnecessary tabs
trigrams_filtered$word3 <- gsub("^\\s+|\\s+$", "", trigrams_filtered$word3) # remove unnecessary spaces
trigrams_filtered$word3 <- gsub('https://','',trigrams_filtered$word3) # removes https://
trigrams_filtered$word3 <- gsub('http://','',trigrams_filtered$word3) # removes http://
trigrams_filtered$word3 <- gsub('[^[:graph:]]', ' ',trigrams_filtered$word3) ## removes graphic characters 
trigrams_filtered$word3 <- gsub('[[:punct:]]', '', trigrams_filtered$word3) # removes punctuation 
trigrams_filtered$word3 <- gsub('[[:cntrl:]]', '', trigrams_filtered$word3) # removes control characters
trigrams_filtered$word3 <- tolower(trigrams_filtered$word3) # makes all letters lowercase
trigrams_filtered$word3 <- trigrams_filtered$word3[!is.na(trigrams_filtered$word3)] # remove NAs



trigramDataFrame <-data.frame (trigrams_filtered %>% count(word1,word2,word3,sort=TRUE))
save(trigramDataFrame,file="rdata4//trigrams_filteredDataFrame.Rdata")

names(trigramDataFrame) <- c("word1", "word2","word3", "freq");

load("rdata4//bigramDataFrame.Rdata")

trigramDataFrame <- merge(trigramDataFrame,bigramDataFrame[,c("word1", "word2","freq")],by=c("word1", "word2"))
head(trigramDataFrame)

trigramDataFrame <- trigramDataFrame%>% mutate (prob=freq.x/freq.y)

trigramDataFrame <-trigramDataFrame[,c(1,2,3,4,6)]
names(trigramDataFrame) <- c("word1", "word2", "word3","freq", "prob")
trigramDataFrame <- trigramDataFrame[order(trigramDataFrame$prob, decreasing=TRUE), ]

save(trigramDataFrame,file="rdata4//trigramDataFrameFinal.Rdata")


#Quadgrams

#tidyQuadgramSampleData <- sampleData %>% unnest_tokens(trigram, text,token = "ngrams", n = 4) 
#Seperating the bigram 

#tidyQuadgramSampleData_separated <- tidyQuadgramSampleData %>% separate(trigram, c("word1", "word2","word3","word4"), sep = " ")
#quadgrams_filtered <- tidyQuadgramSampleData_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)%>% filter(!word3 %in% stop_words$word)%>% filter(!word4 %in% stop_words$word)

load("rdata//twitterRawData.Rdata")

twittertidyQuadgramSampleData <- twitterRawData %>% unnest_tokens(bigram, text,token = "ngrams", n = 4) 
twittertidyQuadgramSampleData_separated <- twittertidyQuadgramSampleData %>% separate(bigram, c("word1", "word2","word3","word4"), sep = " ")
twitterQuadgrams_filtered <- twittertidyQuadgramSampleData_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)%>% filter(!word3 %in% stop_words$word) %>% filter(!word4 %in% stop_words$word)
remove(twittertidyQuadgramSampleData)
remove(twittertidyQuadgramSampleData_separated)

save(twitterQuadgrams_filtered,file="rdata2//twitterQuadgrams_filtered.Rdata")

load("rdata//blogsRawData.Rdata")

blogstidyQuadgramSampleData <- blogsRawData %>% unnest_tokens(bigram, text,token = "ngrams", n = 4) 
blogstidyQuadgramSampleData_separated <- blogstidyQuadgramSampleData %>% separate(bigram, c("word1", "word2","word3","word4"), sep = " ")
blogsQuadgrams_filtered <- blogstidyQuadgramSampleData_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)%>% filter(!word3 %in% stop_words$word) %>% filter(!word4 %in% stop_words$word)
remove(blogstidyQuadgramSampleData)
remove(blogstidyQuadgramSampleData_separated)
remove(blogsRawData)

save(blogsQuadgrams_filtered,file="rdata2//blogsQuadgrams_filtered.Rdata")

load("rdata//newsRawData.Rdata")

newstidyQuadgramSampleData <- newsRawData %>% unnest_tokens(bigram, text,token = "ngrams", n = 4) 
newstidyQuadgramSampleData_separated <- newstidyQuadgramSampleData %>% separate(bigram, c("word1", "word2","word3","word4"), sep = " ")
newsQuadgrams_filtered <- newstidyQuadgramSampleData_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)%>% filter(!word3 %in% stop_words$word) %>% filter(!word4 %in% stop_words$word)
remove(newstidyQuadgramSampleData)
remove(newstidyQuadgramSampleData_separated)
remove(newsRawData)

load("rdata2//blogsQuadgrams_filtered.Rdata")
load("rdata2//twitterQuadgrams_filtered.Rdata")

quadgrams_filtered <- rbind(twitterQuadgrams_filtered,blogsQuadgrams_filtered,newsQuadgrams_filtered)
remove(twitterQuadgrams_filtered)
remove(blogsQuadgrams_filtered)
remove(newsQuadgrams_filtered)



quadgrams_filtered<-quadgrams_filtered[-grep("^_+$", quadgrams_filtered$word1),]
quadgrams_filtered<-quadgrams_filtered[-grep("^_+$", quadgrams_filtered$word2),]
quadgrams_filtered<-quadgrams_filtered[-grep("^_+$", quadgrams_filtered$word3),]
quadgrams_filtered<-quadgrams_filtered[-grep("^_+$", quadgrams_filtered$word4),]
quadgrams_filtered$word1 <- gsub("\\_+","",quadgrams_filtered$word1)
quadgrams_filtered$word2 <- gsub("\\_+","",quadgrams_filtered$word2)
quadgrams_filtered$word3 <- gsub("\\_+","",quadgrams_filtered$word3)
quadgrams_filtered$word4 <- gsub("\\_+","",quadgrams_filtered$word4)
quadgrams_filtered$word1 <- gsub("\\'+","",quadgrams_filtered$word1)
quadgrams_filtered$word2 <- gsub("\\'+","",quadgrams_filtered$word2)
quadgrams_filtered$word3 <- gsub("\\'+","",quadgrams_filtered$word3)
quadgrams_filtered$word4 <- gsub("\\'+","",quadgrams_filtered$word4)
quadgrams_filtered<-quadgrams_filtered[-grep("\\b\\d+\\b", quadgrams_filtered$word1),]
quadgrams_filtered<-quadgrams_filtered[-grep("\\b\\d+\\b", quadgrams_filtered$word2),]
quadgrams_filtered<-quadgrams_filtered[-grep("\\b\\d+\\b", quadgrams_filtered$word3),]
quadgrams_filtered<-quadgrams_filtered[-grep("\\b\\d+\\b", quadgrams_filtered$word4),]

quadgrams_filtered$word1 <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", quadgrams_filtered$word1) # remove retweet entities
quadgrams_filtered$word1 <- gsub("@\\w+", "", quadgrams_filtered$word1) # remove at people
quadgrams_filtered$word1 <- gsub("[ \t]{2,}", "", quadgrams_filtered$word1) # remove unnecessary tabs
quadgrams_filtered$word1 <- gsub("^\\s+|\\s+$", "", quadgrams_filtered$word1) # remove unnecessary spaces
quadgrams_filtered$word1 <- gsub('https://','',quadgrams_filtered$word1) # removes https://
quadgrams_filtered$word1 <- gsub('http://','',quadgrams_filtered$word1) # removes http://
quadgrams_filtered$word1 <- gsub('[^[:graph:]]', ' ',quadgrams_filtered$word1) ## removes graphic characters 
quadgrams_filtered$word1 <- gsub('[[:punct:]]', '', quadgrams_filtered$word1) # removes punctuation 
quadgrams_filtered$word1 <- gsub('[[:cntrl:]]', '', quadgrams_filtered$word1) # removes control characters
quadgrams_filtered$word1 <- tolower(quadgrams_filtered$word1) # makes all letters lowercase
quadgrams_filtered$word1 <- quadgrams_filtered$word1[!is.na(quadgrams_filtered$word1)] # remove NAs

quadgrams_filtered$word2 <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", quadgrams_filtered$word2) # remove retweet entities
quadgrams_filtered$word2 <- gsub("@\\w+", "", quadgrams_filtered$word2) # remove at people
quadgrams_filtered$word2 <- gsub("[ \t]{2,}", "", quadgrams_filtered$word2) # remove unnecessary tabs
quadgrams_filtered$word2 <- gsub("^\\s+|\\s+$", "", quadgrams_filtered$word2) # remove unnecessary spaces
quadgrams_filtered$word2 <- gsub('https://','',quadgrams_filtered$word2) # removes https://
quadgrams_filtered$word2 <- gsub('http://','',quadgrams_filtered$word2) # removes http://
quadgrams_filtered$word2 <- gsub('[^[:graph:]]', ' ',quadgrams_filtered$word2) ## removes graphic characters 
quadgrams_filtered$word2 <- gsub('[[:punct:]]', '', quadgrams_filtered$word2) # removes punctuation 
quadgrams_filtered$word2 <- gsub('[[:cntrl:]]', '', quadgrams_filtered$word2) # removes control characters
quadgrams_filtered$word2 <- tolower(quadgrams_filtered$word2) # makes all letters lowercase
quadgrams_filtered$word2 <- quadgrams_filtered$word2[!is.na(quadgrams_filtered$word2)] # remove NAs

quadgrams_filtered$word3 <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", quadgrams_filtered$word3) # remove retweet entities
quadgrams_filtered$word3 <- gsub("@\\w+", "", quadgrams_filtered$word3) # remove at people
quadgrams_filtered$word3 <- gsub("[ \t]{2,}", "", quadgrams_filtered$word3) # remove unnecessary tabs
quadgrams_filtered$word3 <- gsub("^\\s+|\\s+$", "", quadgrams_filtered$word3) # remove unnecessary spaces
quadgrams_filtered$word3 <- gsub('https://','',quadgrams_filtered$word3) # removes https://
quadgrams_filtered$word3 <- gsub('http://','',quadgrams_filtered$word3) # removes http://
quadgrams_filtered$word3 <- gsub('[^[:graph:]]', ' ',quadgrams_filtered$word3) ## removes graphic characters 
quadgrams_filtered$word3 <- gsub('[[:punct:]]', '', quadgrams_filtered$word3) # removes punctuation 
quadgrams_filtered$word3 <- gsub('[[:cntrl:]]', '', quadgrams_filtered$word3) # removes control characters
quadgrams_filtered$word3 <- tolower(quadgrams_filtered$word3) # makes all letters lowercase
quadgrams_filtered$word3 <- quadgrams_filtered$word3[!is.na(quadgrams_filtered$word3)] # remove NAs

quadgrams_filtered$word4 <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", quadgrams_filtered$word4) # remove retweet entities
quadgrams_filtered$word4 <- gsub("@\\w+", "", quadgrams_filtered$word4) # remove at people
quadgrams_filtered$word4 <- gsub("[ \t]{2,}", "", quadgrams_filtered$word4) # remove unnecessary tabs
quadgrams_filtered$word4 <- gsub("^\\s+|\\s+$", "", quadgrams_filtered$word4) # remove unnecessary spaces
quadgrams_filtered$word4 <- gsub('https://','',quadgrams_filtered$word4) # removes https://
quadgrams_filtered$word4 <- gsub('http://','',quadgrams_filtered$word4) # removes http://
quadgrams_filtered$word4 <- gsub('[^[:graph:]]', ' ',quadgrams_filtered$word4) ## removes graphic characters 
quadgrams_filtered$word4 <- gsub('[[:punct:]]', '', quadgrams_filtered$word4) # removes punctuation 
quadgrams_filtered$word4 <- gsub('[[:cntrl:]]', '', quadgrams_filtered$word4) # removes control characters
quadgrams_filtered$word4 <- tolower(quadgrams_filtered$word4) # makes all letters lowercase
quadgrams_filtered$word4 <- quadgrams_filtered$word4[!is.na(quadgrams_filtered$word4)] # remove NAs

quadgramDataFrame <-data.frame (quadgrams_filtered %>% count(word1,word2,word3,word4,sort=TRUE))


names(quadgramDataFrame) <- c("word1", "word2","word3","word4", "freq");

load("rdata2//trigramDataFrame.Rdata")
quadgramDataFrame <- merge(quadgramDataFrame,trigramDataFrame[,c("word1", "word2","word3","freq")],by=c("word1", "word2","word3"))


quadgramDataFrame <- quadgramDataFrame%>% mutate (prob=freq.x/freq.y)

quadgramDataFrame <-quadgramDataFrame[,c(1,2,3,4,5,7)]
names(quadgramDataFrame) <- c("word1", "word2", "word3","word4","freq", "prob")
quadgramDataFrame <- quadgramDataFrame[order(quadgramDataFrame$prob, decreasing=TRUE), ]
head(quadgramDataFrame)
save(quadgramDataFrame,file="rdata2//quadgramDataFrame.Rdata")



