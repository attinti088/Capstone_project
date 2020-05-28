setwd("H:/RProjects/Capestone project")

library("tidyr")
library("dplyr")
library("tidytext")
library("tm")
library("openNLP")
library("RWeka")
library("tm")

twitterRawData <- readLines("en_US.twitter.txt",warn=FALSE,encoding="UTF-8")
blogsRawData <- readLines("en_US.blogs.txt",warn=FALSE,encoding="UTF-8")
newsRawData <- readLines("en_US.news.txt",warn=FALSE,encoding="UTF-8")
twitterRawData <- iconv(twitterRawData, "latin1", "ASCII", sub="")
blogsRawData <- iconv(blogsRawData, "latin1", "ASCII", sub="")
newsRawData <- iconv(newsRawData, "latin1", "ASCII", sub="")

set.seed(2020)
twitterRawData_sample<- sample(twitterRawData,length(twitterRawData)*0.2)
blogsRawData_sample<- sample(blogsRawData,length(blogsRawData)*0.2)
newsRawData_sample<- sample(newsRawData,length(newsRawData)*0.2)

finalData <- c(twitterRawData_sample,blogsRawData_sample,newsRawData_sample)

sampleData <- tibble(text = finalData)
twitterRawData <-tibble(text = twitterRawData_sample)
blogsRawData <-tibble(text = blogsRawData_sample)
newsRawData <-tibble(text = newsRawData_sample)


dir.create("rdata4", showWarnings = FALSE)
save(sampleData,file="rdata4//samples.Rdata")
save(twitterRawData,file="rdata4//twitterRawData.Rdata")
save(blogsRawData,file="rdata4//blogsRawData.Rdata")
save(newsRawData,file="rdata4//newsRawData.Rdata")


remove(twitterRawData)
remove(blogsRawData)
remove(newsRawData)
rm(list = ls())
gc()