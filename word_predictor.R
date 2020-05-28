
load(file="rdata4//trigramDataFrameFinal.Rdata")
load(file="rdata4//bigramDataFrame.Rdata")
predict_fun <- function(x) {
  #clean incoming string
  
  x <- iconv(x, "latin1", "ASCII", sub="")
  input <-tibble(text = x)
  
  
  input_tokens <- input %>% unnest_tokens(word, text) 
  
  input_tokens$word <- gsub("\\_+","",input_tokens$word)
  input_tokens$word <- gsub("\\'+","",input_tokens$word)
  input_tokens$word <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", input_tokens$word) # remove retweet entities
  input_tokens$word <- gsub("@\\w+", "", input_tokens$word) # remove at people
  input_tokens$word <- gsub("[ \t]{2,}", "", input_tokens$word) # remove unnecessary tabs
  input_tokens$word <- gsub("^\\s+|\\s+$", "", input_tokens$word) # remove unnecessary spaces
  input_tokens$word <- gsub('https://','',input_tokens$word) # removes https://
  input_tokens$word <- gsub('http://','',input_tokens$word) # removes http://
  input_tokens$word <- gsub('[^[:graph:]]', ' ',input_tokens$word) ## removes graphic characters 
  input_tokens$word <- gsub('[[:punct:]]', '', input_tokens$word) # removes punctuation 
  input_tokens$word <- gsub('[[:cntrl:]]', '', input_tokens$word) # removes control characters
  input_tokens$word <- tolower(input_tokens$word) # makes all letters lowercase
  input_tokens$word <- input_tokens$word[!is.na(input_tokens$word)] # remove NAs
  len <- length(input_tokens$word)
  prediction1 <- ""
  prediction2 <- ""
  prediction3 <- ""
  if(len>=2){
    
    preddf <-  trigramDataFrame[trigramDataFrame$word1==input_tokens$word[len-1] & trigramDataFrame$word2==input_tokens$word[len],]$word3
    
    if(len>=1){
      prediction1 <- preddf[1]
    }
    if(len>=2){
      prediction2 <- preddf[2]
    }
    if(len>=3){
      prediction3 <- preddf[3]
    }
    
  }
  if(len==1){
    preddf <-  bigramDataFrame[trigramDataFrame$word1==input_tokens$word[len],]$word2
    if(len>=1){
      prediction1 <- preddf[1]
    }
    if(len>=2){
      prediction2 <- preddf[2]
    }
    if(len>=3){
      prediction3 <- preddf[3]
    }
    
  }
  if(len==0){
    prediction1 <-"Please capture the text";
    prediction2 <-"";
    prediction3 <- "";
    
  }
  return (c(prediction1,prediction2,prediction3))
}