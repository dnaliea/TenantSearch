#devtools::install_github("username/packagename")
library(devtools)
require(qdap)
require(tm)
library("Rstem") 
require("snowfall")
require("SnowballC")
library("twitteR")
library("wordcloud")

#bag_o_words(contentText$profileAdditionalInfo)
#by(contentText$profileAdditionalInfo, contentText$SearchRequestAdditionalInfo, bag_o_words)
#BoW_profileInfo<-lapply(contentText$profileAdditionalInfo,  bag_o_words)
#BoW_AddInfo<-lapply(contentText$searchRequestAdditionalInfo,  bag_o_words)

# build a corpus
doc.vec <- VectorSource(paste(content$searchRequestAdditionalInfo, content$profileAdditionalInfo,sep=" "))
#paste("Hello", "world", sep=" ")
mydata.corpus <- Corpus(doc.vec)

#make each letter lowercase
#tolower is not canonical here, usecontentTransformer
mydata.corpus <- tm_map(mydata.corpus, content_transformer(tolower)) 

#Alternative Option:
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) 

mydata.corpus <- tm_map(mydata.corpus, toSpace, "/|@|\\|&")
mydata.corpus <- tm_map(mydata.corpus, toSpace, "\\&amp")
mydata.corpus <- tm_map(mydata.corpus, toSpace, "\\quot")

toString <- content_transformer(function(x, from, to) gsub(from, to, x))
trim.trailing_in <- content_transformer(function(x) gsub("in$", " ", x))

mydata.corpus <- tm_map(mydata.corpus, toString, "akademiker", "akademiker ")
mydata.corpus <- tm_map(mydata.corpus, toString, "-", "")
mydata.corpus <- tm_map(mydata.corpus, toString, "nichtraucherin", "nichtraucher")
mydata.corpus <- tm_map(mydata.corpus, toString, "schufa", "schufa ")
mydata.corpus <- tm_map(mydata.corpus, toString, "zimmer", "")
mydata.corpus <- tm_map(mydata.corpus, toString, "raum", "")


mydata.corpus <- tm_map(mydata.corpus, trim.trailing_in)


# remove punctuation 
mydata.corpus <- tm_map(mydata.corpus, removePunctuation)
mydata.corpus <- tm_map(mydata.corpus, removeNumbers)
#"removeNumbers"

# remove generic and custom stopwords
my_stopwords <- c(stopwords('german'), "suche", "wohnung","zimmer", "wohnen" , "miet", "raum","jedoch" ,"evtl","bzw", "whg", "zieh", "wurd", "sollt", "jahrig" , "stuttgart", "hamburg", "berlin", "munchen", "such", "sucht" , "derzeit", "dass", "find" , "sowi",  "wohn", "per" ) #define generic and custom stopwords
mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)
mydata.corpus<-tm_map(mydata.corpus, stemDocument, language="german") 

# build a term-document matrix
mydata.dtm <- TermDocumentMatrix(mydata.corpus)#,content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),mc.cores=1)


#inspect(mydata.dtm[1:1000,1:10])

# inspect most popular words
mydata.dtm.common <- removeSparseTerms(mydata.dtm, 0.98) 
findFreqTerms(mydata.dtm.common, lowfreq=1)
dim(mydata.dtm.common)
inspect(mydata.dtm.common)


class(mydata.dtm.common)
