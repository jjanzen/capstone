# literal - language
# regular expressions - combinations of literals
# metacharacters - ^i think - would be start of sentance - "i think we should."
# morning$ - end of line - "well they should this morning"
# [Bb] [Uu] [Ss] [Bb] - will match all versions of the word "bush"
# ^[Ii] am -  will find "i am" or "I am" at start of line 
# ^[0-9] [a-zA-Z] - begin of line num 0-9 and any letter - "7th inning stretch"
# [^?.]$ - looking for any line that does not end in "?" or "." - "we all jump anyway!"
# 9.11 - the "dot" will match any character - "9-11" or "9a11"
# flood|fire - will match any line with "flood" or "fire"
# ^[Gg]ood|[Bb]ad - start line with "good" or "bad" anywhere in line
# ^([Gg]ood|[Bb]ad) - start line with "good" or "bad" 
# [Gg]eorge( [Ww]\.)? [Bb]ush - will matach "george bush" with option to include "w.".  the "\" escapes and uses "." as literal
# (.*) - serach for anything between parathesis - "(24, m, germany)"
# [0-9]+ (.*)[0-9]+ - what be any numbers, followed by text, the followed by numbers again
# [Bb]ush( +[^]++){1,5} debate - will include word "bush" and up to 5 words between "debate".  {1,} - would be at least one time
# +([a-zA-Z]+) +\1 + - start with space, then word, then same word again
# ^s(.*)s - "sitting at starbucks"
# grep, grepl, sub, gsub

getwd()
setwd("~/JHDataScience/capstone/final")
list.files(path=".")

## task 1 ***************************************************
# US blogs
con_en_blogs <- file("./en_US/en_US.blogs.txt", "r") # read full blog file
en_blogs_limit <- readLines(con_en_blogs, 1000) # get top 1000 lines
close(con_en_blogs) # close connection
en_blogs_sample_num <- sample(en_blogs_limit,25,replace=F) # sample 25 lines from the 1000

fileConn<-file("./en_US/sample_en_US.blogs.txt") # create new file
writeLines(en_blogs_sample_num, fileConn) # write the 25 sampled lines
close(fileConn) # close new file connection

# US News
con_en_news <- file("./en_US/en_US.news.txt", "r") 
en_news_limit <- readLines(con_en_news, 1000)
close(con_en_news)
en_news_sample_num <- sample(en_news_limit,25,replace=F)

fileConn<-file("./en_US/sample_en_US.news.txt")
writeLines(en_news_sample_num, fileConn)
close(fileConn)

# US Twitter
con_en_twitter <- file("./en_US/en_US.twitter.txt", "r") 
en_twitter_limit <- readLines(con_en_twitter, 1000)
close(con_en_twitter)
en_twitter_sample_num <- sample(en_twitter_limit,25,replace=F)

fileConn<-file("./en_US/sample_en_US.twitter.txt")
writeLines(en_twitter_sample_num, fileConn)
close(fileConn)

con_en_twitter <- file("./en_US/en_US.twitter.txt", "r") 
en_twitter <- readLines(con_en_twitter, 1)
en_twitter
close(con_en_twitter)

# get each of the new sampled files
con_en_blog_sample <- file("./en_US/samples/sample_en_US.blogs.txt", "r") # read sample blog file
en_blog_sample <- readLines(con_en_blog_sample)
close(con_en_blog_sample)
en_blog_sample

con_en_news_sample <- file("./en_US/samples/sample_en_US.news.txt", "r") # read sample news file
en_news_sample <- readLines(con_en_news_sample)
close(con_en_news_sample)
en_news_sample
list.files("en_US/")

setwd("~/JHDataScience/capstone/final")
con_en_twitter_sample <- file("./en_US/samples/sample_en_US.twitter.txt", "r") # read sample twitter file
en_twitter_sample <- readLines(con_en_twitter_sample)
close(con_en_twitter_sample)
en_twitter_sample
en_samples <- rbind(en_blog_sample, en_news_sample, en_twitter_sample)
en_samples
## end task 1 ***************************************************

## task 2 ********************************************************
# how frequenty do certain works occur, and frequtcy of pairs of words appear
# first have expectations, refine as you go
#http://onepager.togaware.com/TextMiningO.pdf

## works
library(tm)
getTransformations # tm_map transformations
stopwords("english")

my_corpus <- Corpus(VectorSource(en_samples))
inspect(my_corpus)
my_corpus <- tm_map(my_corpus, removeNumbers)
my_corpus <- tm_map(my_corpus, removePunctuation)
my_corpus <- tm_map(my_corpus, removeWords, stopwords("english"))
my_corpus <- tm_map(my_corpus, content_transformer(tolower))
my_corpus <- tm_map(my_corpus, stripWhitespace)
#my_corpus <- tm_map(my_corpus, stemDocument) # removes "es", "ed", and "s", etc
my_corpus
dataframe<-data.frame(text=unlist(sapply(my_corpus, `[`, "content")), 
                      stringsAsFactors=F)
dataframe

# create a documnet term matrix
dtm <- DocumentTermMatrix(my_corpus)
dtm
dim(dtm)
freq <- colSums(as.matrix(dtm))
freq
length(freq)
ord <- order(freq)
freq[tail(ord,30)] # top 30 words
freq[ord] # all words 
# Frequency of frequencies.
head(table(freq), 15) # 145 of 161 words occured 1 time.  18 words twice... 
tail(table(freq), 15)
table_word_count <- table(freq) # table of count of words
barplot(table_word_count, main ="Freq of Words", ylab ="words", xlab ="freq of words", col = "blue")
table_word_count

# word pairs
library(tm); 
BigramTokenizer <-
    function(x)
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

tdm_pairs <- DocumentTermMatrix(my_corpus, control = list(tokenize = BigramTokenizer))
tdm_pairs
freq_pairs <- colSums(as.matrix(tdm_pairs))
freq_pairs
length(freq_pairs)
ord <- order(freq_pairs)
freq_pairs[tail(ord,30)] # top 30 words
freq_pairs[ord] # all words 
# Frequency of frequencies.
head(table(freq_pairs), 15) # 145 of 161 words occured 1 time.  18 words twice... 
tail(table(freq_pairs), 15)
table_word_count_pairs <- table(freq_pairs) # table of count of words
barplot(table_word_count_pairs, main ="Freq of Words", ylab ="words", xlab ="freq of words", col = "blue")
table_word_count_pairs
# end word pairs

# convert to csv
m <- as.matrix(dtm)
m
dim(m)
#write.csv(m, file="dtm.csv")
dim(dtm)
dtms <- removeSparseTerms(dtm, .01)
dim(dtms)
inspect(dtms)
freq <- colSums(as.matrix(dtms))
freq

# identify freq items and associations
findFreqTerms(dtm, lowfreq = 5)
findAssocs(dtm, "love", corlimit = 0.1) # for the word "love"
#install.packages("Rgraphviz")
#library(Rgraphviz)
#plot(dtm, terms=findFreqTerms(dtm, lowfreq=100)[1:50], corThreshold = 0.5)

freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
head(freq,10)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)
library(ggplot2)
data_ggplot <- subset(wf, freq>5) 
ggplot(data_ggplot, aes(word, freq)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=45,hjust=1))
?ggplot

# wordcloud
library(wordcloud)
set.seed(123)
wordcloud(names(freq), freq, min.freq=2)

## end works











tdm <- TermDocumentMatrix(VectorSource(en_twitter_sample),control = list(removePunctuation = TRUE, stopwords = TRUE))
a  <-Corpus(VectorSource(en_twitter_sample), readerControl = list(language="lat")) #specifies the exact folder where my text file(s) is for analysis with tm.
a <- tm_map(a, removePunctuation)
a
summary(a)  #check what went in
a <- tm_map(a, removeNumbers)
a <- tm_map(a, removePunctuation)
a <- tm_map(a , stripWhitespace)
a <- tm_map(a, tolower)
a <- tm_map(a, removeWords, stopwords("english")) # this stopword file is at C:\Users\[username]\Documents\R\win-library\2.13\tm\stopwords 
a <- tm_map(a, stemDocument, language = "english")
adtm <-DocumentTermMatrix(a) 
adtm <- removeSparseTerms(adtm, 0.75)
a
**************************

list.files(path="./de_DE")
de_blogs <- read.table("de_DE/de_DE.blogs.txt", fill=T)
head(de_blogs)
de_news <- read.table("de_DE/de_DE.news.txt")
de_twitter <- read.table("de_DE/de_DE.twitter.txt")

list.files(path="./en_US")
en_blogs <- read.table("en_US/en_US.blogs.txt", fill=T)
en_blogs_rows <- nrow(en_blogs)

#en_blogs_sample <- en_blogs[sample(nrow(en_blogs),nrow(en_blogs)*.05),]
#en_blogs_sample <- head(en_blogs, 200)
#nrow(en_blogs)*.05
#head(en_blogs_sample)
#str(en_blogs_sample)
#head(en_blogs[1,]) # first row


en_news <- read.table("en_US/en_US.news.txt")
en_twitter <- read.table("en_US/en_US.twitter.txt")


list.files(path="./fi_FI")
fi_blogs <- read.table("fi_FI/fi_FI.blogs.txt", fill=T)
head(fi_blogs)
fi_news <- read.table("fi_FI/fi_FI.news.txt")
fi_twitter <- read.table("fi_FI/fi_FI.twitter.txt")

list.files(path="./ru_RU")
ru_blogs <- read.table("ru_RU/ru_RU.blogs.txt", fill=T)
head(ru_blogs)
ru_blogs[1]
str(ru_blogs)
re_news <- read.table("ru_RU/ru_RU.news.txt")
re_twitter <- read.table("ru_RU/ru_RU.twitter.txt")

My code to sample:
    
    writeLines(sample(blg_data, length(blg_data)*.05)
    con="C:\\Users\\Lou\\Documents\\rcaps\\data\\blogs\\smp\\smp_blogs.txt",sep="\n",useBytes=FALSE)

Task 2 - Exploratory analysis
The first step in building a predictive model for text is understanding the distribution and relationship between the words, tokens, and phrases in the text. The goal of this task is to understand the basic relationships you observe in the data and prepare to build your first linguistic models.

Tasks to accomplish

Exploratory analysis - perform a thorough exploratory analysis of the data, understanding the distribution of words and relationship between the words in the corpora.
Understand frequencies of words and word pairs - build figures and tables to understand variation in the frequencies of words and word pairs in the data.
Questions to consider

Some words are more frequent than others - what are the distributions of word frequencies?
What are the frequencies of 2-grams and 3-grams in the dataset?
How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
How do you evaluate how many of the words come from foreign languages?
Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?
Introductory Video

Exploratory analysis
Tips and hints

Consider how you are going to perform your basic exploratory and predictive analyses. Keep in mind that the first n rows of the data set may not be representative. You might want to think about how to sample the data using file() and readLines() to obtain a representative sample. 
Think hard about ways you can "compress" the data, what words appear frequently? What combinations of words appear frequently? Later, when you build the R model object, it will need to be small enough to upload to a Shiny server. The more you can figure out how to compress the data the smaller this object will be. 
