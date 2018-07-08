library(stringi)
library(tm)
library(ngram)
library(ggplot2)
library(RWeka)
library(SnowballC)

### load files
news <- readLines("/home/fabio/MEGA/CURSOS_ONLINE/datasciencespecialization/capstone-project/data/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
blogs <- readLines("/home/fabio/MEGA/CURSOS_ONLINE/datasciencespecialization/capstone-project/data/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines("/home/fabio/MEGA/CURSOS_ONLINE/datasciencespecialization/capstone-project/data/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

### Information of files

#### Size in MB
nsize <- file.info("/home/fabio/MEGA/CURSOS_ONLINE/datasciencespecialization/capstone-project/data/en_US/en_US.news.txt")$size / 1024 ^ 2
bsize <- file.info("/home/fabio/MEGA/CURSOS_ONLINE/datasciencespecialization/capstone-project/data/en_US/en_US.blogs.txt")$size / 1024 ^ 2
tsize <- file.info("/home/fabio/MEGA/CURSOS_ONLINE/datasciencespecialization/capstone-project/data/en_US/en_US.twitter.txt")$size / 1024 ^ 2
totalsize <- nsize + bsize + tsize

#### Length Lines 
nlines <- length(news)
blines <- length(blogs)
tlines <- length(twitter)
totallength <- nlines + blines + tlines

#### Longest line in each file
nmax <- max(stri_count_words(news))
bmax <- max(stri_count_words(blogs))
tmax <- max(stri_count_words(twitter))
totalmax <- nmax + bmax + tmax

### length(grep("love", twitter)) / length(grep("hate", twitter))

#### Mean words in each file
nmean <- mean(stri_count_words(news))
bmean <- mean(stri_count_words(blogs))
tmean <- mean(stri_count_words(twitter))
meantotal <- nmean + bmean + tmean / 3

names <- c("news", "blogs", "twitter")

dfsummary <- data.frame(sizeinmb = c(nsize, bsize, tsize),
                        lengthlines = c(nlines, blines, tlines),
                        longestline = c(nmax, bmax, tmax),
                        meanwords = c(nmean, bmean, tmean),
                        total = c(totalsize, totallength, totalmax), row.names = names)
print(dfsummary)

### Create a sample file
set.seed(5150)
sample <- c(sample(news, length(news) * .005),
            sample(blogs, length(blogs) * .005),
            sample(twitter, length(twitter) * .005))


### Clean and organize data
sample <- iconv(sample, 'UTF-8', 'ASCII')
mycorpus <- VCorpus(VectorSource(sample))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
mycorpus <- tm_map(mycorpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
mycorpus <- tm_map(mycorpus, toSpace, "@[^\\s]+")
mycorpus <- tm_map(mycorpus, tolower)
mycorpus <- tm_map(mycorpus, function(x) iconv(enc2utf8(x), sub = "byte"))
mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))
mycorpus <- tm_map(mycorpus, stemDocument)
mycorpus <- tm_map(mycorpus, removeNumbers)
mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpus <- tm_map(mycorpus, PlainTextDocument)


### Create function to n-grams
getFreq <- function(tdm) {
      freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
      return(data.frame(word = names(freq), freq = freq))
}
bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
makePlot <- function(data, label) {
      ggplot(data[1:30,], aes(reorder(word, -freq), freq)) +
            labs(x = label, y = "Frequency") +
            theme(axis.text.x = element_text(angle = 60, size = 12, hjust = 1)) +
            geom_bar(stat = "identity", fill = I("red"))
}

freq1 <- getFreq(removeSparseTerms(TermDocumentMatrix(mycorpus), 0.9999))
freq2 <- getFreq(removeSparseTerms(TermDocumentMatrix(mycorpus, control = list(tokenize = bigram)), 0.9999))
freq3 <- getFreq(removeSparseTerms(TermDocumentMatrix(mycorpus, control = list(tokenize = trigram)), 0.9999))

makePlot(freq1, "30 Most Common Unigrams")
makePlot(freq2, "30 Most Common Bigrams")
makePlot(freq3, "30 Most Common Trigrams")

