### Load Libraries
library(quanteda)
library(dplyr)
library(stringr)
library(tibble)

### load files
news = readLines("/home/fabio/MEGA/CURSOS_ONLINE/datasciencespecialization/capstone-project/data/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
blogs = readLines("/home/fabio/MEGA/CURSOS_ONLINE/datasciencespecialization/capstone-project/data/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
twitter = readLines("/home/fabio/MEGA/CURSOS_ONLINE/datasciencespecialization/capstone-project/data/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

### Create a sample file
set.seed(1020)
sample = c(sample(news, length(news) * .010),
           sample(blogs, length(blogs) * .010),
           sample(twitter, length(twitter) * .010))

### Remove special char"
sample = gsub("_", " ", sample)

### create a corpus
my_corpus = corpus(sample)
summary(my_corpus)

### Clean and organize data
tokens = my_corpus
tokens = tokens(tokens, what = "word",
                remove_numbers = TRUE, remove_punct = TRUE,
                remove_symbols = TRUE, remove_hyphens = TRUE,
                remove_twitter = TRUE, remove_url = TRUE, 
                remove_separators = TRUE)

#tokens = tokens_select(tokens, stopwords("english"),
#                       selection = "remove")

### Remove specific terms in Twitter
tokens = tokens_select(tokens, c("lol", "rt", "_"), selection = "remove", padding = FALSE)

### Ngram1 and frequencies
token1_dfm = dfm(tokens, tolower = TRUE, ngrams = 1)
token1_freq = textstat_frequency(token1_dfm)

### Ngram2
token2_dfm = dfm(tokens, tolower = TRUE, ngrams = 2)
token2_freq = textstat_frequency(token2_dfm)

### Ngram3
token3_dfm = dfm(tokens, tolower = TRUE, ngrams = 3)
token3_freq = textstat_frequency(token3_dfm)

### Ngram4
token4_dfm = dfm(tokens, tolower = TRUE, ngrams = 4)
token4_freq = textstat_frequency(token4_dfm)

### Ngram5
token5_dfm = dfm(tokens, tolower = TRUE, ngrams = 5)
token5_freq = textstat_frequency(token5_dfm)

### Ngram6
token6_dfm = dfm(tokens, tolower = TRUE, ngrams = 6)
token6_freq = textstat_frequency(token6_dfm)

### Remove unnecessary variables
rm(token1_dfm)
rm(token2_dfm)
rm(token3_dfm)
rm(token4_dfm)
rm(token5_dfm)
rm(token6_dfm)

### Function to prepare data.frame
cleanFun = function(df) {
      df = as.data.frame(df)
      df = df[,-c(3:5)]
      df$prob = df$frequency/sum(df$frequency)
      df = df[,-2]
      if (str_count(df$feature, pattern = "_") == 1) {
            b = structure(data.frame(
                  matrix(unlist(strsplit(df$feature,"_")),length(df$feature),2,T)),
                  names=c("A","B"))
            df = cbind(df, b)
            df = df[c("A", "prob", "B")]
      }
      else if (str_count(df$feature, pattern = "_") == 2) {
            c = structure(data.frame(
                  matrix(unlist(strsplit(df$feature,"_")),length(df$feature),3,T)),
                  names=c("A","B","C"))
            df = cbind(df, c)
            df$A = paste(df$A, df$B, sep = " ")
            df = df[c("A", "prob", "C")]
      }
      else if (str_count(df$feature, pattern = "_") == 3) {
            d = structure(data.frame(
                  matrix(unlist(strsplit(df$feature,"_")),length(df$feature),4,T)),
                  names=c("A","B","C", "D"))
            df = cbind(df, d)
            df$A = paste(df$A, df$B, df$C, sep = " ")
            df = df[c("A", "prob", "D")]
      }
      else if (str_count(df$feature, pattern = "_") == 4) {
            e = structure(data.frame(
                  matrix(unlist(strsplit(df$feature,"_")),length(df$feature),5,T)),
                  names=c("A","B","C","D","E"))
            df = cbind(df, e)
            df$A = paste(df$A, df$B, df$C, df$D, sep = " ")
            df = df[c("A", "prob", "E")]
      }
      else if (str_count(df$feature, pattern = "_") == 5) {
            f = structure(data.frame(
                  matrix(unlist(strsplit(df$feature,"_")),length(df$feature),6,T)),
                  names=c("A","B","C","D","E","F"))
            df = cbind(df, f)
            df$A = paste(df$A, df$B, df$C, df$D, df$E, sep = " ")
            df = df[c("A", "prob", "F")]
      }
}

### Adjust data.frames
token2_freq = cleanFun(token2_freq)
token3_freq = cleanFun(token3_freq)
token4_freq = cleanFun(token4_freq)
token5_freq = cleanFun(token5_freq)
token6_freq = cleanFun(token6_freq)

### Export Ngram data.frames frequencies
save(token1_freq, file = "./week_3/freq/freq1.RData")
save(token2_freq, file = "./week_3/freq/freq2.RData")
save(token3_freq, file = "./week_3/freq/freq3.RData")
save(token4_freq, file = "./week_3/freq/freq4.RData")
save(token5_freq, file = "./week_3/freq/freq5.RData")
save(token6_freq, file = "./week_3/freq/freq6.RData")

### Exercises

#### Exercise 1
nr1 = "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"

#### Exercise 2
nr2 = "You're the reason why I smile everyday. Can you follow me please? It would mean the"

#### Exercise 3
nr3 = "Hey sunshine, can you follow me and make me the"

#### Exercise 4
nr4 = "Very early observations on the Bills game: Offense still struggling but the"

#### Exercise 5
nr5 = "Go on a romantic date at the"

#### Exercise 6
nr6 = "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"

#### Exercise 7
nr7 = "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"

#### Exercise 8
nr8 = "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"

#### Exercise 9 
nr9 = "Be grateful for the good times and keep the faith during the"

#### Exercise 10
nr10 = "If this isn't the cutest thing you've ever seen, then you must be"

listexercises = c(nr1, nr2, nr3, nr4, nr5, nr6, nr7, nr8, nr9, nr10)

for (i in listexercises) {
      x = predict_Backoff(i, nGramModelsList)
      print(x)
}

