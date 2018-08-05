### Load Libraries
library(quanteda)
library(ANLP)

### load files
news <- readLines("/home/fabio/MEGA/CURSOS_ONLINE/datasciencespecialization/capstone-project/data/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
blogs <- readLines("/home/fabio/MEGA/CURSOS_ONLINE/datasciencespecialization/capstone-project/data/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines("/home/fabio/MEGA/CURSOS_ONLINE/datasciencespecialization/capstone-project/data/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

### Create a sample file
set.seed(5150)
sample = c(sample(news, length(news) * .005),
           sample(blogs, length(blogs) * .005),
           sample(twitter, length(twitter) * .005))

### create a corpus
my_corpus = corpus(sample)
summary(my_corpus)

### Clean and organize data
tokens = my_corpus
tokens = tokens(tokens, what = "word",
                remove_numbers = TRUE, remove_punct = TRUE,
                remove_symbols = TRUE, remove_hyphens = TRUE,
                remove_twitter = TRUE, remove_url = TRUE)

tokens = tokens_select(tokens, stopwords("english"),
                       selection = "remove")

### Remove specific terms in Twitter
tokens = tokens_select(tokens, c("lol", "rt"), selection = "remove", padding = FALSE)

### Ngram1 and frequencies
token1_dfm = dfm(tokens, tolower = TRUE, ngrams = 1)
token1_freq = textstat_frequency(token1_dfm)
save(token1_freq, file = "./week_3/freq/freq1.RData")

### Ngram2
token2_dfm = dfm(tokens, tolower = TRUE, ngrams = 2)
token2_freq = textstat_frequency(token2_dfm)
save(token2_freq, file = "./week_3/freq/freq2.RData")

### Ngram3
token3_dfm = dfm(tokens, tolower = TRUE, ngrams = 3)
token3_freq = textstat_frequency(token3_dfm)
save(token3_freq, file = "./week_3/freq/freq3.RData")

### Ngram4
token4_dfm = dfm(tokens, tolower = TRUE, ngrams = 4)
token4_freq = textstat_frequency(token4_dfm)
save(token4_freq, file = "./week_3/freq/freq4.RData")

### Ngram5
token5_dfm = dfm(tokens, tolower = TRUE, ngrams = 5)
token5_freq = textstat_frequency(token5_dfm)
save(token5_freq, file = "./week_3/freq/freq5.RData")

### Ngram6
token6_dfm = dfm(tokens, tolower = TRUE, ngrams = 6)
token6_freq = textstat_frequency(token6_dfm)
save(token6_freq, file = "./week_3/freq/freq6.RData")

### Prepare tokens frequencies data.frame to ANLP Predict Model Backoff
listtokens = c(token1_freq, token2_freq,token3_freq,token4_freq,token5_freq,token6_freq)
names = c("word", "freq")
token_prep = function(data) {
      data = data[ ,-c(3:5)]
      `names<-`(data, names)
}

token1_freq = token_prep(token1_freq)
token2_freq = token_prep(token2_freq)
token3_freq = token_prep(token3_freq)
token4_freq = token_prep(token4_freq)
token5_freq = token_prep(token5_freq)
token6_freq = token_prep(token6_freq)

### NGram Model List

nGramModelsList = list(token1_freq, token2_freq,token3_freq,token4_freq,token5_freq,token6_freq)
test = list(token1_freq, token2_freq)

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

