### Ver arquivo online
url = "https://rpubs.com/marciogualtieri/swiftrkey"

### Function Extract History

extract_history <- function(ngram){
      ifelse(length(ngram) > 1,
             paste(ngram[1:(length(ngram)-1)], collapse = " "),
             ""
      )
}

### Function Extract word

extract_word <- function(ngram) paste(ngram[length(ngram)], collapse = " ")

### Build Processes frequencies - FRC
build_frc = function(data) {
      data = as.data.frame(data)
      data$ngram_length = sapply(data$word, length)
      data$history = sapply(data$word, extract_history)
      data$word_2 = sapply(data$word, extract_word)
      data.frame(data)
}

### Loop to convert as data.frame and execute FRC funcion
token1_freq = build_frc(token1_freq)

frequencies_of_frequencies <- table(token1_freq$freq)

findAssocs(token1_dfm, c("bacon", "beer", "cheese"))

