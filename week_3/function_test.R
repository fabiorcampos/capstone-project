truncateText <- function(text, wordCount = 3) {
      truncText <- paste(tail(unlist(strsplit(text, " ")),
                              wordCount),
                         collapse = " ")
      return(truncText)
}


predictWord = function(text) {
      if(nchar(text) == 0)
            return("")
      
      wordCount = function(text) {
            length(strsplit(text, ' ')[[1]])
      predWord = ""
      }
      
      if (wordCount(text) == 1) {
            sentence = truncateText(text, 1)
            predWord = token2_freq$B[token2_freq$A == sentence]
      }
      return(predWord)
}


testFun = function(input) {
      sentence = input
      if  {
            predword = token2_freq$B[token2_freq$A == sentence]
            }
      return(predword)
}

