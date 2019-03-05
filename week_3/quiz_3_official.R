# Load Libraries
library(stringr)
library(stringi)
library(tm)

### load files
allNews = readLines("/home/fabio/MEGA/CURSOS_ONLINE/datasciencespecialization/capstone-project/data/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
allBlogs = readLines("/home/fabio/MEGA/CURSOS_ONLINE/datasciencespecialization/capstone-project/data/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
allTwitter = readLines("/home/fabio/MEGA/CURSOS_ONLINE/datasciencespecialization/capstone-project/data/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

### Create Functions
tokenmaker = function(x) {
      corpus = Corpus(VectorSource(x))
      corpus = tm_map(corpus, content_transformer(tolower))
      corpus = tm_map(corpus, removePunctuation)
      corpus = tm_map(corpus, stripWhitespace)
      corpus = tm_map(corpus, removeWords, stopwords("english"))
      corpus = tm_map(corpus, removeNumbers)
      corpus = tm_map(corpus, PlainTextDocument)
      corpus = Corpus(VectorSource(corpus))
}  

wordcounter = function(x) {
      dtm=DocumentTermMatrix(x)
      dtm_matrix = as.matrix(dtm)
      word_freq = colSums(dtm_matrix)
      word_freq = sort(word_freq, decreasing = TRUE)
      words = names(word_freq)
      return(list(words, word_freq))
}  

NextWordIs = function(x,y){
      BQuest=grepl(x, allBlogs, ignore.case=TRUE)
      BDocs=allBlogs[BQuest]
      textoachado='a'
      NextWordIs='a'
      i=length(BDocs)
      if (i>0)
      {
            for (i in 1:i)
            {  textoachado[i]= str_extract(BDocs[i], y)
            NextWordIs[i]= stri_extract_last_words(textoachado[i]) 
            }
      }
      NQuest=grepl(x, allNews, ignore.case=TRUE)
      NDocs=allNews[NQuest]
      j=length(NDocs)
      if (j>0)
      {
            for (j in 1:j)
            {  textoachado[i+j]= str_extract(NDocs[j], y)
            NextWordIs[i+j]= stri_extract_last_words(textoachado[i+j]) 
            }
      }
      TQuest=grepl(x, allTwitter, ignore.case=TRUE)
      TDocs=allTwitter[TQuest]
      k=length(TDocs)
      if (k>0)
      {
            for (k in 1:k)
            {  textoachado[i+j+k]= str_extract(TDocs[k], y)
            NextWordIs[i+j+k]= stri_extract_last_words(textoachado[i+j+k]) 
            }
      }
      bundle=as.data.frame(NextWordIs, stringsAsFactors=FALSE)
      summary (bundle)
      blogs_token = tokenmaker(bundle)
      blogs_words = wordcounter(blogs_token)
      summary(nchar(bundle))
      head(bundle)
      tdm_Blogs=TermDocumentMatrix(blogs_token)
      m_Blogs=as.matrix(tdm_Blogs)
      v_Blogs=sort(rowSums(m_Blogs),decreasing=TRUE)
      d_Blogs=data.frame(word=names(v_Blogs),freq=v_Blogs)
      head(v_Blogs, 100)    
      return(list(head(v_Blogs,100)))
}

resultado_01<-NextWordIs("a case of ", "([Aa]+ +[Cc]ase+ +[Oo]f+ +[^ ]+ )" )
resultado_01[[1]]


