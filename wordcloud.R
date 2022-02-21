library(tm) # untuk text mining
library(SnowballC) # untuk stemming : mengubah kata menjadi kata dasar
library(wordcloud) # untuk wordcloud
library(RColorBrewer) # untuk warna warni

# baca file dari html
filePath <- "http://www.textise.net/showText.aspx?strURL=https%253A//www.wartaekonomi.co.id/read253712/sumpah-pemuda-harus-menjadi-tonggak-baru-hadapi-tantangan-global.html"
text <- readLines(filePath)
class(text) #melihat tipe data
text[500:600]

# baca file dari notepad
text <- readLines(file.choose())
class(text)
text

# menjadikan corpus. Corpus : kumpulan semuanya
docs <- Corpus(VectorSource(text))
class(docs)

# Inspect : menampilkan corpus
inspect(docs)

# menghilangkan tanda baca gak penting
toSpace <- content_transformer(function (x,pattern) gsub(pattern," ",x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "(")
docs <- tm_map(docs, toSpace, ")")
inspect(docs)

# mengubah kapital ke kecil
docs <- tm_map(docs, content_transformer(tolower))

# menghilangkan angka
docs <- tm_map(docs, removeNumbers)

# menghilangkan kata inggris (in on at) // defaut dalam bahasa inggris
docs <- tm_map(docs, removeWords, stopwords("english"))

# menghilangkan kata terserah aku
docs <- tm_map(docs, removeWords, c("itu", "ini", "yang", "dan"))

# menghilangkan tanda baca
docs <- tm_map(docs, removePunctuation)

# membuat stemming : menjadikan kata dasar
docs <- tm_map(docs, stemDocument)

inspect(docs)

dtm <- TermDocumentMatrix((docs))
m <- as.matrix(dtm)
v <-sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq=v)
head(d, 10)

# membuat wordcloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq,
          min.freq = 1, max.words = 200,
          random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))
