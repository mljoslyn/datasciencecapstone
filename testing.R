
require(tm)
require(RWeka)
require(parallel)

# set up parallel processing
cl <- makeCluster(detectCores())
invisible(clusterEvalQ(cl, library(tm)))
invisible(clusterEvalQ(cl, library(RWeka)))
options(mc.cores=1)

# set wd
setwd("/media/mike/Extra Drive 1/GIT/Capstone/")

# load data
con <- file("final/en_US/en_US.blogs.txt", "r")
blogs <- readLines("final/en_US/en_US.blogs.txt", 20000, encoding = "UTF-8", skipNul = TRUE)
close(con)
con <- file("final/en_US/en_US.news.txt", "r")
news <- readLines("final/en_US/en_US.news.txt", 20000, encoding = "UTF-8", skipNul = TRUE)
close(con)
con <- file("final/en_US/en_US.twitter.txt", "r")
twitter <- readLines("final/en_US/en_US.twitter.txt", 20000, encoding = "UTF-8", skipNul = TRUE)
close(con)

alldocs <- paste(blogs, news, twitter)

rm(blogs)
rm(news)
rm(twitter)

corpus <- Corpus(VectorSource(alldocs))
rm(alldocs)

# clean the data
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

# tokenizer
UniGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BiGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TriGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
QuadGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

# document matrices
UniGramTdm <- TermDocumentMatrix(corpus, control = list(tokenize = UniGramTokenizer))


# build data frame
freq.unigram <- slam::row_sums(UniGramTdm)

unigram <- data.frame(row.names = seq(1,length(freq.unigram)), "ngram"=names(freq.unigram), "cnt"=freq.unigram)

save(unigram, file = "unigram.RData")

rm(UniGramTokenizer)
rm(UniGramTdm)
rm(freq.unigram)

BiGramTdm  <- TermDocumentMatrix(corpus, control = list(tokenize = BiGramTokenizer))

freq.bigram <- slam::row_sums(BiGramTdm)

bigram <- data.frame(row.names = seq(1,length(freq.bigram)), "ngram"=names(freq.bigram), "cnt"=freq.bigram)

save(bigram, file = "bigram.RData")

rm(BiGramTokenizer)
rm(BiGramTdm)
rm(freq.bigram)

TriGramTdm <- TermDocumentMatrix(corpus, control = list(tokenize = TriGramTokenizer))

freq.trigram <- slam::row_sums(TriGramTdm)

trigram <- data.frame(row.names = seq(1,length(freq.trigram)), "ngram"=names(freq.trigram), "cnt"=freq.trigram)

save(trigram, file = "trigram.RData")

rm(TriGramTokenizer)
rm(TriGramTdm)
rm(freq.trigram)

QuadGramTdm <- TermDocumentMatrix(corpus, control = list(tokenize = QuadGramTokenizer))

freq.quadgram <- slam::row_sums(QuadGramTdm)

quadgram <- data.frame(row.names = seq(1,length(freq.quadgram)), "ngram"=names(freq.quadgram), "cnt"=freq.quadgram)

save(quadgram, file = "quadgram.RData")

rm(QuadGramTokenizer)
rm(QuadGramTdm)
rm(freq.quadgram)

Ntable <- as.data.frame(table(unigram[["cnt"]]))
names(Ntable) <- c("r", "Nr")

maxR = length(unigram$cnt)

Ntable$r <- as.numeric(levels(Ntable$r))[Ntable$r]
Ntable$Nr <- as.numeric(Ntable$Nr)

totalN = sum(unigram$cnt)

# Get Kneser-Ney probabilities for One-grams
for(n in 1:maxR) {
  unigram$pkn[n] <- unigram$cnt[n] / totalN
}

