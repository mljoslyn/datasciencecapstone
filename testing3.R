
# set wd
setwd("/media/mike/Extra Drive 1/GIT/Capstone/")

load(file = "unigrampkn.RData")

load(file = "bigram.RData")

Ntable <- as.data.frame(table(bigram[["cnt"]]))
names(Ntable) <- c("r", "Nr")
Ntable$r <- as.numeric(levels(Ntable$r))[Ntable$r]
Ntable$Nr <- as.numeric(Ntable$Nr)

totalN = sum(bigram$cnt)
maxR = length(bigram$cnt)

# Get the relevant counts at 1 and 2 occurrances
n1 <- Ntable$Nr[Ntable$r == 1]
n2 <- Ntable$Nr[Ntable$r == 2]

# Calculate modified Kneser-Ney discount
d <- n1 / (n1 + 2 * n2)
d1 <- 1 - 2 * (d * n2 / n1)


calcbigrampkns <- function(preface) {
  onecnt <- unigram$cnt[unigram$ngram == preface]
  if (onecnt == 0) {
    print("Not found: ", preface, "\n")
  }
  
  bigrammatches <- bigram[bigram$preface == preface,]
  
  prefacecnt <- length(bigrammatches$cnt)
  
  for (n in 1:prefacecnt) {
    a <- max(bigrammatches$cnt[n] - d1,0) / onecnt
    b <- d1/onecnt * prefacecnt
    c <- unigram$pkn[unigram$ngram == bigrammatches$word[n]]
  
    bigrammatches$pkn[n] <- a + b * c
  }  
  
  bigrammatches
}

calcbigrampkn <- function(ngram) {
  x <- bigram[bigram$ngram == ngram,]
  onecnt <- unigram$cnt[unigram$ngram == x$preface[1]]
  if (onecnt == 0) {
    print("Not found: ", x$preface[1], "\n")
  }
  
  prefacecnt <- length(bigram$cnt[bigram$preface == x$preface[1]])
  
  a <- max(x$cnt[1] - d1,0) / onecnt
  b <- d1/onecnt * prefacecnt
  c <- unigram$pkn[unigram$ngram == x$word[1]]
    
  pkn <- a + b * c
  
  pkn
}






# Get the second word
getsecondword <- function(word) {
  x <- calcbigrampkns(word)
  answer <- x[order(-x$pkn),]$word[1]
  answer
}

getsecondword("bible")