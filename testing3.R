
# set wd
setwd("/media/mike/Extra Drive 1/GIT/Capstone/")
#setwd("/Users/212359551/datascience/datasciencecapstone")

load(file = "unigrampkn.RData")

load(file = "bigram.RData")

Ntable <- as.data.frame(table(bigram[["cnt"]]))
names(Ntable) <- c("r", "Nr")
Ntable$r <- as.numeric(levels(Ntable$r))[Ntable$r]
Ntable$Nr <- as.numeric(Ntable$Nr)

# Get the relevant counts at 1 and 2 occurrances
n1 <- Ntable$Nr[Ntable$r == 1]
n2 <- Ntable$Nr[Ntable$r == 2]

# Calculate modified Kneser-Ney discount
d <- n1 / (n1 + 2 * n2)
d1 <- 1 - 2 * (d * n2 / n1)

rm(d)
rm(n1)
rm(n2)
rm(Ntable)

# bigram functions
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

#trigram functions
calctrigrampkns <- function(preface) {
  twocnt <- bigram$cnt[bigram$ngram == preface]
  if (twocnt == 0) {
    print("Not found: ", preface, "\n")
  }
  
  # Get the count of the number of other potential
  # words could follow this same preface
  trigrammatches <- trigram[trigram$preface == preface,]
  prefacecnt <- length(trigrammatches$cnt)

  for (n in 1:prefacecnt) {
    # take the first word off the preface and add the word.
    s <- unlist(strsplit(trigrammatches$ngram[n], ' '))
    s <- paste (s[2], s[3], sep = " ")    

    a <- max(trigrammatches$cnt[n] - d1,0) / twocnt
    b <- d1/twocnt * prefacecnt
    c <- bigram$pkn[bigram$ngram == s]
    
    trigrammatches$pkn[n] <- a + b * c
  }  
  
  bigrammatches
}

calctrigrampkn <- function(ngram) {
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