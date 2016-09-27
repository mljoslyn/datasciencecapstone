if (getwd() == "C:/Users/212359551/Documents")
  setwd("C:/Users/212359551/datascience/datasciencecapstone")

# ONE_GRAM
onegram <- read.csv("onegram.csv") 

Ntable <- as.data.frame(table(onegram[["cnt"]]))

names(Ntable) <- c("r", "Nr")

maxR = length(onegram$cnt)

Ntable$r <- as.numeric(levels(Ntable$r))[Ntable$r]
Ntable$Nr <- as.numeric(Ntable$Nr)

totalN = sum(onegram$cnt)

# Get Kneser-Ney probabilities for One-grams
for(n in 1:maxR) {
  onegram$pkn[n] <- onegram$cnt[n] / totalN
}

# TWO-GRAM
twogram <- read.csv("twogram.csv") 

Ntable <- as.data.frame(table(twogram[["cnt"]]))
names(Ntable) <- c("r", "Nr")
Ntable$r <- as.numeric(levels(Ntable$r))[Ntable$r]
Ntable$Nr <- as.numeric(Ntable$Nr)

totalN = sum(twogram$cnt)
maxR = length(twogram$cnt)

# Get the relevant counts at 1 and 2 occurrances
n1 <- Ntable$Nr[Ntable$r == 1]
n2 <- Ntable$Nr[Ntable$r == 2]

# Calculate modified Kneser-Ney discount
d <- n1 / (n1 + 2 * n2)
d1 <- 1 - 2 * (d * n2 / n1)

twogram$preface = as.character(twogram$preface)
twogram$word = as.character(twogram$word)

# Get Kneser-Ney probabilities for two-grams
for(n in 1:maxR) {
  twogram$onecnt[n] <- onegram$cnt[onegram$word == twogram$preface[n]]
  if (twogram$onecnt[n] == 0)
    twogram$onecnt[n] = 3
  
  twogram$complete[n] <- length(twogram$cnt[twogram$preface == twogram$preface[n]])
  
  twogram$a[n] <- max(twogram$cnt[n] - d1,0) / twogram$onecnt[n]
  twogram$b[n] <- d1/twogram$onecnt[n] * twogram$complete[n]
  twogram$c[n] <- onegram$pkn[onegram$word == twogram$word[n]]
  
  twogram$pkn[n] <- twogram$a[n] + twogram$b[n] * twogram$c[n]
}

# THREE-GRAM
threegram <- read.csv("threegram.csv") 

Ntable <- as.data.frame(table(threegram[["cnt"]]))
names(Ntable) <- c("r", "Nr")
Ntable$r <- as.numeric(levels(Ntable$r))[Ntable$r]
Ntable$Nr <- as.numeric(Ntable$Nr)

totalN = sum(threegram$cnt)
maxR = length(threegram$cnt)

n1 <- Ntable$Nr[Ntable$r == 1]
n2 <- Ntable$Nr[Ntable$r == 2]
#n3 <- Ntable$Nr[Ntable$r == 3]
#n4 <- Ntable$Nr[Ntable$r == 4]

# Calculate modified Kneser-Ney discount
d <- n1 / (n1 + 2 * n2)
d2 <- d  # Need to adjust this

threegram$preface = as.character(threegram$preface)
threegram$ngram = as.character(threegram$ngram)

# Get Kneser-Ney probabilities for three-grams
for(n in 1:maxR) {
  threegram$twocnt[n] <- twogram$cnt[twogram$ngram == threegram$preface[n]]
  if (threegram$twocnt[n] == 0)
    threegram$twocnt[n] = 1
  
  threegram$complete[n] <- length(threegram$cnt[threegram$preface == threegram$preface[n]])
  
  # take the first word off the preface and add the word.
  s <- unlist(strsplit(threegram$ngram[n], ' '))
  threegram$s[n] <- paste (s[2], s[3], sep = " ")
  
  threegram$a[n] <- max(threegram$cnt[n] - d2,0) / threegram$twocnt[n]
  threegram$b[n] <- d2/threegram$twocnt[n] * threegram$complete[n]
  threegram$c[n] <- twogram$pkn[twogram$ngram == threegram$s[n]]
  
  threegram$pkn[n] <- threegram$a[n] + threegram$b[n] * threegram$c[n]
}

df <- threegram[threegram$preface == "is the",c("word", "ngram", "pkn")]
df[order(-df$pkn),]

# FOUR-GRAM
fourgram <- read.csv("fourgram.csv") 

Ntable <- as.data.frame(table(fourgram[["cnt"]]))
names(Ntable) <- c("r", "Nr")
Ntable$r <- as.numeric(levels(Ntable$r))[Ntable$r]
Ntable$Nr <- as.numeric(Ntable$Nr)

totalN = sum(fourgram$cnt)
maxR = length(fourgram$cnt)

n1 <- Ntable$Nr[Ntable$r == 1]
n2 <- Ntable$Nr[Ntable$r == 2]
#n3 <- Ntable$Nr[Ntable$r == 3]
#n4 <- Ntable$Nr[Ntable$r == 4]

# Calculate modified Kneser-Ney discount
d <- n1 / (n1 + 2 * n2)
d3 <- d  # Need to adjust this

fourgram$preface = as.character(fourgram$preface)
fourgram$ngram = as.character(fourgram$ngram)

# Get Kneser-Ney probabilities for four-grams
for(n in 1:maxR) {
  fourgram$threecnt[n] <- threegram$cnt[threegram$ngram == fourgram$preface[n]]
#  if (fourgram$threecnt[n] == 0)
#    fourgram$threecnt[n] = 1
  
  fourgram$complete[n] <- length(fourgram$cnt[fourgram$preface == fourgram$preface[n]])
  
  # take the first word off the preface and add the word.
  s <- unlist(strsplit(fourgram$ngram[n], ' '))
  fourgram$s[n] <- paste (s[2], s[3], s[4], sep = " ")
  
  fourgram$a[n] <- max(fourgram$cnt[n] - d3,0) / fourgram$twocnt[n]
  fourgram$b[n] <- d3/fourgram$threecnt[n] * fourgram$complete[n]
  fourgram$c[n] <- threegram$pkn[threegram$ngram == fourgram$s[n]]
  
  fourgram$pkn[n] <- fourgram$a[n] + fourgram$b[n] * fourgram$c[n]
}