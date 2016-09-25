# ONE_GRAM
onegram <- read.csv("onegram.csv") 

Ntable <- as.data.frame(table(onegram[["cnt"]]))

names(Ntable) <- c("r", "Nr")

maxR = length(onegram$cnt)

Ntable$r <- as.numeric(levels(Ntable$r))[Ntable$r]
Ntable$Nr <- as.numeric(Ntable$Nr)

totalN = sum(onegram$cnt)

n1 <- Ntable$Nr[Ntable$r == 1]
n2 <- Ntable$Nr[Ntable$r == 2]
n3 <- Ntable$Nr[Ntable$r == 3]
n4 <- Ntable$Nr[Ntable$r == 4]

d1 <- n1 / (n1 + n2)

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
maxR = length(onegram$cnt)

n1 <- Ntable$Nr[Ntable$r == 1]
n2 <- Ntable$Nr[Ntable$r == 2]
n3 <- Ntable$Nr[Ntable$r == 3]
n4 <- Ntable$Nr[Ntable$r == 4]

d1 <- n1 / (n1 + 2 * n2)

twogram$preface = as.character(twogram$preface)

# Get Kneser-Ney probabilities for two-grams
for(n in 1:maxR) {
  twogram$onecnt[n] <- onegram$cnt[onegram$word == twogram$preface[n]]
  if (twogram$onecnt[n] == 0)
    twogram$onecnt[n] = 3
  
  twogram$a[n] <- max(twogram$cnt[n] - d1,0) / twogram$onecnt[n]
  twogram$b[n] <- d1/twogram$onecnt[n] * twogram$onecnt[n]
  twogram$c[n] <- twogram$onecnt[n] * onegram$pkn[onegram$word == twogram$preface[n]] / totalN
  
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
n3 <- Ntable$Nr[Ntable$r == 3]
n4 <- Ntable$Nr[Ntable$r == 4]

d2 <- n2 / (n2 + 3 * n3)

threegram$preface = as.character(threegram$preface)

# Get Kneser-Ney probabilities for two-grams
for(n in 1:maxR) {
  threegram$onecnt[n] <- twogram$cnt[twogram$X2.gram == threegram$preface[n]]
  if (threegram$twocnt[n] == 0)
    threegram$twocnt[n] = 1
  
  threegram$a[n] <- max(threegram$cnt[n] - d2,0) / threegram$twocnt[n]
  threegram$b[n] <- d2/threegram$twocnt[n] * threegram$twocnt[n]
  threegram$c[n] <- threegram$twocnt[n] * twogram$pkn[twogram$X2.gram == threegram$preface[n]] / totalN
  
  threegram$pkn[n] <- threegram$a[n] + threegram$b[n] * threegram$c[n]
}