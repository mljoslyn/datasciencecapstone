

s <- "done done done done done the bad cat the cat the the the is is the the the a a a a a in town boy girl head tail foot bag ball"

# Split into words and make a frequency table
v <- strsplit(s, ' ')
table(v)

# Convert table into data frame
d <- as.data.frame(table(v))

Ntable <- as.data.frame(table(d[["Freq"]]))

names(Ntable) <- c("r", "Nr")
Ntable$Zr = 0.0
Ntable$estimate = 0.0

maxR = length(Ntable$r)

Ntable$r <- as.numeric(levels(Ntable$r))[Ntable$r]
Ntable$Nr <- as.numeric(Ntable$Nr)

totalN = sum(Ntable$Nr*Ntable$r)

for(n in maxR:1) {
  q = 0
  if (n > 1)
    q = Ntable$r[n - 1]
  t = 2 * Ntable$r[n] - q
  if (n < maxR)
    t = Ntable$r[n + 1]
  
  Ntable$Zr[n] = Ntable$Nr[n] / (0.5 * (t - q))
  
  cat ("n", n, ", t", t, ", q", q)
  cat (" Ntable$r[n]", Ntable$r[n], "\n")
  
  if (n < maxR) {
    cat("Ntable$Zr[n+1]", Ntable$Zr[n+1], "\n")
    Ntable$estimate[n] = (Ntable$r[n] + 1) * (Ntable$Zr[n+1] / Ntable$Zr[n]) / totalN
  }
  else {
    Ntable$estimate[n] = ((Ntable$r[n] + 1) * (Ntable$Nr[n]/totalN) / Ntable$Zr[n]) / totalN
  }
}
