## set working path
setwd('/home/Yulong/RESEARCH/QAPTest/')

## to symmetric matrix
SymMat <- function(mat) {
  ## example
  ## tmp1 <- matrix(c(1, '-', '-', 2, 4, '-', 3, 5, 6), ncol = 3, byrow = TRUE)
  mat <- apply(mat, 1:2, as.character)
  mat <- ifelse(mat == '-', 0, mat)
  mat <- apply(mat, 1:2, as.numeric)

  matt <- t(mat)
  w <- which(upper.tri(matt))
  mat[w] <- matt[w]

  return(mat)
}

## read in network
resA <- read.csv('res-a.csv', stringsAsFactors = FALSE, row.names = 1)
resB <- read.csv('res-b.csv', stringsAsFactors = FALSE, row.names = 1)
resC <- read.csv('res-c.csv', stringsAsFactors = FALSE, row.names = 1)
resA <- SymMat(resA)
resB <- SymMat(resB)
resC <- SymMat(resC)

## build graph
g <- array(dim = c(3, 40, 40))
g[1, ,] <- resA
g[2, ,] <- resB
g[3, ,] <- resC

## QAP test
require('statnet')

gcor(g)

## test
AvsB <- qaptest(g, gcor, g1 = 1, g2 = 2)
AvsC <- qaptest(g, gcor, g1 = 1, g2 = 3)
BvsC <- qaptest(g, gcor, g1 = 2, g2 = 3)

par(mfrow=c(1,3))
plot(AvsB)
plot(AvsC)
plot(BvsC)
