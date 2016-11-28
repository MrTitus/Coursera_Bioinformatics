mostFrequentPatternsBySorting <- function(DNAseq, k) {
  index <- array(dim = c(nchar(DNAseq)-k))
  count <- array(dim = c(nchar(DNAseq)-k))
  
  for (i in 1:(nchar(DNAseq)-k+1)) {
    pattern <- substr(DNAseq, i, i+k-1)
    index[i] <- patternToNumber(pattern)
    count[i] <- 1
  }
  
  sortedIndex <- sort(index)
  
  for (i in 2:(nchar(DNAseq)-k+1)) {
    if (sortedIndex[i] == sortedIndex[i-1]) {
      count[i] <- count[i-1] + 1
    }
  }
  
  maxValue <- max(count)
  
  for (i in 1:(nchar(DNAseq)-k+1)) {
    if (count[i] == maxValue) {
      pattern <- numberToPattern(sortedIndex[i], k)
      print(pattern)
    }
  }
}

mydata <- readLines("dna.txt")
DNAseq <- mydata[1]
k <- 9

mostFrequentPatternsBySorting(DNAseq, k)
