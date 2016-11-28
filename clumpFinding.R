clumpFinding <- function(DNAseq, k, t, L) {
  
  clump <- array(dim = c(4^k))
  matchingPatterns <- array(dim = c(4^k))
  
  for (i in 1:(4^k)) {
    clump[i] <- 0
  }
  
  segment <- substr(DNAseq, 1, L)
  FrequencyArray <- computingFrequencies(segment, k)
 
  for (i in 1:(4^k)) {
    if (FrequencyArray[i] >= t) {
      clump[i] <- 1
    }
  }
  
  for (i in 2:(nchar(DNAseq)-L)) {
    firstPattern <- substr(DNAseq, i-1, i+k-2)
    index <- patternToNumber(firstPattern)
    FrequencyArray[index+1] <- FrequencyArray[index+1]-1
    lastPattern <- substr(DNAseq, i+L-k+1, i+L)
    index <- patternToNumber(lastPattern)
    FrequencyArray[index+1] <- FrequencyArray[index+1]+1
    
    if (FrequencyArray[index+1] >= t) {
      clump[index+1] <- 1
    }
  }
  
  count <- 1
  
  for (i in 1:(4^k)) {
    if (clump[i] == 1) {
      pattern <- numberToPattern(i-1, k)
      matchingPatterns[count] <- pattern
      count <- count+1
    }
  }
  
  write(count, file = "clump.txt")
}

mydata <- readLines("dna.txt")

DNAseq <- mydata[1]
k <- 9
L <- 500
t <- 3

clumpFinding(DNAseq, k, t, L)


