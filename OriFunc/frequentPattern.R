mostFrequentPatterns <- function(DNAseq, k) {
  FrequencyArray <- computingFrequencies(DNAseq, k)
  maxValue <- max(FrequencyArray)

  ###Find frequency of all possible patterns of k-char
  for (i in 1:(4^k)) {
    if (FrequencyArray[i] == maxValue) {
      pattern <- numberToPattern(i-1, k)
      print(pattern)
    }
  }
}
  

mydata <- readLines("dataset_2994_5.txt")
DNAseq <- mydata[1]
k <- strtoi(mydata[2])

mostFrequentPatterns(DNAseq, k)



