computingFrequencies <- function(DNAseq, k) {
  FrequencyArray <- array(dim = c(4^k))
  
  for (i in 1:(4^k)) {
    FrequencyArray[i] <- 0
  }
  
  for (i in 1:(nchar(DNAseq)-k+1)) {
    pattern <- substr(DNAseq, i, i+k-1)
    j <- patternToNumber(pattern)
    FrequencyArray[j+1] <- FrequencyArray[j+1] + 1
    
    if (FrequencyArray[j+1] > maxValue) {
      maxValue <- FrequencyArray[j+1]
    }
  }
  
  write(FrequencyArray, file = "frequencyarray.txt")
  return(FrequencyArray)
}

getwd()
setwd("C:/Users/Brandon/Desktop/Coursera\ Bioinformatics")
mydata <- readLines("dataset_2994_5.txt")

DNAseq <- mydata[1]
k <- strtoi(mydata[2])
FrequencyArray <- computingFrequencies(DNAseq, k)


