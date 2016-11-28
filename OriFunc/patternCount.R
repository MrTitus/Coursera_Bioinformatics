##Set to proper working directory in Coursera Bioinformatics
setwd("C:/Users/Brandon/Desktop/Coursera Bioinformatics")

#Function to count the number of times a pattern appears
#in text
patternCount <- function(text, pattern, start, position) {
  count <- 0
  
  if (missing(position)) {
    position <- FALSE
  }
  
  if (missing(start)) {
    start <- 1
  }
  
  positions <- ""
  
  ##Loop that counts the how many times a pattern appears in
  ##a specific DNA string
  for (i in start:nchar(text)-nchar(pattern)+1) {
    if (substr(text, i, i+nchar(pattern)-1)==pattern) {
      count <- count + 1
      if (position == TRUE) {
        positions <- paste(positions, toString(i-1))
      }
    }
  }
  
  write(positions, "positions.txt")
  return(count)
}

mydata <- readLines("vibrioDNA.txt")
pattern <- "CTTGATCAT"
text <- mydata[1]
patternCount(text, pattern, position=TRUE)


