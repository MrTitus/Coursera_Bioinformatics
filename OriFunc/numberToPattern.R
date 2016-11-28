numberToPattern <- function(number, k) {
  pos <- k-1
  pattern <- ""
  
  for (i in 1:k) {
    if (((4^pos)*3) <= number) {
      pattern <- paste(pattern, "T", sep="")
      number <- number - ((4^pos)*3)
    } else if (((4^pos)*2) <= number) {
      pattern <- paste(pattern, "G", sep="")
      number <- number - ((4^pos)*2)
    } else if (((4^pos)*1) <= number) {
      pattern <- paste(pattern, "C", sep="")
      number <- number - ((4^pos)*1)
    } else {
      pattern <- paste(pattern, "A", sep="")
    }
    
    pos <- pos - 1
  }
  
  return(pattern)
}

numberToPattern(5532, 10)
