patternToNumber <- function(DNAseq) {
  pos <- nchar(DNAseq)-1
  number <- 0 
  
  for (i in 1:nchar(DNAseq)) {
    
    if (substr(DNAseq, i, i) == "A") {
      number <- number + ((4^pos)*0)
    } else if (substr(DNAseq, i, i) == "C") {
      number <- number + ((4^pos)*1)
    } else if (substr(DNAseq, i, i) == "G") {
      number <- number + ((4^pos)*2)
    } else if (substr(DNAseq, i, i) == "T") {
      number <- number + ((4^pos)*3)
    }
    
    pos <- pos - 1
    
  }
  
  return(number)
}

patternToNumber("TGATATTTCGCCAAGTCAAT")

