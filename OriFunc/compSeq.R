mydata <- readLines("dataset_3_2.txt")
DNAseq <- mydata[1]

compSeq <- ""

for (i in nchar(DNAseq):1) {
  if (substr(DNAseq, i, i) == "A") {
    compSeq <- paste(compSeq, "T", sep="")
  } else if (substr(DNAseq, i, i) == "T") {
    compSeq <- paste(compSeq, "A", sep="")
  } else if (substr(DNAseq, i, i) == "G") {
    compSeq <- paste(compSeq, "C", sep="")
  } else if (substr(DNAseq, i, i) == "C") {
    compSeq <- paste(compSeq, "G", sep="")
  }
}

write(compSeq, file="comp_3_2.txt")
